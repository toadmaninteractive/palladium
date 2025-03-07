import {
    AfterViewInit,
    ChangeDetectorRef,
    Component,
    ElementRef,
    HostListener,
    Input,
    OnDestroy,
    OnInit,
    ViewChild
} from '@angular/core';
import { BehaviorSubject, combineLatest, Subject } from 'rxjs';
import * as d3 from 'd3';
import { debounceTime, filter, takeUntil } from 'rxjs/operators';
import * as VisualizationProtocol from '../../../protocol/visualization-protocol';
import * as DataProtocol from '../../../protocol/data-protocol';
import { ChartBoxExpansionService } from "../../../core/services/chart-box-expansion.service";

@Component({
    selector: 'm-d3-box-plot',
    templateUrl: './d3-box-plot.component.html',
    styleUrls: ['./d3-box-plot.component.scss'],
})

export class D3BoxPlotComponent implements OnInit, AfterViewInit, OnDestroy {
    @ViewChild('chartContainer', {static: true}) private chartContainer: ElementRef;
    @ViewChild('plotContainer', {static: true}) private plotContainer: ElementRef;
    @ViewChild('boxContainer', {static: true}) private boxContainer: ElementRef;
    @ViewChild('axisBottom', {static: true}) axisBottom: ElementRef;
    @ViewChild('axisLeft', {static: true}) axisLeft: ElementRef;
    @ViewChild('xGrid', {static: true}) xGrid: ElementRef;
    @ViewChild('yGrid', {static: true}) yGrid: ElementRef;
    @ViewChild('axisContextBottom', {static: true}) axisTimelineBottom: ElementRef;
    @ViewChild('axisContextLeft', {static: true}) axisTimelineLeft: ElementRef;
    @ViewChild('zoomContainer') zoomContainer: ElementRef;
    @ViewChild('tooltipContainer') private tooltipRef: ElementRef;
    @ViewChild('tooltipContainerBar') private tooltipRefBar: ElementRef;
    @ViewChild('measureCanvas') private canvas: ElementRef;

    @Input('options')
    set _options(value: VisualizationProtocol.VisualizationBoxPlot | null | undefined) {
        this.options$.next(value);
    }

    options$ = new BehaviorSubject<VisualizationProtocol.VisualizationBoxPlot | null | undefined>(null);

    @Input('data')
    set _seriesData(value: DataProtocol.BoxPlotSeries | null) {
        this.series$.next(value);
    }

    series$ = new BehaviorSubject<DataProtocol.BoxPlotSeries | null>(null);

    @Input() uniqueId = '0';

    destroy$ = new Subject<any>();
    tooltipXCoordinate$ = new BehaviorSubject<DataProtocol.BoxPlotPoint>(null);
    pristineOptions = {} as VisualizationProtocol.VisualizationBoxPlot;

    monthSet = new Set(['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']);
    maxLabelYPrimaryWidth = 0;
    maxLabelYSecondaryWidth = 0;
    fontSize = 14;
    marginBetweenAxisLabelsAndTitle = 15;
    rotationDegree = -20;

    tooltipPosition = {left: 0, top: 0};
    tooltipContent = '';
    tooltipHeader = '';

    leftAxisTitleTranslate = 0;
    bottomAxisTranslate = 0;
    padding = {top: 20, right: 0, bottom: 50, left: 0};
    plotProps = {width: 0, height: 0};
    zoom: any;

    colorPalette = [
        '#6bcaff', '#ff7d85', '#ffd363', '#9edb89',
        '#b1a6fe', '#fca77d', '#86ead6', '#218fcc',
        '#ef5b64', '#dda820', '#6ac04c', '#cb7212',
        '#2fc3a5', '#0970aa', '#d83a43', '#ac7c00',
        '#54921f', '#501f76', '#a15402'];

    xPrimaryScale: (d3.ScaleTime<number, number> | d3.ScaleBand<string> | d3.ScaleLinear<number, number>) & d3.AxisScale<any>;
    yPrimaryScale: (d3.ScaleTime<number, number> | d3.ScaleBand<string> | d3.ScaleLinear<number, number>) & d3.AxisScale<any>;
    new_yPrimaryScale: (d3.ScaleTime<number, number> | d3.ScaleBand<string> | d3.ScaleLinear<number, number>) & d3.AxisScale<any>;
    xPrimaryAxis: d3.Axis<any>;
    yPrimaryAxis: d3.Axis<any>;
    xPrimaryValues: string[];
    yPrimaryValues: number[];

    context: CanvasRenderingContext2D;
    legendSeriesMap = new Map<string, DataProtocol.BoxPlotPoint>();
    drawingSeries = new Array<DataProtocol.BoxPlotPoint>();
    formattedBarSeries = [];

    axisTypeEnum = VisualizationProtocol.AxisValueType;
    shiftState = false;
    showPoint = false;
    showZoom = false;

    @HostListener('document:keydown.shift') shiftPushed() {
        this.shiftState = true;
    }

    @HostListener('document:keyup.shift') shiftReleased() {
        this.shiftState = false;
    }

    constructor(
        private cdr: ChangeDetectorRef,
        public chartBoxExpansionService: ChartBoxExpansionService,
    ) {

        combineLatest(
            this.options$.asObservable(),
            this.series$.pipe(filter(opts => !!opts)),
        ).pipe(
            takeUntil(this.destroy$),
            debounceTime(250),
        ).subscribe(([options, series]) => {
            this.pristineOptions = options;

            this.drawingSeries = series.data.map((item: DataProtocol.BoxPlotPoint) => {
                const modifiedItem = item;
                modifiedItem.label = this.dateParse(item.label);
                return modifiedItem;
            });

            this.drawingSeries.forEach(sds => this.legendSeriesMap.set(sds.label, sds));
            if (this.drawingSeries.length > 0) this.drawChart(this.drawingSeries, this.pristineOptions);
            setTimeout(() => this.cdr.detectChanges());
        });

        // SUBSCRIBE TO CHARTBOX EXPANSION
        this.chartBoxExpansionService.expand
            .pipe(takeUntil(this.destroy$))
            .subscribe(object => {
                setTimeout(() => {
                    if (this.uniqueId === object.containerKey) {
                        this.drawChart(this.drawingSeries, this.pristineOptions);
                        this.cdr.detectChanges();
                    }
                }, object.delay);
            });
    }

    ngOnInit(): void {
        this.tooltipXCoordinate$
            .pipe(filter(item => item !== null),
                takeUntil(this.destroy$)
            )
            .subscribe(item => {
                this.tooltipContent = this.getTooltipContent(item);
            });
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
    }

    ngAfterViewInit(): void {
        this.context = (<HTMLCanvasElement>this.canvas.nativeElement).getContext('2d');
        this.context.font = this.fontSize + 'px Ubuntu';
    }

    drawChart(series: DataProtocol.BoxPlotPoint[], options: VisualizationProtocol.VisualizationBoxPlot): void {
        d3.select(this.plotContainer.nativeElement).attr('clip-path', 'url(#clip' + this.uniqueId + ')');
        this.setUniqueAxisValues(series);
        this.context.font = this.fontSize + 'px Ubuntu';
        this.maxLabelYPrimaryWidth = this.marginBetweenAxisLabelsAndTitle + this.getMaxWidth(this.yPrimaryValues, options.yAxisType);
        this.padding.left = 0;
        this.padding.left += this.maxLabelYPrimaryWidth !== -Infinity ? this.maxLabelYPrimaryWidth : 10;
        this.padding.left += options.yAxisTitle ? this.fontSize : 0;
        this.padding.right = 0;
        this.padding.bottom = options.xAxisTitle ? 50 : 30;
        this.padding.bottom += this.needRotateAxisLabel()
            ? this.getMaxWidth(this.xPrimaryValues, options.xAxisType) * Math.cos(this.rotationDegree * Math.PI / 180)
            : 0;
        const width = this.chartContainer.nativeElement.getBoundingClientRect().width - this.padding.left - this.padding.right,
            height = this.chartContainer.nativeElement.getBoundingClientRect().height - this.padding.top - this.padding.bottom;

        this.plotProps.width = width !== Infinity ? width : 0;
        this.plotProps.height = height !== Infinity ? height : 0;

        this.leftAxisTitleTranslate = (this.plotProps.height + this.context.measureText(options.yAxisTitle).width) / 2;
        this.bottomAxisTranslate = (this.plotProps.width - this.context.measureText(options.xAxisTitle).width) / 2;

        this.createScales(options);
        this.createAxis(options);
        this.appendAxis();
        this.addGridLines(options);
        this.addMouseMoveRect();
        this.addZoom();

        setTimeout(() => {
            this.showZoom = false;
            this.commonDraw(true, this.drawingSeries, this.xPrimaryScale, this.yPrimaryScale);
        });
    }

    zoomed(): void {
        if (this.shiftState && d3.event.type === 'zoom') {
            this.showZoom = true;
            const transform = d3.event.transform;
            this.new_yPrimaryScale = transform.rescaleY(this.yPrimaryScale);
            this.yPrimaryAxis = d3.axisLeft(this.new_yPrimaryScale);
            d3.select(this.axisLeft.nativeElement).transition().duration(0).call(this.yPrimaryAxis.scale(this.new_yPrimaryScale));
            d3.select(this.yGrid.nativeElement).call(this.yPrimaryAxis.tickArguments([7]).tickSize(-this.plotProps.width));
            this.commonDraw(false, this.drawingSeries, this.xPrimaryScale, this.new_yPrimaryScale);
        }
    }

    addMouseMoveRect(): void {
        d3.select(this.zoomContainer.nativeElement)
            .style('fill', 'none')
            .style('pointer-events', 'all')
            .attr('width', this.plotProps.width)
            .attr('height', this.plotProps.height);

    }

    appendAxis(): void {
        if (this.xPrimaryAxis) {
            d3.select(this.axisBottom.nativeElement).transition().duration(800).call(this.xPrimaryAxis)
                .selectAll('text')
                .attr('transform', this.needRotateAxisLabel() ? 'rotate(' + this.rotationDegree + ')' : null)
                .style('text-anchor', this.needRotateAxisLabel() ? 'end' : 'middle');
        }
        if (this.yPrimaryAxis) d3.select(this.axisLeft.nativeElement).transition().duration(800).call(this.yPrimaryAxis);
    }

    commonDraw(animate: boolean, series: DataProtocol.BoxPlotPoint[], scaleX: d3.AxisScale<any>, scaleY: d3.AxisScale<any>): void {
        d3.select(this.boxContainer.nativeElement).selectAll('.box-point').remove();
        d3.select(this.boxContainer.nativeElement).selectAll('.box-line').remove();
        d3.select(this.boxContainer.nativeElement).selectAll('.box-rect').remove();

        let gSelection = d3.select(this.boxContainer.nativeElement);
        let widthBox = 80,
            centerBox = 0;
        series.forEach(data => {
            centerBox = scaleX(data.label);
            const dataSorted = data.value.sort(d3.ascending),
                q1 = d3.quantile(dataSorted, .25),
                median = d3.quantile(dataSorted, .5),
                q3 = d3.quantile(dataSorted, .75),
                interQuantileRange = q3 - q1,
                min = q1 - 1.5 * interQuantileRange,
                max = q3 + 1.5 * interQuantileRange,
                notOutliersData = dataSorted.filter(i => i > min && i < max),
                widthBox = this.plotProps.width / series.length && series.length <= 3 ? 80 : this.plotProps.width / series.length - 10;

            gSelection.append("line")
                .attr('class', 'box-line')
                .attr("x1", centerBox)
                .attr("x2", centerBox)
                .attr("y1", scaleY(notOutliersData[0]))
                .attr("y2", scaleY(notOutliersData[notOutliersData.length - 1]))

                .attr("stroke", "black")

            gSelection.append("line")
                .attr('class', 'box-line')
                .attr("x1", centerBox - widthBox / 2)
                .attr("x2", centerBox + widthBox / 2)
                .attr("y1", scaleY(notOutliersData[0]))
                .attr("y2", scaleY(notOutliersData[0]))
                .attr("stroke", "black");

            gSelection.append("line")
                .attr('class', 'box-line')
                .attr("x1", centerBox - widthBox / 2)
                .attr("x2", centerBox + widthBox / 2)
                .attr("y1", scaleY(notOutliersData[notOutliersData.length - 1]))
                .attr("y2", scaleY(notOutliersData[notOutliersData.length - 1]))

                .attr("stroke", "black");

            if (animate) {
                // rectangle for the main box
                let rect = gSelection.append("rect")
                    .attr('class', 'box-rect')
                    .attr("x", centerBox - widthBox / 2)
                    .attr("y", scaleY(q3))
                    .attr("height", 0)
                    .attr("width", widthBox)
                    .attr("stroke", "black")
                    .style("fill", "#6bcaff")
                    .on('mousemove', () => this.showTooltip())
                    .on('mouseleave', () => this.hideTooltip());

                rect.transition()
                    .ease(d3.easeLinear)
                    .duration(1300)
                    .attr("height", scaleY(q1) - scaleY(q3));
            } else {
                // rectangle for the main box
                let rect = gSelection.append("rect")
                    .attr('class', 'box-rect')
                    .attr("x", centerBox - widthBox / 2)
                    .attr("y", scaleY(q3))
                    .attr("height", scaleY(q1) - scaleY(q3))
                    .attr("width", widthBox)
                    .attr("stroke", "black")
                    .style("fill", "#6bcaff")
                    .on('mousemove', () => this.showTooltip())
                    .on('mouseleave', () => this.hideTooltip());
            }

            gSelection.append("line")
                .attr('class', 'box-line')
                .attr("x1", centerBox - widthBox / 2)
                .attr("x2", centerBox + widthBox / 2)
                .attr("y1", scaleY(median))
                .attr("y2", scaleY(median))
                .attr("stroke", "black")
                .style("width", 80);

            if (this.showPoint) {
                gSelection.selectAll('toto')
                    .data(dataSorted.filter(i => i < min || i > max))
                    .enter()
                    .append("circle")
                    .attr('class', 'box-point')
                    .attr("cx", centerBox)
                    .attr("cy", (d) => scaleY(d))
                    .attr("r", 4)
                    .style("fill", "#6bcaff")
                    .attr("stroke", "none")
                    .attr('opacity', 0.5);
            }
        });
    }

    needRotateAxisLabel(): boolean {
        let labels = this.xPrimaryValues;
        if (this.options$.getValue().xAxisType === VisualizationProtocol.AxisValueType.Date) {
            const domain = this.xPrimaryValues.map(elem => new Date(+elem)),
                maxDate = Math.max.apply(null, domain),
                minDate = Math.min.apply(null, domain),
                dateRange = Math.abs(maxDate - minDate) / (1000 * 3600 * 24);
            labels = labels.map(item => {
                if (dateRange > 365) return new Date(+item).toLocaleDateString('ru-RU');
                else return new Date(+item).toLocaleDateString('ru-RU').slice(0, -5);
            });
        }
        const maxPossibleWidth = (this.chartContainer.nativeElement.getBoundingClientRect().width - this.padding.left - this.padding.right) / this.calculateTickCount(labels),
            maxLabelWidth = Math.max(...labels.map(item => this.context.measureText(item).width));
        return maxLabelWidth > maxPossibleWidth || ((this.xPrimaryScale && this.options$.getValue().xAxisType === VisualizationProtocol.AxisValueType.Date)
            ? this.getMinimalTickDistance(this.xPrimaryScale) < maxLabelWidth : false);
    }

    showTooltip(): void {
        const xCoordinate = d3.mouse(d3.event.target)[0],
            xPrimaryValues = this.drawingSeries.map(np => np.label);

        let needleX = null,
            minDiff = Infinity;

        xPrimaryValues.forEach(item => {
            const diff = Math.abs(xCoordinate - this.xPrimaryScale(item));
            if (diff < minDiff) {
                minDiff = diff;
                needleX = item;
            }
        });
        const needleXCoord = this.xPrimaryScale(needleX),
            tooltipContentPage = this.tooltipRef.nativeElement.getBoundingClientRect();
        this.tooltipPosition.left = needleXCoord + this.padding.left - tooltipContentPage.width / 2
            + (this.pristineOptions.xAxisType === VisualizationProtocol.AxisValueType.Text ? this.xPrimaryScale.bandwidth() / 2 : 0);
        this.tooltipPosition.top = d3.mouse(d3.event.target)[1] - tooltipContentPage.height + 10;
        this.cdr.detectChanges();
        this.tooltipXCoordinate$.next(this.drawingSeries.filter(np => np.label === needleX)[0]);
    }

    hideTooltip(): void {
        this.tooltipXCoordinate$.next(null);
    }

    private getTooltipContent(data: DataProtocol.BoxPlotPoint): string {
        const dataSorted = data.value.sort(d3.ascending),
            q1 = d3.quantile(dataSorted, .25),
            median = d3.quantile(dataSorted, .5),
            q3 = d3.quantile(dataSorted, .75),
            interQuantileRange = q3 - q1,
            min = q1 - 1.5 * interQuantileRange,
            max = q3 + 1.5 * interQuantileRange,
            result =
                '<li>' + data.label + '</li>' +
                '<li>&nbsp</li>' +
                '<li> Median: ' + '<b>' + median + '</b></li>' +
                '<li> Q1: ' + '<b>' + q1 + '</b></li>' +
                '<li> Q3: ' + '<b>' + q3 + '</b></li>' +
                '<li> IQR: ' + '<b>' + interQuantileRange + '</b>' + '<br></li>' +
                '<li>&nbsp</li>' +
                '<li> Min: ' + '<b>' + min + '</b></li>' +
                '<li> Max: ' + '<b>' + max + '</b></li>' +
                '<li> Min Outlier: ' + '<b>' + dataSorted[0] + '</b></li>' +
                '<li> Max Outlier: ' + '<b>' + dataSorted[dataSorted.length - 1] + '</b></li>';
        return result;
    }

    toggleSeries(): void {
        if (this.showPoint) {
            this.showPoint = false;
            this.createScales(this.pristineOptions);
            this.createAxis(this.pristineOptions);
            this.appendAxis();
            this.addGridLines(this.pristineOptions);
            this.showZoom = false;
            this.commonDraw(true, this.drawingSeries, this.xPrimaryScale, this.yPrimaryScale);
        } else {
            this.showPoint = true;
            this.createScales(this.pristineOptions);
            this.createAxis(this.pristineOptions);
            this.appendAxis();
            this.addGridLines(this.pristineOptions);
            this.commonDraw(true, this.drawingSeries, this.xPrimaryScale, this.yPrimaryScale);
        }
    }

    getColorByKey(label: string): string {
        const findIndex = this.drawingSeries.findIndex(item => item.label === label);
        return findIndex === -1 ? '#000' : this.colorPalette[findIndex % this.colorPalette.length];
    }

    formatValue(value: number): number {
        return Math.round(value * 100) / 100;
    }

    getMaxWidth(labels: Array<any>, labelsType: VisualizationProtocol.AxisValueType) {
        // TODO: Find how it set by scale
        const axisTickSize = 5,
            axisPadding = 5;
        if (labelsType === VisualizationProtocol.AxisValueType.Date) {
            const domain = labels.map(elem => new Date(+elem)),
                maxDate = Math.max.apply(null, domain),
                minDate = Math.min.apply(null, domain),
                dateRange = Math.abs(maxDate - minDate) / (1000 * 3600 * 24);
            labels = labels.map(item => {
                if (dateRange > 365) return new Date(+item).toLocaleDateString('ru-RU');
                else return new Date(+item).toLocaleDateString('ru-RU').slice(0, -5);
            });
        }
        return Math.max(...labels.map(item =>
                !isNaN(+item)
                    ? this.context.measureText(Math.round(+item * 10) / 10 + '').width
                    : this.context.measureText(item).width))
            + axisTickSize + axisPadding;
    }

    dateParse(dateString: string | Date): string {
        if (dateString instanceof Date) return dateString.getTime() + '';
        const monthTest = new RegExp(/^([a-zA-Z]*)\s?([0-9]*)?,\s?([0-9][0-9][0-9][0-9])/);
        const quaterTest = new RegExp(/^([0-9][0-9][0-9][0-9])\sQ([0-4])$/);
        if (monthTest.test(dateString)) {
            const matchedArr = dateString.match(monthTest);
            if (this.monthSet.has(matchedArr[1])) {
                const monthNum = new Date(Date.parse(matchedArr[1] + ' 1,' + matchedArr[3])).getMonth();
                return new Date(+matchedArr[3], monthNum, matchedArr[2] === undefined ? 1 : +matchedArr[2]).getTime() + '';
            } else if (matchedArr[1] === 'Wk') {
                return new Date(+matchedArr[3], 0, 1 + (+matchedArr[2] - 1) * 7).getTime() + '';
            }
        } else if (quaterTest.test(dateString)) {
            const matchedArr = dateString.match(quaterTest);
            return new Date(+matchedArr[1], +matchedArr[2] * 3, 1).getTime() + '';
        } else return dateString;
    }

    setUniqueAxisValues(series: DataProtocol.BoxPlotPoint[]): void {
        this.xPrimaryValues = [...new Set(
            series.flatMap(item => item.label.toString()))];
        this.yPrimaryValues = [...new Set(
            series.flatMap(item => item.value.map(element => element)))];
    }

    createScales(options: VisualizationProtocol.VisualizationBoxPlot): void {
        this.xPrimaryScale = this.xPrimaryValues.length > 0
            ? this.setXScale(this.plotProps.width, this.xPrimaryValues, options.xAxisType)
            : null;

        this.yPrimaryScale = this.yPrimaryValues.length > 0
            ? this.setYScale(this.plotProps.height, this.yPrimaryValues, options.yAxisType)
            : null;
    }

    setXScale(width: number, labels: Array<any>, axisType: VisualizationProtocol.AxisValueType): d3.ScaleTime<number, number> | d3.ScaleBand<string> | d3.ScaleLinear<number, number> {
        switch (axisType) {
            case VisualizationProtocol.AxisValueType.Date:
                return d3
                    .scaleTime()
                    .range([0, width])
                    .domain(d3.extent(labels.map(item => new Date(+item))));

            case VisualizationProtocol.AxisValueType.Numeric:
                let tempLabels = labels.map(item => parseFloat(item));
                return d3
                    .scaleLinear()
                    .range([0, width])
                    .domain([tempLabels[0], tempLabels[tempLabels.length - 1]]);

            case VisualizationProtocol.AxisValueType.Text:
                return d3
                    .scaleBand()
                    .range([0, width])
                    .padding(0.1)
                    .domain(labels);
        }
    }

    setYScale(height: number, labels: Array<any>, axisType: VisualizationProtocol.AxisValueType): d3.ScaleTime<number, number> | d3.ScaleBand<string> | d3.ScaleLinear<number, number> {
        switch (axisType) {
            case VisualizationProtocol.AxisValueType.Date:
                return d3
                    .scaleTime()
                    .range([height, 0])
                    .domain(d3.extent(labels.map(item => new Date(+item))));

            case VisualizationProtocol.AxisValueType.Numeric:
                const templabels = labels.sort((a, b) => {
                    return a - b;
                });

                if (this.showPoint) {
                    const deviationLabel = (Math.abs(templabels[0]) + Math.abs(templabels[templabels.length - 1]) / 2) * 0.2;
                    return d3
                        .scaleLinear()
                        .rangeRound([height, 0])
                        .domain([templabels[0] - deviationLabel, templabels[templabels.length - 1] + deviationLabel]);
                } else {
                    const q1 = d3.quantile(templabels, .25),
                        q3 = d3.quantile(templabels, .75),
                        interQuantileRange = q3 - q1,
                        min = q1 - 1.5 * interQuantileRange,
                        max = q3 + 1.5 * interQuantileRange,
                        notOutliersData = templabels.filter(i => i > min && i < max),
                        deviationLabelNotOut = (Math.abs(notOutliersData[0]) + Math.abs(notOutliersData[notOutliersData.length - 1]) / 2) * 0.2;
                    return d3
                        .scaleLinear()
                        .rangeRound([height, 0])
                        .domain([notOutliersData[0] - deviationLabelNotOut, notOutliersData[notOutliersData.length - 1] + deviationLabelNotOut]);
                }

            case VisualizationProtocol.AxisValueType.Text:
                return d3
                    .scaleBand()
                    .range([height, 0])
                    .padding(0.1)
                    .domain(labels);
        }
    }

    createAxis(options: VisualizationProtocol.VisualizationBoxPlot): void {
        if (this.xPrimaryScale) {
            this.xPrimaryAxis = d3.axisBottom(this.xPrimaryScale)
                .tickArguments([this.calculateTickCount(
                    this.xPrimaryValues.filter(date => new Date(+date) <= this.xPrimaryScale.domain()[1] && new Date(+date) >= this.xPrimaryScale.domain()[0]))])
                .tickFormat(options.xAxisType === VisualizationProtocol.AxisValueType.Date
                    ? this.tickFormat(VisualizationProtocol.AxisValueType.Date, this.xPrimaryScale)
                    : null);
        }
        if (this.yPrimaryScale) {
            this.yPrimaryAxis = d3.axisLeft(this.yPrimaryScale);
        }
    }

    addGridLines(options: VisualizationProtocol.VisualizationBoxPlot): void {
        if (options.showXGridLine) {
            if (this.xPrimaryAxis) {
                d3.select(this.xGrid.nativeElement).call(this.xPrimaryAxis.tickArguments([this.calculateTickCount(this.xPrimaryValues) / 3]).tickSize(-this.plotProps.height));
            }
        }
        if (options.showYGridLine) {
            if (this.yPrimaryAxis) {
                d3.select(this.yGrid.nativeElement).call(this.yPrimaryAxis.tickArguments([7]).tickSize(-this.plotProps.width));
            }
        }
    }

    tickFormat(axisType: VisualizationProtocol.AxisValueType, scale: any): (date: Date) => string {
        if (axisType === VisualizationProtocol.AxisValueType.Date) {
            const domain = scale.domain(),
                dateRange = Math.abs(domain[1].getTime() - domain[0].getTime()) / (1000 * 3600 * 24);
            return dateRange < 365 ? d3.timeFormat('%d.%m') : d3.timeFormat('%d.%m.%y');
        }
        return null;
    }

    calculateTickCount(labels: Array<any>): number {
        return Math.min(labels.length, 10);
    }

    addZoom(): void {
        this.zoom = d3.zoom()
            .scaleExtent([1, 5])
            .translateExtent([[0, 0], [this.plotProps.width, this.plotProps.height]])
            .extent([[0, 0], [this.plotProps.width, this.plotProps.height]]).on('zoom', () => this.zoomed());
        d3.select(this.zoomContainer.nativeElement).call(this.zoom);
    }

    getMinimalTickDistance(scale: any): number {
        const ticks = scale.ticks(this.calculateTickCount(this.xPrimaryValues.filter(date => new Date(+date) <= this.xPrimaryScale.domain()[1] && new Date(+date) >= this.xPrimaryScale.domain()[0]))),
            spaces = Array<number>();
        for (let i = 0; i < ticks.length - 1; i++) {
            spaces.push(scale(ticks[i + 1]) - scale(ticks[i]));
        }
        return Math.min(...spaces);
    }
}
