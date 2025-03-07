import { AfterViewInit, ChangeDetectorRef, Component, ElementRef, Input, OnDestroy, QueryList, ViewChild, ViewChildren } from '@angular/core';
import { BehaviorSubject, combineLatest, Subject } from 'rxjs';
import { debounceTime, filter, takeUntil } from 'rxjs/operators';
import * as d3 from 'd3';
import { D3Basic } from '../shared/d3-basic';
import { ChartBoxExpansionService } from '../../../core/services/chart-box-expansion.service';
import * as VisualizationProtocol from '../../../protocol/visualization-protocol';
import * as DataProtocol from '../../../protocol/data-protocol';

@Component({
    selector: 'm-d3-synchronized',
    templateUrl: './d3-synchronized.component.html',
    styleUrls: ['./d3-synchronized.component.scss']
})
export class D3SynchronizedComponent extends D3Basic implements OnDestroy, AfterViewInit {
    @ViewChild('graphContainer', {static: true}) graphContainer: ElementRef;
    @ViewChild('chartContainer', {static: true}) chartContainer: ElementRef;
    @ViewChild('measureCanvas') canvas: ElementRef;
    @ViewChild('tooltipContainer') private tooltipContainer: ElementRef;
    @ViewChild('timecontext') timeContext: ElementRef;
    @ViewChild('timecontextLine') timeContextLine: ElementRef;
    @ViewChild('timecontextBrush') timeContextBrush: ElementRef;
    @ViewChild('axisTimelineBottom', {static: true}) axisTimelineBottom: ElementRef;
    @ViewChild('axiTimelineLeftAxis', {static: true}) axisTimelineLeft: ElementRef;
    @ViewChild('axisTimelineRightAxis', {static: true}) axisTimelineRight: ElementRef;
    @ViewChild('zoomContainer') zoomContainer: ElementRef;
    @ViewChildren('svgPlot') svgPlots !: QueryList<ElementRef>;

    @Input('options')
    set _options(value: VisualizationProtocol.VisualizationSynchronizedPlot | null | undefined) {
        this.options$.next(value);
    }
    options$ = new BehaviorSubject<VisualizationProtocol.VisualizationSynchronizedPlot | null | undefined>(null);

    @Input('seriesData')
    set _seriesData(value: DataProtocol.NumericSeries[] | null) {
        this.series$.next(value);
    }

    series$ = new BehaviorSubject<DataProtocol.NumericSeries[] | null>([]);
    destroy$ = new Subject<any>();

    plotProps = { width: 0, height: 0 };
    maximumPlotHeight = 230;
    plotTitleMarginBottom = 10;
    groupSet = new BehaviorSubject<Set<number> | null>(null);

    context: CanvasRenderingContext2D;
    pristineSeries = new Array<DataProtocol.NumericSeries>();
    drawingSeries = new Array<DataProtocol.NumericSeries>();
    pristineOptions: VisualizationProtocol.VisualizationSynchronizedPlot;

    margin = { top: 0, right: 0, bottom: 0, left: 0 };
    leftPadding = 0;
    rightPadding = 0;
    maxLabelYPrimaryWidth = 0;
    maxLabelYSecondaryWidth = 0;
    leftAxisTitleTranslate = 0;
    rightAxisTitleTranslate = 0;
    bottomAxisTranslate = 0;
    padding = { top: 20, right: this.rightPadding, bottom: 40, left: this.leftPadding };
    prevXCoord: number;
    fontSize = 14;

    constructor(private chartBoxExpansionService: ChartBoxExpansionService,
                private cdr: ChangeDetectorRef) {
        super();
        combineLatest(
            this.options$.asObservable(),
            this.series$.pipe(filter(opts => !!opts)),
        ).pipe(
            takeUntil(this.destroy$),
            debounceTime(250),
        ).subscribe(([options, series]) => {
            this.drawingSeries = series.map(
                oneSeria => {
                    const modifiedSeria = oneSeria;
                    modifiedSeria.data.map(
                        item => {
                            const modifyedItem = item;
                            modifyedItem.label = this.dateParse(item.label, options);
                            return modifyedItem;
                        });
                    return modifiedSeria;
                });
            this.drawingSeries.forEach(sds => this.pristineSeries.push(sds));
            this.pristineOptions = options;
            this.drawChart(series, options);

        });

        // Subscribe to chart box expansion
        this.chartBoxExpansionService.expand.subscribe(() => {
            setTimeout(() => {
                d3.selectAll('.plot').remove();
                this.drawChart(this.series$.getValue(), this.options$.getValue());
                this.cdr.detectChanges();
            }, 500);
        });
    }

    ngOnDestroy() {
        this.destroy$.next();
        this.destroy$.complete();
    }

    ngAfterViewInit(): void {
        this.context = (<HTMLCanvasElement>this.canvas.nativeElement).getContext('2d');
        this.context.font = '11px';
    }

    drawChart(series: DataProtocol.NumericSeries[], options: VisualizationProtocol.VisualizationSynchronizedPlot): void {
        this.context.font = '14px Ubuntu';
        this.leftPadding = (options.yAxisTitle && options.yAxisTitle !== '') ? 40 : 0;
        this.rightPadding = (options.secondaryYAxisTitle && options.secondaryYAxisTitle !== '') ? 40 : 0;

        this.setUniqueAxisValues(series);

        // 10 px for tick width and additional char like a common
        this.maxLabelYPrimaryWidth = 10 + Math.max(...this.yPrimaryValues.map(item => !isNaN(+item) ? this.context.measureText(Math.round(+item * 10) / 10 + '').width : this.context.measureText(item).width));
        if (this.ySecondaryValues.length > 0)
            this.maxLabelYSecondaryWidth = 10 + Math.max(...this.ySecondaryValues.map(item => !isNaN(+item) ? this.context.measureText(Math.round(+item * 10) / 10 + '').width + 5 : this.context.measureText(item).width));
        else this.maxLabelYSecondaryWidth = 0;
        this.padding.left = 0;
        this.padding.left += this.maxLabelYPrimaryWidth !== -Infinity ? this.maxLabelYPrimaryWidth : 10;
        this.padding.left += options.yAxisTitle ? this.fontSize : 0;

        this.padding.right = this.maxLabelYSecondaryWidth !== -Infinity ? this.rightPadding + this.maxLabelYSecondaryWidth : 0;
        const width = this.chartContainer.nativeElement.getBoundingClientRect().width - this.padding.left - this.padding.right,
            height = this.maximumPlotHeight - this.plotTitleMarginBottom - this.padding.top - this.padding.bottom;
        this.plotProps.width = width !== Infinity ? width : 0;
        this.plotProps.height = height !== Infinity ? height : 0;
        this.leftAxisTitleTranslate = (this.plotProps.height + this.context.measureText(options.yAxisTitle).width) / 2;
        this.rightAxisTitleTranslate = (this.plotProps.height - this.context.measureText(options.secondaryYAxisTitle).width) / 2;
        this.bottomAxisTranslate = (this.plotProps.width - this.context.measureText(options.xAxisTitle).width) / 2;
        this.createScales(options);
        this.createAxis(options);

        this.groupSet.next(new Set(series.map(item => this.getSeriesOptions(item.key).groupIndex)));
        this.cdr.detectChanges();
        this.groupSet.getValue().size > 1
            ? [...this.groupSet.getValue()].forEach(groupNumber => {
                const seriesOnOnePlot = series.filter(elem => this.getSeriesOptions(elem.key).groupIndex === groupNumber);
                this.drawOnePlot(seriesOnOnePlot, options);
            })
            : this.drawOnePlot(series, options);

        if (options.showTimeline) {
            this.addTimelineAxis(this.axisTimelineBottom, options);
            this.drawTimelineContext(series);
        }

    }

    drawOnePlot(series: DataProtocol.NumericSeries[], options: VisualizationProtocol.VisualizationSynchronizedPlot): void {
        const needleSvg = this.svgPlots.find(item => item.nativeElement.classList.contains('svg-' + this.getSeriesOptions(series[0].key).groupIndex));
        const svgContainer = d3.select(needleSvg.nativeElement)
            // +1 - fix rounding error
            .attr('width', this.chartContainer.nativeElement.getBoundingClientRect().width + 1)
            .attr('height', this.maximumPlotHeight);

        const gContainer = svgContainer.append('g')
            .attr('class', 'plot')
            .attr('width', this.plotProps.width)
            .attr('height', this.plotProps.height)
            .attr('transform', 'translate(' + (this.padding.left) + ',' + (this.padding.top + this.plotTitleMarginBottom) + ')');

        // Axis & Titles
        gContainer.append('g').attr('class', 'axis x-axis')
            .attr('transform', 'translate(0,' + (this.plotProps.height) + ')')
            .call(this.xPrimaryAxis.tickArguments([this.calculateTickCount(this.xPrimaryValues)]).tickFormat(d3.timeFormat('%d.%m')));

        const yUniquePrimaryValues = series
                .filter(item => this.getSeriesOptions(item.key).yAxis === VisualizationProtocol.AxisKindY.Primary)
                .reduce((acc, val) => acc.concat(val.data), [])
                .map(np => np.value),
            yUniqueSecondaryValues = series
                .filter(item => this.getSeriesOptions(item.key).yAxis === VisualizationProtocol.AxisKindY.Secondary)
                .reduce((acc, val) => acc.concat(val.data), [])
                .map(np => np.value);
        this.yPrimaryScale = this.setYScale(this.plotProps.height, yUniquePrimaryValues, options.yAxisType);
        const yAxis = d3.axisLeft(this.yPrimaryScale).ticks(3);
        if (yUniqueSecondaryValues.length) {
            this.ySecondaryScale = this.setYScale(this.plotProps.height, yUniqueSecondaryValues, options.secondaryYAxisType);
            const ySecondaryAxis = d3.axisRight(this.ySecondaryScale).ticks(3);
            gContainer.append('g').attr('class', 'axis y-axis').attr('transform', 'translate(' + this.plotProps.width + ',0)')
                .call(ySecondaryAxis);
        }

        gContainer.append('g')
            .attr('class', 'axis y-axis')
            .call(yAxis);

        gContainer.append('g')
            .attr('class', 'y-primary-title axis-left-title ')
            .append('text')
            .text(options.yAxisTitle)
            .attr('transform', 'rotate(-90)  translate(' + (-this.leftAxisTitleTranslate) + ',' + (-this.maxLabelYPrimaryWidth) + ')');
        gContainer.append('g')
            .attr('class', 'y-secondary-title axis-left-title ')
            .append('text')
            .text(options.secondaryYAxisTitle)
            .attr('transform', 'rotate(90)  translate(' + (this.rightAxisTitleTranslate ? this.rightAxisTitleTranslate : 0) + ',' + (-this.plotProps.width - this.maxLabelYSecondaryWidth) + ')');

        const pathContainer = gContainer.append('g')
            .attr('clip-path', 'url(#clip-' + this.getSeriesOptions(series[0].key).groupIndex + ')')
            .attr('class', 'path-group path-group-' +  this.getSeriesOptions(series[0].key).groupIndex);
       this.drawPlotSeries(pathContainer, series);

        // Title text
        gContainer.append('text')
            .text(() => this.combineTitles(series))
            .attr('fill', 'black')
            .attr('x', this.calculateTranslate(series))
            .attr('y', -this.plotTitleMarginBottom)
            .attr('class', 'plot-title');

        // Grid lines
        if (options.showXGridLine) {
            gContainer.append('g')
                .attr('class', 'x-grid')
                .attr('transform', 'translate(0,' + this.plotProps.height + ')')
                .call(this.xPrimaryAxis.tickArguments([this.calculateTickCount(this.xPrimaryValues)]).tickSize(-this.plotProps.height));
        }
        if (options.showYGridLine) {
            gContainer.append('g')
                .attr('class', 'y-grid')
                .call(this.yPrimaryAxis.ticks(3).tickSize(-this.plotProps.width));
        }

        // Tooltip vertical line
        gContainer
            .append('path')
            .attr('class', 'tooltip-line')
            .attr('d', 'M0,0 0,0')
            .attr('stroke', 'red');

        // Tooltip icons
        gContainer.append('g').classed('tooltip-icons', true)
            .selectAll('.tooltip-symbol')
            .data<DataProtocol.NumericSeries>(series)
            .enter()
            .append('path')
            .attr('class', d => 'tooltip-symbol ' + d.key)
            .attr('opacity', '0')
            .attr('d', (d, i) => this.symbolGenerator(i))
            .attr('fill', d => this.getColorByKey(d.key, series));

        gContainer
            .append('rect')
            .classed('mouseMoveContainer', true)
            .attr('width', this.plotProps.width)
            .attr('height', '200')
            .attr('fill', 'transparent')
            .on('mousemove', () => this.mouseMove())
            .on('mouseleave', () => this.mouseLeave());
    }

    drawPlotSeries(pathContainer: any, series: DataProtocol.NumericSeries[]): void {
        pathContainer.selectAll('path').remove();

        pathContainer.selectAll('path')
            .data(series)
            .enter()
            .append('path')
            .attr('d', d => this.drawLine(d.data,
                this.getSeriesOptions(d.key).yAxis === VisualizationProtocol.AxisKindY.Primary ? this.yPrimaryScale : this.ySecondaryScale,
                true, true))
            .attr('fill', 'none')
            .attr('class', d => 'groupIndex' + d.key)
            .attr('stroke', d => this.getColorByKey(d.key, series))
            .attr('stroke-width', '2');
    }

    drawTimelineContext(series: DataProtocol.NumericSeries[]): void {
        if (!series) return;
        d3.select(this.timeContextLine.nativeElement)
            .selectAll('path').remove();
        d3.select(this.timeContextLine.nativeElement)
            .selectAll('path')
            .data(series)
            .enter()
            .append('path')
            .attr('class', d => d.key + ' timeline')
            .attr('d', (d, i) => this.drawTimeLine(d.data, i))
            .attr('fill', 'none')
            .attr('stroke', d => this.getColorByKey(d.key));

        d3.select(this.axisTimelineLeft.nativeElement).append('path').attr('class', 'axis').attr('d', 'M 0,0 0,40');
        d3.select(this.axisTimelineRight.nativeElement).append('path').attr('class', 'axis').attr('d', 'M 0,0 0,40');
    }

    drawTimeLine(data: DataProtocol.NumericPoint[], index: number) {
        const line = d3.line<DataProtocol.NumericPoint>()
            .x(d => this.xTimelineScale(d.label))
            .y(d => this.yTimelineScale(d.value));
        return line(data);
    }

    brushed(): void {
        if (d3.event && d3.event.sourceEvent && d3.event.sourceEvent.type === 'zoom') return; // ignore brush-by-zoom
        const s = d3.event ? d3.event.selection || this.xTimelineScale.range() : this.xTimelineScale.range();

        // @ts-ignore
        this.xPrimaryScale.domain(s.map(this.xTimelineScale.invert, this.xTimelineScale));
        this.createAxis(this.pristineOptions);
        d3.selectAll('.x-axis')
            .call(this.xPrimaryAxis);

        // redraw data
        this.groupSet.getValue().size > 1
            ? [...this.groupSet.getValue()].forEach(groupNumber => {

                const seriesOnOnePlot = this.series$.getValue().filter(elem => this.getSeriesOptions(elem.key).groupIndex === groupNumber);
                const yUniquePrimaryValues = seriesOnOnePlot
                        .filter(item => this.getSeriesOptions(item.key).yAxis === VisualizationProtocol.AxisKindY.Primary)
                        .reduce((acc, val) => acc.concat(val.data), [])
                        .map(np => np.value),
                    yUniqueSecondaryValues = seriesOnOnePlot
                        .filter(item => this.getSeriesOptions(item.key).yAxis === VisualizationProtocol.AxisKindY.Secondary)
                        .reduce((acc, val) => acc.concat(val.data), [])
                        .map(np => np.value);
                this.yPrimaryScale = this.setYScale(this.plotProps.height, yUniquePrimaryValues, this.options$.getValue().yAxisType);
                if (yUniqueSecondaryValues.length) {
                    this.ySecondaryScale = this.setYScale(this.plotProps.height, yUniqueSecondaryValues, this.options$.getValue().secondaryYAxisType);
                }

                this.drawPlotSeries(d3.select('.path-group-' + groupNumber), seriesOnOnePlot);
            })
            : this.drawPlotSeries(d3.select('.path-group-0'), this.series$.getValue());
    }

    mouseMove(): void {
        const xCoord = d3.mouse(d3.event.target)[0],
            mouseMoveContainerWidth = d3.event.target.width.baseVal.value,
            xPrimaryValues = this.drawingSeries
                // .filter(item => item.xAxisPosition === D3AxisPosition.Primary)
                .reduce((acc, val) => acc.concat(val.data), [])
                .map(np => np.label);
        let needleX = null,
            minDiff = Infinity;

        xPrimaryValues.forEach(item => {
            const diff = Math.abs(xCoord - this.xPrimaryScale(item));

            if (diff < minDiff) {
                minDiff = diff;
                needleX = item;
            }
        });

        const needleXCoord = this.xPrimaryScale(needleX);

        if (needleXCoord !== this.prevXCoord) {
            d3.selectAll('.tooltip-line')
                .transition()
                .duration(50)
                .attr('d', 'M' + needleXCoord + ',0 ' + needleXCoord + ',' + this.plotProps.height);

            d3.selectAll<SVGElement, DataProtocol.NumericSeries>('.tooltip-symbol')
                .attr('opacity', 1)
                .attr('transform', d => this.tooltipSymbolTranslate(needleX, d));

            [...this.groupSet.getValue()].forEach((group, index) => {
                const tooltipSelection = d3.select<HTMLBaseElement, any>('.tooltip-container-' + group);
                tooltipSelection.html('<ul>' + this.getTooltipContent(needleX, this.pristineSeries.filter(item => this.getSeriesOptions(item.key).groupIndex === group)) + '</ul>');
                const tooltipHeight = tooltipSelection.node().getBoundingClientRect().height,
                    tooltipWidth = d3.selectAll<HTMLElement, DataProtocol.NumericSeries>('.tooltip-container-' + group).node().getBoundingClientRect().width;

                tooltipSelection
                    .classed(mouseMoveContainerWidth - needleXCoord - tooltipWidth >= 0 ? 'tooltip-container' : 'tooltip-container-arrow-right', true)
                    .classed(mouseMoveContainerWidth - needleXCoord - tooltipWidth < 0 ? 'tooltip-container' : 'tooltip-container-arrow-right', false)
                    .style('opacity', 1)
                    .style('top', (this.plotProps.height  + this.padding.top + this.padding.bottom) * (index + 0.5) + this.plotTitleMarginBottom * index - tooltipHeight / 2 + 'px')
                    // 8px - for tooltip arrow
                    .style('left', mouseMoveContainerWidth - needleXCoord - tooltipWidth > 0 ? needleXCoord + (this.padding.left) + 8 + 'px' : needleXCoord + (this.padding.left ) - tooltipWidth - 8 + 'px');
            });
        }
            this.prevXCoord = needleXCoord;
    }

    mouseLeave(): void {
        d3.selectAll('.tooltip-line')
            .transition()
            .duration(50)
            .attr('d', 'M0,0 0,0');

        d3.selectAll('.tooltip-symbol')
            .attr('opacity', 0);

        [...this.groupSet.getValue()].forEach(group => {
            const tooltipSelection = d3.selectAll('.tooltip-container-' + group);
            tooltipSelection.style('opacity', 0);
        });

    }

    drawLine(data: DataProtocol.NumericPoint[], yScale: (d3.ScaleTime<number, number> | d3.ScaleBand<string> | d3.ScaleLinear<number, number>) & d3.AxisScale<any>,
             flag: boolean = true, curved: boolean = false) {
        if (!data) return;
        const line = d3.line<DataProtocol.NumericPoint>()
                .curve(curved ? d3.curveMonotoneX : d3.curveLinear)
                .x(d => this.xPrimaryScale(d.label))
                .y(d => flag ? yScale(d.value) : yScale(0));

        return line(data);
    }

    getColorByKey(key: string, series: DataProtocol.NumericSeries[] = null): string {
        const findIndex = series ? series.findIndex(item => item.key === key) : this.pristineSeries.findIndex(item => item.key === key);
        return findIndex === -1 ? '#000' : this.colorPalette[findIndex % this.colorPalette.length];
    }

    tooltipSymbolTranslate(xLabel: string | Date, serie: DataProtocol.NumericSeries): string {
        const xCoordinate = this.xPrimaryScale(xLabel),
            yValue = serie.data.filter(elem => {
                if (this.pristineOptions.xAxisType === VisualizationProtocol.AxisValueType.Date) return new Date(+elem.label).getTime() === new Date(+xLabel).getTime();
                    else return elem.label === xLabel;
                })[0].value,
            seriesGroup = this.pristineSeries.filter(item => this.getSeriesOptions(item.key).groupIndex === this.getSeriesOptions(serie.key).groupIndex),
            yUniqueValues = seriesGroup
                .filter(item => this.getSeriesOptions(item.key).yAxis === this.getSeriesOptions(serie.key).yAxis)
                .reduce((acc, val) => acc.concat(val.data), [])
                .map(np => np.value),
            yScale = this.setYScale(this.plotProps.height, yUniqueValues, this.pristineOptions.yAxisType),
            yCoordinate = yScale(yValue);
        return 'translate(' + xCoordinate + ',' + yCoordinate + ')';
    }

    setYScale(height: number, labels: Array<string>, axisType: VisualizationProtocol.AxisValueType): d3.ScaleLinear<number, number> {
        return d3
            .scaleLinear()
            .rangeRound([height, 0])
            .domain([0, Math.max(...labels.map(item => parseFloat(item))) * 1.1]);
    }

    combineTitles(series: DataProtocol.NumericSeries[]): string {
        return series.map(item => this.getSeriesOptions(item.key).name).join(' – ');
    }

    calculateTranslate(series: DataProtocol.NumericSeries[]): number {
        return (this.plotProps.width - this.context.measureText(this.combineTitles(series)).width) / 2;
    }

    private getTooltipContent(xLabel: string | Date, series: DataProtocol.NumericSeries[]): string {
        if (series.length === 0) return '';
        let result = '';
        result += '<span class="tooltip-title">' + (this.pristineOptions.xAxisType === VisualizationProtocol.AxisValueType.Date ? new Date(+xLabel).toLocaleDateString('ru-RU') : xLabel) + '</span>';
        series.forEach((oneSerie) => {
            const value = oneSerie.data.filter(elem => {
                if (this.pristineOptions.xAxisType === VisualizationProtocol.AxisValueType.Date) return new Date(+elem.label).getTime() === new Date(+xLabel).getTime();
                else return elem.label === xLabel;
            })[0].value;
            result += '<li>' +
                '<span style="margin: 0 3px; color:' + this.getColorByKey(oneSerie.key, series) + '">●</span>' +
                this.getSeriesOptions(oneSerie.key).name + ': ' + '<b>' + Math.round(value * 10) / 10 + '</b></li>';
        });

        return result;
    }

    calculateTickCount(labels: Array<any>): number {
        return Math.min(labels.length, 7);
    }
}
