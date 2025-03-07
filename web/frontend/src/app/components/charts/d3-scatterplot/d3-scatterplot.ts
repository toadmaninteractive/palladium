import { AfterViewInit, ChangeDetectionStrategy, ChangeDetectorRef, Component, ElementRef, HostListener, Input, OnDestroy, ViewChild } from '@angular/core';
import { BehaviorSubject, combineLatest, Subject } from 'rxjs';
import { debounceTime, filter, takeUntil } from 'rxjs/operators';
import * as d3 from 'd3';
import { D3ScatterPoint } from '../shared/d3-common-options';
import { D3Basic } from '../shared/d3-basic';
import { ChartBoxExpansionService } from 'src/app/core/services/chart-box-expansion.service';
import * as DataProtocol from '../../../protocol/data-protocol';
import * as VisualizationProtocol from '../../../protocol/visualization-protocol';

@Component({
  selector: 'm-d3-scatterplot',
  templateUrl: './d3-scatterplot.html',
  styleUrls: ['./d3-scatterplot.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class D3ScatterplotComponent extends D3Basic implements AfterViewInit, OnDestroy {
    // TODO: set default options
    @Input('options')
    set _options(value: VisualizationProtocol.VisualizationScatter | null | undefined) {
        this.options$.next(value);
    }

    options$ = new BehaviorSubject<VisualizationProtocol.VisualizationScatter | null | undefined>(null);

    @Input('seriesData')
    set _seriesData(value: DataProtocol.ScatterSeries[] | null) {
        this.series$.next(value);
    }

    series$ = new BehaviorSubject<DataProtocol.ScatterSeries[] | null>([]);

    @Input() maxHeight = 700;

    @ViewChild('chartContainer', {static: true}) chartContainer: ElementRef;
    @ViewChild('axisBottom', {static: true}) axisBottom: ElementRef;
    @ViewChild('axisTop', {static: true}) axisTop: ElementRef;
    @ViewChild('axisLeft', {static: true}) axisLeft: ElementRef;
    @ViewChild('axisRight', {static: true}) axisRight: ElementRef;
    @ViewChild('xGrid', {static: true}) xGrid: ElementRef;
    @ViewChild('yGrid', {static: true}) yGrid: ElementRef;
    @ViewChild('measureCanvas') canvas: ElementRef;
    @ViewChild('data') data: ElementRef;
    @ViewChild('tooltipContainer') tooltipRef: ElementRef;

    destroy$ = new Subject<any>();

    dotRadiusScale: d3.ScaleLinear<number, number>;

    legendSeriesMap = new Map<string, VisualizationProtocol.ScatterSerie>();
    drawingSeries = new Array<DataProtocol.ScatterSeries>();
    pristineOptionsScatter: VisualizationProtocol.VisualizationScatter;

    leftPadding = 38;
    leftAxisTitleTranslateX = 0;

    bottomAxisTranslate = 0;
    plotProps = {width: 0, height: 0};
    legendY = 0;
    formattedData = [];
    context: CanvasRenderingContext2D;
    margin = {top: 0, right: 0, bottom: 0, left: 0};
    padding = {top: 5, right: 20, bottom: 35, left: this.leftPadding};
    nrOfTicks = 4;
    selectedPointsCount: number;
    selectedPointsSumValue: number;
    brushOn = false;


    @HostListener('document:keydown', ['$event'])
    setBrushOn(event: KeyboardEvent) {
        if (event.code === 'KeyS') {
            this.brushOn = true;
            d3.select(this.data.nativeElement)
                .call(d3.brush()
                    .extent([[0, 0], [this.plotProps.width, this.plotProps.height]])
                    .on('end', () => this.brushed()));
        }
    }

    @HostListener('document:keyup', ['$event'])
    setBrushOff(event) {
        if (event.code === 'Escape') {
            this.brushEnd();
        }
    }

    constructor(
        private cdr: ChangeDetectorRef,
        private chartBoxExpansionService: ChartBoxExpansionService) {
        super();
        combineLatest(
            this.options$,
            this.series$.pipe(filter(series => !!series)),
        ).pipe(
            takeUntil(this.destroy$),
            debounceTime(250),
        ).subscribe(([options, series]) => {
            if (series) {
                if (this.context) {
                    this.context.font = '14px Ubuntu sans-serif';
                    let yPrimaryValues = series.reduce((acc, val) => acc.concat(val.data), [])
                        .map(sp => sp.y);
                    yPrimaryValues = yPrimaryValues.map( item => Math.round(item*100)/100)
                    const maxLabelYWidth = Math.max(...yPrimaryValues.map(item => this.context.measureText(item).width));
                    this.padding.left = this.leftPadding + maxLabelYWidth;
                }

                this.legendSeriesMap = new Map();
                this.pristineOptionsScatter = options;
                this.drawingSeries = series;
                options.series.forEach(serie => this.legendSeriesMap.set(serie.key, serie));
                // this.drawingSeries.forEach(sds => this.legendSeriesMap.set(sds.key, sds));
                this.drawChart(series, options);
                setTimeout(() => this.cdr.detectChanges());
            }
        });

        this.chartBoxExpansionService.expand
        .pipe(takeUntil(this.destroy$))
        .subscribe(object => {
            setTimeout(() => {
                this.drawChart(this.drawingSeries, this.options$.getValue());
                this.cdr.detectChanges();
            }, object.delay);
        });
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
    }

    ngAfterViewInit(): void {
        this.context = (<HTMLCanvasElement>this.canvas.nativeElement).getContext('2d');
        this.context.font = '14px Ubuntu sans-serif';

        if (this.drawingSeries) {
            const yPrimaryValues = this.drawingSeries
                .reduce((acc, val) => acc.concat(val.data), [])
                .map(sp => sp.x);

            const maxLabelYWidth = Math.max(...yPrimaryValues.map(item => this.context.measureText(item).width));
            this.padding.left = this.leftPadding + maxLabelYWidth;
        }
    }

    drawChart(series: DataProtocol.ScatterSeries[], options: VisualizationProtocol.VisualizationScatter): void {
        this.cdr.detectChanges();
        const containerWidth = this.chartContainer.nativeElement.getBoundingClientRect().width,
            width = containerWidth - this.padding.left - this.padding.right,
            minimumHeightOfChart = 200,
            maximumHeightOfChart = this.maxHeight,
            uniqueItemsMap = new Set<string | Date>();
        series.forEach(elem => elem.data.forEach(item => uniqueItemsMap.add(item.x)));
        // this.setUniqueAxisValues(series);
        this.plotProps.width = width;
        this.context.font = '14px Ubuntu sans-serif';
        this.plotProps.height = Math.min(maximumHeightOfChart, Math.max(uniqueItemsMap.size * series.length * 25, this.context.measureText(options.yAxisTitle).width, minimumHeightOfChart)); // char height

        this.leftAxisTitleTranslateX = (this.plotProps.height + this.context.measureText(options.yAxisTitle).width) / 2;
        this.bottomAxisTranslate = (this.plotProps.width / 2) - (this.context.measureText(options.xAxisTitle).width / 2) - 10;

        this.legendY = this.plotProps.height + (this.padding.bottom / 2) + 10;

        this.convertData(series);
        this.createScales();
        this.createAxis();
        this.createGrid(options);

        this.drawDots();

    }

    brushed(): void {
        if (!d3.event.selection) return null;
        const selection = d3.event.selection,
        selectedPoints =  this.formattedData.filter(item => this.isSelected(item, selection));
        this.selectedPointsCount = selectedPoints.length;
        this.selectedPointsSumValue = selectedPoints.reduce((acc, current) => acc + current.value, 0);
        this.crossLineToolTip(selection);
        // @ts-ignore
        d3.select(this.data.nativeElement).selectAll('circle').attr('fill', d => this.isSelected(d, selection) ? d.color : '#979797');
    }

    isSelected(item: any, selection: any): boolean {
        return  this.xPrimaryScale(item.x) >= selection[0][0]
            && this.xPrimaryScale(item.x) <= selection[1][0]
            && this.yPrimaryScale(item.y) >= selection[0][1]
            && this.yPrimaryScale(item.y) <= selection[1][1];
    }

    brushEnd(): void {
        if (this.brushOn) {
            d3.select('#crossTextTooltip').remove();
            d3.select('#poligonTooltip').remove();
            if (d3.brush()) {
                d3.brush().move(d3.select(this.data.nativeElement), null);
                d3.select(this.data.nativeElement)
                    .call(d3.brush().extent([[0, 0], [0, 0]]));
            }

            // @ts-ignore
            d3.select(this.data.nativeElement).selectAll('circle').attr('fill', d => d.color);
            this.brushOn = false;
        }
    }

    crossLineToolTip(selection: any) {
        d3.select('#crossTextTooltip').remove();
        d3.select('#poligonTooltip').remove();
        const firstLine = 'points: ' + this.selectedPointsCount,
            secondLine = 'summary: ' + this.selectedPointsSumValue;
        this.context.font = '14px Ubuntu sans-serif';
        const tooltipWidth = 30 + (this.context.measureText(firstLine).width >= this.context.measureText(secondLine).width ? this.context.measureText(firstLine).width : this.context.measureText(secondLine).width);
        const tooltipHeight = 18;
        const xCoord = this.plotProps.width - selection[1][0] > tooltipWidth ? selection[1][0] : selection[0][0],
        yCoord =  selection[0][1] + ((selection[1][1] - selection[0][1]) / 2);
        let marginText = 0;
        if (this.plotProps.width - selection[1][0] > tooltipWidth) {
            marginText = 12;
            d3.select(this.data.nativeElement)
                .append('polyline')
                .attr('id', 'poligonTooltip')
                .attr('stroke', '#3B4298')
                .attr('fill', 'white')
                .attr('stroke-linecap', 'round')
                // tslint:disable-next-line:max-line-length
                .attr('points', xCoord + ',' + yCoord + ' ' + (xCoord + 5) + ',' + (yCoord - 5) + ' ' + (xCoord + 5) + ',' + (yCoord - tooltipHeight) + ' ' + (xCoord + tooltipWidth) + ',' + (yCoord - tooltipHeight) + ' ' + (xCoord + tooltipWidth) + ',' + (yCoord + tooltipHeight) + ' ' + (xCoord + 5) + ',' +
                    (yCoord + tooltipHeight) + ' ' + (xCoord + 5) + ',' + (yCoord + tooltipHeight) + ' ' + (xCoord + 5) + ',' + (yCoord + 10) + ' ' + (xCoord + 5) + ',' + (yCoord + 5) + ' ' + xCoord + ',' + yCoord);
        } else {
            marginText = -tooltipWidth + 5;
            d3.select(this.data.nativeElement)
                .append('polyline')
                .attr('id', 'poligonTooltip')
                .attr('stroke', '#3B4298')
                .attr('fill', 'white')
                .attr('stroke-linecap', 'round')
                // tslint:disable-next-line:max-line-length
                .attr('points', xCoord + ',' + yCoord + ' ' + (xCoord - 5) + ',' + (yCoord - 5) + ' ' + (xCoord - 5) + ',' + (yCoord - 15) + ' ' + (xCoord - tooltipWidth) + ',' + (yCoord - 15) + ' ' + (xCoord - tooltipWidth) + ',' + (yCoord + 18) + ' ' + (xCoord - 5) + ',' +
                    (yCoord + 18) + ' ' + (xCoord - 5) + ',' + (yCoord + 18) + ' ' + (xCoord - 5) + ',' + (yCoord + 10) + ' ' + (xCoord - 5) + ',' + (yCoord + 5) + ' ' + xCoord + ',' + yCoord);
        }

        d3.select(this.data.nativeElement)
            .append('text')
            .attr('class', 'title')
            .attr('id', 'crossTextTooltip')
            .attr('x', xCoord + marginText)
            .attr('y', yCoord + 14)
            .attr('font-size', 14)
            .attr('fill', 'black')
            .attr('font-family', 'monospace')
            .append('tspan')
            .text(secondLine);
        d3.select('#crossTextTooltip')
            .append('tspan')
            .attr('x', xCoord + marginText)
            .attr('y', yCoord - 2)
            .text(firstLine);
    }

    createScales(): void {
        const xPrimaryValues = [],
            yPrimaryValues = [],
            xAxisInverted = this.options$.getValue().invertXAxis,
            yAxisInverted = this.options$.getValue().invertYAxis;
        this.formattedData.forEach(item => !xPrimaryValues.includes(item.x) ? xPrimaryValues.push(item.x) : null);
        this.formattedData.forEach(item => !yPrimaryValues.includes(item.y) ? yPrimaryValues.push(item.y) : null);

        if (this.pristineOptionsScatter.xAxisType === VisualizationProtocol.AxisValueType.Text) {
            this.xPrimaryScale = d3.scaleBand()
            .domain(xPrimaryValues)
            // @ts-ignore
            .range(this.checkIfInverted([0, this.plotProps.width], xAxisInverted));
        } else {
            this.xPrimaryScale = d3.scaleLinear()
            .domain([
                Math.min(...xPrimaryValues) > 0 ? Math.min(...xPrimaryValues) * 0.95 : Math.min(...xPrimaryValues) * 1.05,
                Math.max(...xPrimaryValues) > 0 ? Math.max(...xPrimaryValues) * 1.05 : Math.max(...xPrimaryValues) * 0.95 ])
                .range(this.checkIfInverted([0, this.plotProps.width], xAxisInverted));
        }
        if (this.pristineOptionsScatter.yAxisType === VisualizationProtocol.AxisValueType.Text) {
            this.yPrimaryScale = d3.scaleBand()
            .domain(yPrimaryValues)
            // @ts-ignore
            .range(this.checkIfInverted([0, this.plotProps.height], yAxisInverted));
        } else {
            this.yPrimaryScale = d3.scaleLinear()
            .domain([
                Math.min(...yPrimaryValues) > 0 ? Math.min(...yPrimaryValues) * 0.95 : Math.min(...yPrimaryValues) * 1.05,
                Math.max(...yPrimaryValues) > 0 ? Math.max(...yPrimaryValues) * 1.05 : Math.max(...yPrimaryValues) * 0.95 ])
            .range(this.checkIfInverted([0, this.plotProps.height], yAxisInverted));
        }

        this.dotRadiusScale = d3.scaleLinear()
            // @ts-ignore
            .domain([0, d3.max(this.formattedData.map(item => item.value))])
            .range([2, 16]);
    }

    createAxis(): void {
        if (this.xPrimaryScale) {
            this.xPrimaryAxis = d3.axisBottom(this.xPrimaryScale);
            d3.select(this.axisBottom.nativeElement).transition().duration(800)
                .call(this.xPrimaryAxis.ticks(this.nrOfTicks));
            this.xSecondaryAxis = d3.axisTop(this.xPrimaryScale);
            d3.select(this.axisTop.nativeElement).transition().duration(800)
                .call(this.xSecondaryAxis.ticks(this.nrOfTicks));
        }

        if (this.yPrimaryScale) {
            this.yPrimaryAxis = d3.axisLeft(this.yPrimaryScale);
            d3.select(this.axisLeft.nativeElement).call(this.yPrimaryAxis);
        }
    }

    createGrid(options: VisualizationProtocol.VisualizationScatter): void {
        if (options.showXGridLine) {
            d3.select(this.xGrid.nativeElement)
                .call(this.xPrimaryAxis
                    .ticks(this.nrOfTicks)
                    .tickSize(-this.plotProps.height)
                    .tickSizeOuter(0));
        }
        if (options.showYGridLine) {
            d3.select(this.yGrid.nativeElement)
                .call(this.yPrimaryAxis
                    .tickArguments([5]).tickSize(-this.plotProps.width));
        }
    }

    convertData(series: DataProtocol.ScatterSeries[]): void {
        this.formattedData = [];

        series.forEach(serie => {
            serie.data.forEach(scPoint => {
                const formattedDatapoint = {
                    y: scPoint.y.split('').every(symbol => /\.|,|[0-9]/.test(symbol)) ? Number(parseFloat(scPoint.y).toFixed(2)) : scPoint.y,
                    x: scPoint.x.split('').every(symbol => /\.|,|[0-9]/.test(symbol)) ? Number(parseFloat(scPoint.x).toFixed(2)) : scPoint.x,
                    value: parseFloat(scPoint.value.toFixed(2)),
                    seriesKey: serie.key,
                    color: this.getColorByKey(serie.key),
                    radius: this.getScatterSeriesOptions(serie.key).radius
                } as D3ScatterPoint;

                this.formattedData.push(formattedDatapoint);
            });
        });
        this.formattedData = this.formattedData.sort((a, b) =>  a.value < b.value ? 1 : -1);
    }

    drawDots(): void {
        const dotSelection = d3.select(this.data.nativeElement)
            .selectAll('circle')
            // @ts-ignore
            .data(this.formattedData, d => `${d.y}_${d.seriesKey}`);

        dotSelection.exit()
            .transition()
            .ease(d3.easeLinear)
            .duration(200)
            .attr('r', 0)
            .remove();

        dotSelection
            .attr('cx', d => this.pristineOptionsScatter.xAxisType === VisualizationProtocol.AxisValueType.Text ? this.xPrimaryScale(d.x) + (this.xPrimaryScale.bandwidth() / 2) : this.xPrimaryScale(d.x))
            .attr('cy', d => this.pristineOptionsScatter.yAxisType === VisualizationProtocol.AxisValueType.Text ? this.yPrimaryScale(d.y ) + (this.yPrimaryScale.bandwidth() / 2) : this.yPrimaryScale(d.y))
            .attr('fill', d => d.color)
            .attr('r', d => d.radius ? d.radius : this.dotRadiusScale(d.value));

        dotSelection
            .enter()
            .append<SVGCircleElement>('circle')
            .attr('cx', d => this.pristineOptionsScatter.xAxisType === VisualizationProtocol.AxisValueType.Text ? this.xPrimaryScale(d.x) + (this.xPrimaryScale.bandwidth() / 2) : this.xPrimaryScale(d.x))
            .attr('cy', d => this.pristineOptionsScatter.yAxisType === VisualizationProtocol.AxisValueType.Text ? this.yPrimaryScale(d.y) + (this.yPrimaryScale.bandwidth() / 2) : this.yPrimaryScale(d.y))
            .attr('fill', d => d.color)
            .transition()
            .ease(d3.easeBackOut.overshoot(3))
            .duration(300)
            .attr('r', d => d.radius ? d.radius : this.dotRadiusScale(d.value));
    }

    toggleSeries(key: string): void {
        if (!this.legendSeriesMap.has(key)) {
            const series = this.pristineOptionsScatter.series.find(item => item.key === key);
            this.legendSeriesMap.set(series.key, series);
        } else {
            this.legendSeriesMap.delete(key);
        }

        this.convertData(this.drawingSeries.filter(item => this.legendSeriesMap.has(item.key)));
        this.createScales();
        this.createAxis();
        this.createGrid(this.options$.getValue());
        this.brushEnd();
        this.drawDots();
    }

    getColorByKey(key: string): string {
        if (this.getScatterSeriesOptions(key) && this.getScatterSeriesOptions(key).color) {
            return this.getScatterSeriesOptions(key).color;
        }

        const findIndex = this.drawingSeries.findIndex(item => item.key === key);
        return findIndex === -1 ? '#000' : this.colorPalette[findIndex];
    }

    getScatterSeriesOptions(key: string): VisualizationProtocol.ScatterSerie {
        return this.pristineOptionsScatter.series.find(item => item.key === key);
    }

    checkIfInverted(range: number[], axisInverted: boolean) {
        const invertedArray = [range[1], range[0]];
        if (axisInverted) return invertedArray;
        return range;
    }
}
