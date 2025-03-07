import {
    AfterViewInit,
    ChangeDetectionStrategy,
    ChangeDetectorRef,
    Component,
    ElementRef,
    Input,
    OnDestroy,
    ViewChild
} from '@angular/core';
import { BehaviorSubject, combineLatest, Subject } from 'rxjs';
import { debounceTime, filter, takeUntil } from 'rxjs/operators';
import * as d3 from 'd3';
import { D3BarPoint } from '../shared/d3-common-options';
import { D3Basic } from '../shared/d3-basic';
import { ChartBoxExpansionService } from 'src/app/core/services/chart-box-expansion.service';
import * as VisualizationProtocol from '../../../protocol/visualization-protocol';
import * as DataProtocol from '../../../protocol/data-protocol';
import * as D3CommonOptions from '../shared/d3-common-options';

@Component({
    selector: 'm-d3-horizontal-bars',
    templateUrl: './d3-horizontal-bars.component.html',
    styleUrls: ['./d3-horizontal-bars.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class D3HorizontalBarsComponent extends D3Basic implements AfterViewInit, OnDestroy {
    @Input('options')
    set _options(value: VisualizationProtocol.VisualizationHorizontalBars | null | undefined) {
        this.options$.next(value);
    }

    options$ = new BehaviorSubject<VisualizationProtocol.VisualizationHorizontalBars | null | undefined>(null);

    @Input('seriesData')
    set _seriesData(value: DataProtocol.NumericSeries[] | null) {
        this.series$.next(value);
    }

    @Input()
    countRows: number;

    series$ = new BehaviorSubject<DataProtocol.NumericSeries[] | null>([]);

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


    ySubgroupScale: (d3.ScaleTime<number, number> | d3.ScaleBand<string> | d3.ScaleLinear<number, number>) & d3.AxisScale<any>;

    legendSeriesMap = new Map<string, DataProtocol.NumericSeries>();
    drawingSeries = new Array<DataProtocol.NumericSeries>();
    pristineHorOptions = {} as VisualizationProtocol.VisualizationHorizontalBars;

    leftPadding = 38; // padding for axis title
    leftAxisTitleTranslateX = 0;

    bottomAxisTranslate = 0;
    plotProps = {width: 0, height: 0};
    tooltipPosition = {left: 0, top: 0};
    tooltipContent = '';
    legendY = 0;
    formattedData = [];
    selectedSeriesKey = false;
    context: CanvasRenderingContext2D;
    margin = {top: 0, right: 0, bottom: 0, left: 0};
    padding = {top: 20, right: 0, bottom: 68, left: this.leftPadding};
    nrOfTicks = 4;

    constructor(
        private cdr: ChangeDetectorRef,
        private chartBoxExpansionService: ChartBoxExpansionService
    ) {
        super();
        combineLatest(
            this.options$.asObservable(),
            this.series$.pipe(filter(opts => !!opts)),
        ).pipe(
            takeUntil(this.destroy$),
            debounceTime(250),
        ).subscribe(([options, series]) => {
            if (series) {
                if (this.context) {
                    this.context.font = '14px Roboto';
                    const yPrimaryValues = series
                        .reduce((acc, val) => acc.concat(val.data), [])
                        .map(np => np.label);
                    const maxLabelYWidth = Math.max(...yPrimaryValues.map(item => this.context.measureText(item).width));

                    this.padding.left = this.leftPadding + maxLabelYWidth;
                }

                this.legendSeriesMap = new Map();
                this.pristineHorOptions = options;

                if (this.countRows > 0) {
                    if (series.length === 1) {
                        for (let i = series[0].data.length; i >= this.countRows; i--) {
                            series[0].data.splice(i, 1);
                        }
                    } else {
                        if (series.length > 1) {
                            series.forEach(item => {
                                const leng = Math.floor(this.countRows / series.length);
                                for (let i = item.data.length; i >= leng; i--) {
                                    item.data.splice(i, 1);
                                }
                            });
                        }
                    }
                }

                this.drawingSeries = series;
                this.drawingSeries.forEach(sds => this.legendSeriesMap.set(sds.key, sds));
                this.drawChart(series, options);
                setTimeout(() => this.cdr.detectChanges());
            }
        });

        // SUBSCRIBE TO CHARTBOX EXPANSION
        this.chartBoxExpansionService.expand
            .pipe(takeUntil(this.destroy$))
            .subscribe(object => {
                setTimeout(() => {
                    this.drawChart(this.series$.getValue(), this.options$.getValue());
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
        this.context.font = '14px Roboto';
        if (this.drawingSeries) {
            const yPrimaryValues = this.drawingSeries
                .reduce((acc, val) => acc.concat(val.data), [])
                .map(np => np.label);
            const maxLabelYWidth = Math.max(...yPrimaryValues.map(item => this.context.measureText(item).width));
            this.padding.left = this.leftPadding + maxLabelYWidth;
        }
    }

    drawChart(series: DataProtocol.NumericSeries[], options: VisualizationProtocol.VisualizationHorizontalBars): void {
        this.cdr.detectChanges();
        const containerWidth = this.chartContainer.nativeElement.getBoundingClientRect().width,
            width = containerWidth - this.padding.left - this.padding.right,
            minimumHeightOfChart = 200,
            uniqueItemsMap = new Set<string | Date>();
        series.forEach(elem => elem.data.forEach(item => uniqueItemsMap.add(item.label)));
        this.setUniqueAxisValues(series);

        this.plotProps.width = width;
        this.context.font = '14px Roboto';

        this.plotProps.height = Math.max(uniqueItemsMap.size * series.length * 25, this.context.measureText(options.yAxisTitle).width, minimumHeightOfChart); // char height

        this.leftAxisTitleTranslateX = (this.plotProps.height + this.context.measureText(options.yAxisTitle).width) / 2;
        this.bottomAxisTranslate = (this.plotProps.width / 2) - (this.context.measureText(options.xAxisTitle).width / 2) - 10;

        this.legendY = this.plotProps.height + (this.padding.bottom / 2) + 10;

        this.createHorizontalScales(series);
        this.createAxis();
        this.createGrid(options);

        this.convertData(series);
        this.drawBars();

    }

    createHorizontalScales(series: DataProtocol.NumericSeries[]): void {
        let nrOfNumericPoints = 0;
        series.forEach(serie => serie.data.forEach(() => nrOfNumericPoints++));

        const xPrimaryValues = series
                .reduce((acc, val) => acc.concat(val.data), [])
                .map(np => np.value),
            yPrimaryValues = series
                .reduce((acc, val) => acc.concat(val.data), [])
                .map(np => np.label);

        this.xPrimaryScale = d3.scaleLinear()
            .domain([0, Math.max(...xPrimaryValues) * 1.1])
            .range([0, this.plotProps.width - 1]); // fix bug with abnormal round and non-displayed last tick

        this.yPrimaryScale = d3.scaleBand()
            .range([0, this.plotProps.height])
            .domain(yPrimaryValues)
            .paddingOuter(nrOfNumericPoints <= 2 ? 0.8 : 0.25);

        const subgroups = series.map(item => item.key);
        series.length > 1
            ? this.ySubgroupScale = d3.scaleBand()
                .domain(subgroups)
                .range([0, this.yPrimaryScale.bandwidth()])
                .paddingInner(.25)
                .paddingOuter(0.5)
            : this.ySubgroupScale = d3.scaleBand()
                .domain(subgroups)
                .range([0, this.yPrimaryScale.bandwidth()])
                .paddingOuter(.5);
    }

    // setUniqueHorAxisValues(series: DataProtocol.NumericSeries[]): void {
    //     // @ts-ignore
    //     this.xPrimaryValues = [...new Set(series
    //         .filter(item => item.xAxisPosition === D3AxisPosition.Primary)
    //         .flatMap(item => item.data.map(element => element.label.toString()))
    //     )];
    //
    //     this.xSecondaryValues = series
    //         .filter(item => item.xAxisPosition === D3AxisPosition.Secondary)
    //         .reduce((acc, val) => acc.concat(val.data), [])
    //         .map(np => np.label),
    //         this.yPrimaryValues = series
    //             .filter(item => item.yAxisPosition === D3AxisPosition.Primary)
    //             .reduce((acc, val) => acc.concat(val.data), [])
    //             .map(np => np.value),
    //         this.ySecondaryValues = series
    //             .filter(item => item.yAxisPosition === D3AxisPosition.Secondary)
    //             .reduce((acc, val) => acc.concat(val.data), [])
    //             .map(np => np.value);
    // }


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

    createGrid(options: VisualizationProtocol.VisualizationHorizontalBars): void {
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

    convertData(series: DataProtocol.NumericSeries[]): void {
        this.formattedData = [];

        series.forEach(serie =>
            serie.data.forEach(elem => {
                if (this.formattedData.filter(item => item['label'] === elem.label).length === 0) {
                    this.formattedData.push({label: elem.label, values: []});
                }
                this.formattedData.filter(item => item['label'] === elem.label)[0]['values'].push({
                    key: serie.key,
                    value: elem.value
                });
            }));
    }

    drawBars(): void {

        d3.select(this.data.nativeElement).selectAll('.bar-value').remove();
        const fontSize = 12;
        const barGroupSelection = d3.select(this.data.nativeElement)
            .selectAll<SVGGElement, any>('g')
            .data(this.formattedData);
        barGroupSelection.exit().remove();

        const barGroupSelectionEnter = barGroupSelection.enter()
            .append<SVGGElement>('g')
            .merge(barGroupSelection)
            .attr('transform', d => 'translate(0,' + this.yPrimaryScale(d.label) + ')');


        const bars = barGroupSelectionEnter
            .merge(barGroupSelection)
            .selectAll<SVGRectElement, any>('rect')
            .data<D3CommonOptions.D3BarPoint>(d => d.values);

        bars.exit().transition().attr('width', 0).remove();

        bars.enter()
            .append<SVGRectElement>('rect')
            .merge(bars)
            .attr('class', d => d.key + ' h-bars')
            .attr('y', d => this.ySubgroupScale(d.key))
            .attr('height', this.ySubgroupScale.bandwidth())
            .attr('fill', d => this.getColorByKey(d.key))
            .on('mouseover', d => this.showTooltip(d))
            .on('mouseleave', () => this.hideTooltip())
            .transition()
            .duration(800)
            .attr('width', d => (d.value > 0 && this.xPrimaryScale(d.value) > 1) ? this.xPrimaryScale(d.value) : 1);


        const barValueSelection = barGroupSelectionEnter.selectAll<SVGTextElement, any>('text')
            .data<D3BarPoint>(d => d.values);

        barValueSelection.exit().remove();

        barValueSelection.enter()
            .append('text')
            .merge(barValueSelection)
            .attr('class', 'bar-value')
            .text(d => this.formatDecimal((Math.round(d.value * 100) / 100).toString()))
            .attr('y', d => this.ySubgroupScale(d.key) + (this.ySubgroupScale.bandwidth() / 2) + (fontSize / 2.8)) // 2.8 - just a ratio for vertical align
            .attr('font-size', fontSize + 'px')
            .attr('fill', 'black')
            .transition()
            .duration(800)
            .attr('x', d => (d.value > 0 && this.xPrimaryScale(d.value) > 1) ? this.xPrimaryScale(d.value) + 5 : 10);

        // REMOVE AXIS LABELS IF VALUES ARE 0
        const sumOfValuesIsZero = !this.formattedData
            .map((item) => item.values.map(numericPoint => numericPoint.value).reduce((acc, val) => acc + val))
            .some(value => value > 0);

        if (sumOfValuesIsZero) {
            d3.select(this.axisTop.nativeElement).selectAll('g').each((d, i, arr) => d3.select(arr[i]).text(''));
            d3.select(this.axisBottom.nativeElement).selectAll('g').each((d, i, arr) => d3.select(arr[i]).text(''));
        }

        // barValueSelection.exit().remove();
    }

    showTooltip(d): void {
        // if (this.options$.getValue().showTooltip) {
        const coordsElem = d3.event.target.getBoundingClientRect();
        const currentClass = d3.event.target.className;
        if (this.selectedSeriesKey === d.key || this.drawingSeries.map(item => item.key).findIndex(element => element === d.key) === -1) return;
        this.selectedSeriesKey = true;
        const titleTool = this.getSeriesOptions(d.key).name;
        if (titleTool) {
            this.tooltipContent = this.getSeriesOptions(d.key).name + '<br/>' + this.formatValue(d.value) + '';
        } else {
            this.tooltipContent = this.formatValue(d.value) + '';
        }
        const tooltipContentPage = this.tooltipRef.nativeElement.getBoundingClientRect();
        this.tooltipPosition.top = coordsElem.top - tooltipContentPage.height / 2 + coordsElem.height / 2;
        this.tooltipPosition.left = coordsElem.left + coordsElem.width + 8;
        d3.select(this.chartContainer.nativeElement).selectAll<SVGRectElement, any>('rect')
            // @ts-ignore
            .filter(function () {
                if (this.classList.value === currentClass.baseVal) {
                    this.setAttribute('style', 'opacity:1');
                } else {
                    this.setAttribute('style', 'opacity:0.2');
                }
            });
        this.cdr.detectChanges();
        // }

    }

    hideTooltip(): void {
        this.selectedSeriesKey = false;
        this.cdr.detectChanges();
        d3.select(this.chartContainer.nativeElement).selectAll<SVGRectElement, any>('rect')
            // @ts-ignore
            .filter(function () {
                if (this.classList.contains('h-bars')) {
                    this.setAttribute('style', 'opacity:1');
                }
            });
        this.cdr.detectChanges();
    }

    toggleSeries(key: string): void {
        if (!this.legendSeriesMap.has(key)) {
            const series = this.drawingSeries.find(item => item.key === key);
            this.legendSeriesMap.set(series.key, series);
        } else this.legendSeriesMap.delete(key);
        this.createHorizontalScales(this.drawingSeries.filter(item => this.legendSeriesMap.has(item.key)));
        this.createAxis();
        this.convertData(this.drawingSeries.filter(item => this.legendSeriesMap.has(item.key)));
        this.drawBars();
    }

    getColorByKey(key: string): string {
        const findIndex = this.drawingSeries.findIndex(item => item.key === key);
        return findIndex === -1 ? '#000' : this.colorPalette[findIndex];
    }

    formatValue(value: number): number {
        return Math.round(value * 100) / 100;
    }

    formatDecimal(num) {
        return num.toString().replace(/(\d)(?=(\d{3})+(?!\d))/g, '$1,');
    }

    getSeriesOptions(key: string): any {
        return this.pristineHorOptions.series.find(item => item.key === key);
    }
}
