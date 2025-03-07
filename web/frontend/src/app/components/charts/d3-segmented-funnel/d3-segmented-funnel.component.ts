import { Component, ElementRef, OnDestroy, ViewChild, Input, ChangeDetectorRef, HostListener } from '@angular/core';
import { BehaviorSubject, combineLatest, Subject, Subscription } from 'rxjs';
import { debounceTime, filter, takeUntil } from 'rxjs/operators';
import * as d3 from 'd3';
import { D3Options } from '../shared/d3-common-options';
import * as DataProtocol from '../../../protocol/data-protocol';
import { VisualizationFunnel } from '../../../protocol/visualization-protocol';
import { D3ResponsiveSize } from '../shared/d3-responsive-sizes';
import { WindowRefService } from 'src/app/core/services/window-ref.service';
import { ChartBoxExpansionService } from 'src/app/core/services/chart-box-expansion.service';


@Component({
    selector: 'm-d3-segmented-funnel',
    templateUrl: './d3-segmented-funnel.component.html',
    styleUrls: ['./d3-segmented-funnel.component.scss']
})
export class D3SegmentedFunnelComponent implements OnDestroy {
    @ViewChild('chartContainer', {static: true}) private chartContainer: ElementRef;
    @ViewChild('inner', {static: true}) private innerContainer: ElementRef;
    @ViewChild('graphContainer', {static: true}) private graphContainer: ElementRef;

    @Input('options')
    set _options(value: VisualizationFunnel | null | undefined) {
        this.options$.next(value);
    }

    defaultOptions = {title: '', subtitle: ''} as D3Options;
    options$ = new BehaviorSubject<VisualizationFunnel | null | undefined>(null);

    @Input('seriesData')
    set _seriesData(value: DataProtocol.NumericSeries[] | null) {
        this.series$.next(value);
    }

    series$ = new BehaviorSubject<DataProtocol.NumericSeries[] | null>(null);
    destroy$ = new Subject<any>();
    responsiveMode: string;
    relativePerc = false; // SAVED, NOT USED
    totalSessions: number;
    conversionRate: number;

    // SELECTIONS
    xAxis: any;
    yAxis: any;
    xSubgroupScale: any;
    subgroups = [];

    // OPTIONS
    caption = 'Funnel Chart';
    segmentTitlePaddings = {top: 22, right: 0, bottom: 0, left: 17};
    graphContainerWidth = 0;
    graphContainerHeight = 313;
    columnWidthRatio: number;
    innerPaddingRatio = 0.2;
    columnWidth: number;
    innerPadding: number;

    highlightedBarKey = null;
    formattedData: any;
    xScale: any;
    yScale: any;


    colorPalette = [
        '#34BFA3', '#716ACA', '#f4516C', '#EECC02',
        '#00c5dc', '#3B4298', '#BD1F75', '#F1941E',
        '#9A66E1', '#22A3A6', '#1474E5', '#EC5B63',
        '#8CDE4B', '#CB7212', '#4DAEFA', '#39994A',
        '#E1B7E1', '#D83A43', '#75B53E'];

    constructor(private cdr: ChangeDetectorRef,
                private window: WindowRefService,
                private chartBoxExpansionService: ChartBoxExpansionService) {
        combineLatest(
            this.options$.asObservable(),
            this.series$.pipe(filter(opts => !!opts)),
        ).pipe(
            takeUntil(this.destroy$),
            debounceTime(250),
        ).subscribe(([options, series]) => {
            this.drawGraph(series);
            setTimeout(() => this.cdr.detectChanges());
        });

        // SUBSCRIBE TO CHARTBOX EXPANSION
        this.chartBoxExpansionService.expand
        .pipe(takeUntil(this.destroy$))
        .subscribe(object => {
            setTimeout(() => {
                this.drawGraph(event, this.series$.getValue());
                this.cdr.detectChanges();
            }, object.delay);
        });
    }

    ngOnDestroy() {
        this.destroy$.next();
        this.destroy$.complete();
    }

    @HostListener('window:resize', ['$event'])
    drawGraph(event, series: DataProtocol.NumericSeries[] = this.series$.getValue()): void {
        this.setResponsiveMode();
        this.graphContainerWidth = this.graphContainer.nativeElement.getBoundingClientRect().width;
        this.columnWidthRatio = this.window.width <= D3ResponsiveSize.medium ? 0.045 : 0.022;
        this.columnWidth = this.graphContainerWidth * this.columnWidthRatio;
        this.innerPadding = this.columnWidth * this.innerPaddingRatio;

        this.setScales(series);
        this.subgroups = series.map(item => item.key);
        this.xSubgroupScale = d3.scaleBand()
            .domain(this.subgroups)
            .range([0, this.subgroups.length * (this.columnWidth + this.innerPadding)]);

        this.setupData(series);
        setTimeout(() => this.cdr.detectChanges());
    }

    setScales(series: DataProtocol.NumericSeries[]): void {
        const uniqueLabels = new Set<string>();
        const yScaleStartingPoint = this.window.width <= D3ResponsiveSize.medium ? 200 : this.graphContainerHeight * 0.31;

        series.forEach(serie => serie.data.forEach(item => uniqueLabels.add(item.label)));

        this.xScale = d3.scaleBand()
            .domain(Array.from(uniqueLabels))
            .range([0, this.graphContainerWidth]);

        this.yScale = d3.scaleLinear()
            .domain([0, 100])
            .range([this.graphContainerHeight, yScaleStartingPoint]);

        this.xAxis = d3.axisBottom(this.xScale);
        this.yAxis = d3.axisBottom(this.yScale);
    }

    getTextXColumnPositions(item, i, dataArr, elementRef): number {
        if (elementRef.className.baseVal.includes('segmentTitle')) {
            return this.xScale(item.label) + this.segmentTitlePaddings.left;
        }

        const centeredColumnTextWidth = elementRef.getBoundingClientRect().width,
            halfOfElementsWidth = centeredColumnTextWidth / 2,
            xOriginalPos = this.xScale(item.label);

        if (item === dataArr[dataArr.length - 1]) {
            const xEndPos = this.graphContainerWidth;
            return xOriginalPos + ((xEndPos - xOriginalPos) / 2) - halfOfElementsWidth;
        }

        const nextPos = this.xScale(dataArr[i + 1].label);

        return (xOriginalPos + (nextPos - xOriginalPos) / 2) - halfOfElementsWidth;
    }

    setupData(series: DataProtocol.NumericSeries[]): void {
        this.formattedData = [];

        series.forEach((serie, serieIndex) =>
            serie.data.forEach((elem, elemIndex) => {
                if (this.formattedData.filter(item => item['label'] === elem.label).length === 0) {
                    this.formattedData.push({label: elem.label, values: []});
                }
                const sessionPercentage = series[serieIndex].data[0].value === 0
                    ? 0
                    : Math.round((elem.value / series[serieIndex].data[0].value) * 100);
                const relativePercentage = elemIndex === 0
                    ? 100
                    : series[serieIndex].data[elemIndex - 1].value === 0
                        ? 0
                        : Math.round((elem.value / series[serieIndex].data[elemIndex - 1].value) * 100);
                this.formattedData
                    .filter(item => item['label'] === elem.label)[0]['values']
                    .push({
                            key: serie.key,
                            value: elem.value,
                            percentage: sessionPercentage,
                            relativePercentage: relativePercentage,
                            color: this.getColorByKey(serie.key),
                            borderColor: this.getBorderColor(this.getColorByKey(serie.key)),
                            name: this.options$.getValue().series.find(s => s.key === serie.key).name
                        });
            }));
            this.totalSessions = this.formattedData[0].values[0].value;
            this.conversionRate = this.formattedData[this.formattedData.length - 1].values[0].percentage;
    }

    setResponsiveMode(): void {
        this.responsiveMode =
            this.window.width >= D3ResponsiveSize.large ? 'xLarge' :
            this.window.width >= D3ResponsiveSize.medium ? 'large' :
            this.window.width >= D3ResponsiveSize.small ? 'medium' :
            this.window.width >= 0 ? 'small'
            : null;
    }

    getColorByKey(key: string): string {
        const findIndex = this.series$.getValue().findIndex(item => item.key === key);
        return findIndex === -1 ? '#000' : this.colorPalette[findIndex];
    }

    getBorderColor(color: string): string {
        const regExp = new RegExp(/^#([0-9a-f]{6})$/i);
        if (regExp.test(color)) return  '#' + (parseInt(color.substr(1), 16) - parseInt('101010', 16)).toString(16);
        else return color;
    }

    getGridPath(series): string {
        const pathValues: any = [
            [this.xScale(series.label), this.graphContainerHeight],
            [this.xScale(series.label), 0],
        ];
        return d3.line()(pathValues);
    }

    getResponsiveBarPlacement(series: any): string {
        const leftMargin = this.window.width < D3ResponsiveSize.medium ? 5 : this.xScale.bandwidth() / 1.9;
        return `translate(${(this.xScale(series.label) + leftMargin)},0)`;
    }

    getResponsivePercentageCoordinate(series: any, index: number): number {
        let coordinate;
        const offsetFromContainerHeight = this.graphContainerHeight * 0.755;
        const margin = 35;

        if (this.window.width <= D3ResponsiveSize.medium) {
            coordinate = 10 + (series.values.length - index) * margin;
        } else {
            coordinate = this.yScale(0) - index * margin;
        }
        return coordinate;
    }

    highlightOnHoverHandler(key: string) {
        if (key === this.highlightedBarKey) {
            return;
        }
        this.highlightedBarKey = key;
    }

    highlightOnLeaveHandler() {
        this.highlightedBarKey = null;
    }
}
