import { Component, ElementRef, OnDestroy, ViewChild, Input, ChangeDetectorRef, HostListener } from '@angular/core';
import { BehaviorSubject, combineLatest, Subject } from 'rxjs';
import { debounceTime, filter, takeUntil } from 'rxjs/operators';
import * as d3 from 'd3';
import {D3ResponsiveSize} from '../shared/d3-responsive-sizes';
import { WindowRefService } from '../../../core/services/window-ref.service';
import { ChartBoxExpansionService } from 'src/app/core/services/chart-box-expansion.service';
import { NumericPoint, NumericSeries } from '../../../protocol/data-protocol';
import * as VisualizationProtocol from '../../../protocol/visualization-protocol';



export interface FunnelData {
    label: string | Date;
    value: number;
    percentageOfTotal: number;
    relativePercentageOfPrev: number;
}

@Component({
    selector: 'm-d3-funnel',
    templateUrl: './d3-funnel.component.html',
    styleUrls: ['./d3-funnel.component.scss']
})

export class D3FunnelComponent implements OnDestroy {
    @ViewChild('chartContainer', {static: true}) private chartContainer: ElementRef;
    @ViewChild('inner', {static: true}) private innerContainer: ElementRef;
    @ViewChild('graphContainer', {static: true}) private graphContainer: ElementRef;
    @ViewChild('xAxis', {static: true}) private xAxisRef: ElementRef;
    @ViewChild('areaPath', {static: true}) private arePath: ElementRef;
    @ViewChild('yGrid', {static: true}) private yGrid: ElementRef;

    options$ = new BehaviorSubject<VisualizationProtocol.VisualizationFunnel | null | undefined>(null);
    @Input('options')
    set _options(value: VisualizationProtocol.VisualizationFunnel | null | undefined) {
        this.options$.next(value);
    }

    series$ = new BehaviorSubject<NumericPoint[] | null>(null);
    @Input('seriesData')
    set _seriesData(value: NumericSeries[] | null) {
        this.series$.next(value[0].data);
    }

    destroy$ = new Subject<any>();

    // SELECTIONS
    xAxisSelection: any;
    graphContainerSelection: any;
    areaPathSelection: any;
    yGridSelection: any;

    // OPTIONS
    segmentTitlePaddings = {top: 23, right: 0, bottom: 0, left: 10};
    areaPathTopMargin: number = 0;
    areaPathTopMarginRatio = 0.75;
    graphContainerWidth = 0;
    graphContainerHeight = 240;

    segmentTotalFontSize = 10;
    segmentTotalFontWeight = 'bold';
    segmentPercentageFontSize = 24;
    segmentPercentageFontWeight = 'bold';
    responsiveSegmentPercentageFontSize = 25;
    relativePercentageText = 20;

    areaPathD = '';

    textXCoordinates: number[];
    relativePerc = false;
    formattedData: FunnelData[];
    labelsData: FunnelData[];
    xScale: d3.ScaleBand<string> & d3.AxisScale<any>;
    yScale:  d3.ScaleLinear<number, number> & d3.AxisScale<any>;
    graphColor = '#ffd363';
    totalSessions: number;
    conversionRate: number;
    responsiveMode: string;

    constructor(
        private cdr: ChangeDetectorRef,
        private window: WindowRefService,
        private chartBoxExpansionService: ChartBoxExpansionService) {
        combineLatest(
            this.options$.asObservable(),
            this.series$.pipe(filter(opts => !!opts)),
        ).pipe(
            takeUntil(this.destroy$),
            debounceTime(250),
        ).subscribe(([options, series]) => {
            this.drawChart();
            setTimeout(() => this.cdr.detectChanges());
        });

        // Subscribe to chart box expansion
        this.chartBoxExpansionService.expand
        .pipe(takeUntil(this.destroy$))
        .subscribe(object => {
            setTimeout(() => {
                this.drawChart();
                this.cdr.detectChanges();
            }, object.delay);
        })
    }

    ngOnDestroy() {
        this.destroy$.next();
        this.destroy$.complete();
    }

    @HostListener('window:resize', [])
    drawChart(): void {
        this.graphContainerWidth = this.graphContainer.nativeElement.getBoundingClientRect().width;

        this.setResponsiveMode();
        this.formattedData = this.setupData(this.series$.getValue());
        this.labelsData = Object.assign([], this.formattedData);
        this.labelsData.pop();

        this.xAxisSelection = d3.select(this.xAxisRef.nativeElement);
        this.graphContainerSelection = d3.select(this.graphContainer.nativeElement);
        this.areaPathSelection = d3.select(this.arePath.nativeElement);
        this.yGridSelection = d3.select(this.yGrid.nativeElement);

        this.setScales();
        this.areaPathTopMargin = this.yScale(this.formattedData[0].value * this.areaPathTopMarginRatio);
        this.areaPathD = this.drawArea();
        setTimeout(() => this.cdr.detectChanges());
    }

    drawArea(): string {
        if (!this.formattedData) {
            return;
        }
        const areaGenerator = d3.area<FunnelData>()
            .x((d, i, n) => {
                return i === n.length - 1 ? this.graphContainerWidth : this.xScale(d['label']);
            })
            .y0(this.yScale(0) * this.areaPathTopMarginRatio)
            .y1((d, i, n) => {
                if (this.relativePerc && i !== 0) {
                    const prevVal = n[i - 1]['value'];
                    const newVal = prevVal * (d['relativePercentageOfPrev'] / 100);
                    return this.yScale(newVal) * this.areaPathTopMarginRatio;
                } else {
                    return this.yScale(d['value']) * this.areaPathTopMarginRatio;
                }
            });

        return areaGenerator(this.formattedData);
    }

    setScales(): void {
        this.xScale = d3.scaleBand()
            .domain(this.formattedData.map(item => item.label + ''))
            .range([0, this.graphContainerWidth]);
        this.yScale = d3.scaleLinear()
            .domain([0, d3.max<number>(this.formattedData.map(item => item.value))])
            .range([this.graphContainerHeight, 0]);
    }

    getTextXColumnPositions(item: FunnelData, i: number, dataArr: FunnelData[], elementRef: any): number {
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

    getGridPath(series: FunnelData): string {
        const pathValues: any = [
            [this.xScale(series.label), this.graphContainerHeight],
            [this.xScale(series.label), 0],
        ];
        return d3.line()(pathValues);
    }

    setupData(dataArr: NumericPoint[]): FunnelData[] {


        const unsortedArr = [...dataArr];

        // Sort data by sessions
        const sortedArr = unsortedArr.sort((a, b) => b.value - a.value);

        const newArr = sortedArr.map((item, i, arr) => {
            const sessionPercentage = Math.round((item.value / arr[0].value) * 100);
            const relativePercentage =
                i === 0 ? 100
                : arr[i-1].value === 0 ? 0
                : Math.round((item.value / arr[i - 1].value) * 100);
                return {
                    ...item,
                    percentageOfTotal: sessionPercentage,
                    relativePercentageOfPrev: relativePercentage
                };
        });

        this.totalSessions = d3.max<number>(dataArr.map(d => d.value));
        this.conversionRate = newArr[newArr.length - 1].percentageOfTotal;
        newArr.push(newArr[newArr.length - 1]); // duplicate last item
        return newArr;
    }
    setResponsiveMode(): void {
        this.responsiveMode =
            this.window.width >= D3ResponsiveSize.large ? 'xLarge' :
            this.window.width >= D3ResponsiveSize.medium ? 'large' :
            this.window.width >= D3ResponsiveSize.small ? 'medium' :
            this.window.width >= 0 ? 'small'
            : null;
    }

    toggleRelativePercentages(): void {
        this.relativePerc = !this.relativePerc;
        this.areaPathD = this.drawArea();
    }
}
