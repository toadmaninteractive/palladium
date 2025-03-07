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
import { D3Basic } from '../shared/d3-basic';
import { ChartBoxExpansionService } from 'src/app/core/services/chart-box-expansion.service';
import * as VisualizationProtocol from '../../../protocol/visualization-protocol';
import * as DataProtocol from '../../../protocol/data-protocol';
import * as D3CommonOptions from '../shared/d3-common-options';


@Component({
    selector: 'm-d3-plot',
    templateUrl: './d3-plot.component.html',
    styleUrls: ['./d3-plot.component.scss'],
})

export class D3PlotComponent extends D3Basic implements OnInit, AfterViewInit, OnDestroy {
    @ViewChild('chartContainer', {static: true}) private chartContainer: ElementRef;
    @ViewChild('plotContainer', {static: true}) private plotContainer: ElementRef;
    @ViewChild('barsContainer', {static: true}) private barsContainer: ElementRef;
    @ViewChild('mouseMoveRect', {static: true}) private mouseMoveRect: ElementRef;
    @ViewChild('linesContainer', {static: true}) private linesContainer: ElementRef;
    @ViewChild('axisBottom', {static: true}) axisBottom: ElementRef;
    @ViewChild('axisTop', {static: true}) axisTop: ElementRef;
    @ViewChild('axisLeft', {static: true}) axisLeft: ElementRef;
    @ViewChild('axisRight', {static: true}) axisRight: ElementRef;
    @ViewChild('xGrid', {static: true}) xGrid: ElementRef;
    @ViewChild('yGrid', {static: true}) yGrid: ElementRef;
    @ViewChild('axisContextBottom', {static: true}) axisTimelineBottom: ElementRef;
    @ViewChild('axisContextLeft', {static: true}) axisTimelineLeft: ElementRef;
    @ViewChild('timecontext') timeContext: ElementRef;
    @ViewChild('timecontextLine') timeContextLine: ElementRef;
    @ViewChild('timecontextBrush') timeContextBrush: ElementRef;
    @ViewChild('zoomContainer') zoomContainer: ElementRef;
    @ViewChild('measureCanvas') private canvas: ElementRef;
    @ViewChild('tooltipContainer') private tooltipRef: ElementRef;
    @ViewChild('tooltipContainerBar') private tooltipRefBar: ElementRef;

    @Input('options')
    set _options(value: VisualizationProtocol.VisualizationCommonPlot | null | undefined) {
        this.options$.next(value);
    }

    options$ = new BehaviorSubject<VisualizationProtocol.VisualizationCommonPlot | null | undefined>(null);

    @Input('data')
    set _seriesData(value: DataProtocol.NumericSeries[] | DataProtocol.ConfidenceSeries[] | null) {
        /*
        let arrData : DataProtocol.ConfidenceSeries[] = [];
        // @ts-ignore
        arrData = value.map( item => {
            return {
                data: item.data.map( i => {
                    return {
                        label: i.label,
                        value: i.value,
                        tValue: 1,
                        stdError: 10 * this.randomizeFloat(1,6),
                    } as DataProtocol.ConfidencePoint;
                } ),
                key: item.key,
                kind: item.kind,
            } as DataProtocol.ConfidenceSeries;
        });

        this.series$.next(arrData);
         */
        this.series$.next(value);
    }

    series$ = new BehaviorSubject<DataProtocol.NumericSeries[] | DataProtocol.ConfidenceSeries[] | null>([]);

    @Input() uniqueId = '0';

    destroy$ = new Subject<any>();
    tooltipXCoordinate$ = new BehaviorSubject<string>(null);

    maxLabelYPrimaryWidth = 0;
    maxLabelYSecondaryWidth = 0;
    fontSize = 14;
    marginBetweenAxisLabelsAndTitle = 15;
    rotationDegree = -20;

    tooltipPosition = {left: 0, top: 0};
    tooltipContent = '';
    tooltipHeader = '';
    estimate = [];
    estimateCoords = [];

    context: CanvasRenderingContext2D;
    legendSeriesMap = new Map<string, DataProtocol.NumericSeries | DataProtocol.ConfidenceSeries>();
    drawingSeries = new Array<DataProtocol.NumericSeries | DataProtocol.ConfidenceSeries>();
    formattedBarSeries = [];
    selectedSeriesKey = false;

    axisTypeEnum = VisualizationProtocol.AxisValueType;

    @HostListener('document:keydown.shift') shiftPushed() {
        this.shiftState = true;
    }

    @HostListener('document:keyup.shift') shiftReleased() {
        this.shiftState = false;
    }

    constructor(
        private cdr: ChangeDetectorRef,
        private chartBoxExpansionService: ChartBoxExpansionService) {
        super();
        combineLatest(
            this.options$.asObservable(),
            this.series$.pipe(filter(opts => !!opts)),
        ).pipe(
            takeUntil(this.destroy$),
            debounceTime(250),
        ).subscribe(([options, series]) => {
            this.pristineOptions = options;

            //@ts-ignore
            this.drawingSeries = series.map(
                oneSeries => {
                    const modifiedSeries = oneSeries;
                    modifiedSeries.data.map(
                        item => {
                            const modifiedItem = item;
                            modifiedItem.label = this.dateParse(item.label, options);
                            return modifiedItem;
                        });
                    return modifiedSeries;
                });

            this.drawingSeries.forEach(sds => this.legendSeriesMap.set(sds.key, sds));
            this.existBarSeries = this.drawingSeries.filter(elem => this.getSeriesOptions(elem.key).chartType === VisualizationProtocol.ChartType.VerticalBar).length > 0;
            this.existLineSeries = this.drawingSeries.filter(elem => this.getSeriesOptions(elem.key).chartType !== VisualizationProtocol.ChartType.VerticalBar).length > 0;
            if (this.existBarSeries) this.formattedBarSeries = this.convertBarData(this.drawingSeries.filter(elem => this.getSeriesOptions(elem.key).chartType === VisualizationProtocol.ChartType.VerticalBar));
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

    drawChart(series: DataProtocol.NumericSeries[] | DataProtocol.ConfidenceSeries[], options: VisualizationProtocol.VisualizationCommonPlot | VisualizationProtocol.VisualizationSynchronizedPlot): void {
        d3.select(this.plotContainer.nativeElement).attr('clip-path', 'url(#clip' + this.uniqueId + ')');
        this.setUniqueAxisValues(series);
        this.context.font = this.fontSize + 'px Ubuntu';

        this.maxLabelYPrimaryWidth = this.marginBetweenAxisLabelsAndTitle + this.getMaxWidth(this.yPrimaryValues, options.yAxisType);
        this.maxLabelYSecondaryWidth = this.marginBetweenAxisLabelsAndTitle + this.getMaxWidth(this.ySecondaryValues, options.yAxisType);

        this.padding.left = 0;
        this.padding.left += this.maxLabelYPrimaryWidth !== -Infinity ? this.maxLabelYPrimaryWidth : 10;
        this.padding.left += options.yAxisTitle ? this.fontSize : 0;

        this.padding.right = 0;
        this.padding.right += this.maxLabelYSecondaryWidth !== -Infinity ? this.maxLabelYSecondaryWidth : 10;
        this.padding.right += options.secondaryYAxisTitle ? this.fontSize : 0;

        this.padding.bottom = options.xAxisTitle ? 50 : 30;
        this.padding.bottom += this.needRotateAxisLabel()
            ? this.getMaxWidth(this.xPrimaryValues, options.xAxisType) * Math.cos(this.rotationDegree * Math.PI / 180)
            : 0;
        const width = this.chartContainer.nativeElement.getBoundingClientRect().width - this.padding.left - this.padding.right,
            height = this.chartContainer.nativeElement.getBoundingClientRect().height - this.padding.top - this.padding.bottom;

        this.plotProps.width = width !== Infinity ? width : 0;
        this.plotProps.height = height !== Infinity ? height : 0;

        this.leftAxisTitleTranslate = (this.plotProps.height + this.context.measureText(options.yAxisTitle).width) / 2;
        this.rightAxisTitleTranslate = (this.plotProps.height - this.context.measureText(options.secondaryYAxisTitle).width) / 2;

        this.bottomAxisTranslate = (this.plotProps.width - this.context.measureText(options.xAxisTitle).width) / 2;

        this.createScales(options);
        this.createAxis(options);
        this.appendAxis();
        this.addGridLines(options);
        this.addMouseMoveRect();
        this.setEstimate();

        if (this.existBarSeries) {
            this.createSubgroupScale();
            this.drawBars();
        }

        setTimeout(() => this.commonDraw(true, series));


        if (options.showTimeline) {
            this.addZoom();
            this.addTimelineAxis(this.axisTimelineBottom, options);
            d3.select(this.axisTimelineBottom.nativeElement)
                .selectAll('text')
                .attr('transform', this.needRotateAxisLabel() ? 'rotate(' + this.rotationDegree + ')' : null)
                .style('text-anchor', this.needRotateAxisLabel() ? 'end' : 'middle');
            this.drawTimeline(series);
        }
    }

    brushed(): void {
        if (d3.event && d3.event.sourceEvent && d3.event.sourceEvent.type === 'zoom') return; // ignore brush-by-zoom

        // Rescale Axis
        const s = d3.event ? d3.event.selection || this.xTimelineScale.range() : this.xTimelineScale.range();
        // @ts-ignore
        this.xPrimaryScale.domain(s.map(this.xTimelineScale.invert, this.xTimelineScale));
        this.createAxis(this.pristineOptions);

        d3.select(this.axisBottom.nativeElement)
            .call(this.xPrimaryAxis)
            .selectAll('text')
            .attr('transform', this.needRotateAxisLabel() ? 'rotate(' + this.rotationDegree + ')' : null)
            .style('text-anchor', this.needRotateAxisLabel() ? 'end' : 'middle');

        d3.select(this.timeContextBrush.nativeElement)
            .selectAll('.handle--custom')
            .attr('transform', d => d === 'w' ? 'translate(' + s[0] + ', 0)' : 'translate(' + s[1] + ', 0)');

        d3.select(this.timeContextBrush.nativeElement)
            .selectAll('.handle--mini')
            .attr('transform', d => d === 'w' ? 'translate(' + (s[0] - 2) + ', 12)' : 'translate(' + (s[1] - 2) + ', 12)');

        // redraw data
        this.commonDraw(false, this.drawingSeries.filter(item => this.legendSeriesMap.has(item.key)));
        // translate zoom
        d3.select(this.zoomContainer.nativeElement).call(this.zoom.transform, d3.zoomIdentity
            .scale(this.plotProps.width / (s[1] - s[0]))
            .translate(-s[0], 0));
    }

    zoomed(): void {
        if (!this.shiftState) {
            return;
        } else {
            if (d3.event.sourceEvent && d3.event.sourceEvent.type === 'brush') return; // ignore zoom-by-brush
            const t = d3.event.transform;
            this.xPrimaryScale.domain(t.rescaleX(this.xTimelineScale).domain());
            // redraw data
            this.commonDraw(false, this.drawingSeries.filter(item => this.legendSeriesMap.has(item.key)));
            // d3.select(this.chartContainer.nativeElement).call(xAxis);
            this.createAxis(this.pristineOptions);
            d3.select(this.axisBottom.nativeElement)
                .call(this.xPrimaryAxis)
                .selectAll('text')
                .attr('transform', this.needRotateAxisLabel() ? 'rotate(' + this.rotationDegree + ')' : null)
                .style('text-anchor', this.needRotateAxisLabel() ? 'end' : 'middle');
            d3.select(this.timeContextBrush.nativeElement).call(this.brush.move, this.xPrimaryScale.range().map(t.invertX, t)).selectAll('rect').attr('transform', 'translate(' + this.padding.left + ',0)');
        }
    }

    addMouseMoveRect(): void {
        d3.select(this.zoomContainer.nativeElement)
            .style('fill', 'none')
            .style('pointer-events', 'all')
            .attr('width', this.plotProps.width)
            .attr('height', this.plotProps.height)
            .on('mousemove', () => this.showTooltipLine())
            .on('mouseleave', () => this.hideTooltipLine());
    }

    appendAxis(): void {
        if (this.xPrimaryAxis) {
            d3.select(this.axisBottom.nativeElement).transition().duration(800).call(this.xPrimaryAxis)
                .selectAll('text')
                .attr('transform', this.needRotateAxisLabel() ? 'rotate(' + this.rotationDegree + ')' : null)
                .style('text-anchor', this.needRotateAxisLabel() ? 'end' : 'middle');
        }
        if (this.xSecondaryAxis) d3.select(this.axisTop.nativeElement).transition().duration(800).call(this.xSecondaryAxis);
        if (this.yPrimaryAxis) d3.select(this.axisLeft.nativeElement).transition().duration(800).call(this.yPrimaryAxis);
        if (this.ySecondaryAxis) d3.select(this.axisRight.nativeElement).transition().duration(800).call(this.ySecondaryAxis);
    }

    createSubgroupScale(): void {
        const subgroups = this.drawingSeries.filter(elem => this.getSeriesOptions(elem.key).chartType === VisualizationProtocol.ChartType.VerticalBar).map(item => item.key);
        subgroups.length > 1
            ? this.xSubgroupScale = d3.scaleBand()
                .domain(subgroups)
                .range([0, this.xPrimaryScale.bandwidth()])
                .paddingInner(.25)
                .paddingOuter(.3)
            : this.xSubgroupScale = d3.scaleBand()
                .domain(subgroups)
                .range([0, this.xPrimaryScale.bandwidth()])
                .paddingOuter(.25);
    }

    commonDraw(animate: boolean, series: DataProtocol.NumericSeries[] | DataProtocol.ConfidenceSeries): void {
        d3.select(this.linesContainer.nativeElement).selectAll('.dots').remove();
        d3.select(this.linesContainer.nativeElement).selectAll('.dots-bg').remove();
        d3.select(this.linesContainer.nativeElement).selectAll('path').remove();

        const typeArray = Object.keys(VisualizationProtocol.ChartType).filter(key => /^[0-9]+$/.test(key)).map(item => +item);
        typeArray.filter(type => type !== VisualizationProtocol.ChartType.VerticalBar).forEach(type => {
            let gSelection, path, area;
            //@ts-ignore
            const filteredSeries = series.filter(serie => this.getSeriesOptions(serie.key).chartType === type);
            if (animate && filteredSeries.length > 0) {
                gSelection = d3.select(this.linesContainer.nativeElement)
                    .append('g')
                    .attr('class', VisualizationProtocol.ChartType[type]);
                switch (type) {
                    case VisualizationProtocol.ChartType.Line:
                    case VisualizationProtocol.ChartType.Spline:
                        path = gSelection
                            .selectAll('.spline-line')
                            .data(filteredSeries)
                            .enter()
                            .append('path')
                            .attr('class', d => d.key + ' spline-line')
                            .attr('d', d => this.drawLine(d.data, this.getSeriesOptions(d.key).yAxis, true, type === VisualizationProtocol.ChartType.Spline))
                            .attr('fill', 'none')
                            .attr('stroke', d => this.getColorByKey(d.key))
                            .attr('stroke-width', 2)
                            .style('opacity', 1)
                            .on('mousemove', () => this.showTooltipLine())
                            .on('mouseleave', () => this.hideTooltipLine());

                        // Add animation path
                        path.each(function (d) {
                            d.totalLength = this.getTotalLength();
                        })
                            .attr('stroke-dasharray', d => d.totalLength + ' ' + d.totalLength)
                            .attr('stroke-dashoffset', d => d.totalLength)
                            .transition()
                            .duration(2000)
                            .attr('stroke-dashoffset', 0);
                        break;

                    case VisualizationProtocol.ChartType.Area:
                    case VisualizationProtocol.ChartType.AreaSpline:
                        area = gSelection
                            .selectAll('.areaFill')
                            .data(filteredSeries)
                            .enter()
                            .append('path')
                            .attr('class', d => d.key + ' areaFill')
                            .attr('fill', d => this.getColorByKey(d.key))
                            .attr('stroke', d => this.getColorByKey(d.key))
                            .attr('stroke-width', 4)
                            .style('opacity', .2)
                            .attr('d', d => this.drawArea(d.data, this.getSeriesOptions(d.key).yAxis, false, type === VisualizationProtocol.ChartType.AreaSpline))
                            .on('mousemove', () => this.showTooltipLine())
                            .on('mouseleave', () => this.hideTooltipLine())
                            .transition()
                            .duration(2000)
                            .attr('d', d => this.drawArea(d.data, this.getSeriesOptions(d.key).yAxis, true, type === VisualizationProtocol.ChartType.AreaSpline));

                        path = gSelection
                            .selectAll('.areaContour')
                            .data(filteredSeries)
                            .enter()
                            .append('path')
                            .attr('class', d => d.key + ' areaContour')
                            .attr('fill', 'none')
                            .attr('stroke', d => this.getColorByKey(d.key))
                            .attr('stroke-width', 2)
                            .style('opacity', 1)
                            .attr('d', d => this.drawLine(d.data, this.getSeriesOptions(d.key).yAxis, true, type === VisualizationProtocol.ChartType.AreaSpline))
                            .transition()
                            .duration(2000)
                            .attr('d', d => this.drawLine(d.data, this.getSeriesOptions(d.key).yAxis, true, type === VisualizationProtocol.ChartType.AreaSpline));
                        break;
                    case VisualizationProtocol.ChartType.Confidence:
                        area = gSelection
                            .selectAll('.areaFill')
                            .data(filteredSeries)
                            .enter()
                            .append('path')
                            .attr('class', d => d.key + ' areaFill')
                            .attr('fill', d => this.getColorByKey(d.key))
                            .attr('stroke', d => this.getColorByKey(d.key))
                            .attr('stroke-width', 4)
                            .style('opacity', .2)
                            .attr('d', d => this.drawConfidenceArea(d.data, this.getSeriesOptions(d.key).yAxis, false, type !== VisualizationProtocol.ChartType.Confidence))
                            .on('mousemove', () => this.showTooltipLine())
                            .on('mouseleave', () => this.hideTooltipLine())
                            .transition()
                            .duration(2000)
                            .attr('d', d => this.drawConfidenceArea(d.data, this.getSeriesOptions(d.key).yAxis, true, type !== VisualizationProtocol.ChartType.Confidence));

                        path = gSelection
                            .selectAll('.spline-line')
                            .data(filteredSeries)
                            .enter()
                            .append('path')
                            .attr('class', d => d.key + ' spline-line')
                            .attr('d', d => this.drawLineConfidence(d.data, this.getSeriesOptions(d.key).yAxis, true, type !== VisualizationProtocol.ChartType.Confidence))
                            .attr('fill', 'none')
                            .attr('stroke', d => this.getColorByKey(d.key))
                            .attr('stroke-width', 2)
                            .style('opacity', 1)
                            .on('mousemove', () => this.showTooltipLine())
                            .on('mouseleave', () => this.hideTooltipLine());

                        // Add animation path
                        path.each(function (d) {
                            d.totalLength = this.getTotalLength();
                        })
                            .attr('stroke-dasharray', d => d.totalLength + ' ' + d.totalLength)
                            .attr('stroke-dashoffset', d => d.totalLength)
                            .transition()
                            .duration(2000)
                            .attr('stroke-dashoffset', 0);

                        break;
                    default:
                        break;
                }
            } else if (filteredSeries.length > 0) {
                gSelection = d3.select(this.linesContainer.nativeElement).select('.' + VisualizationProtocol.ChartType[type]);

                switch (type) {
                    case VisualizationProtocol.ChartType.Line:
                    case  VisualizationProtocol.ChartType.Spline:
                        path = gSelection
                            .selectAll('.spline-line')
                            .data(filteredSeries)
                            .enter()
                            .append('path')
                            .attr('d', d => this.drawLine(d.data, this.getSeriesOptions(d.key).yAxis, true, type === VisualizationProtocol.ChartType.Spline))
                            .attr('class', d => d.key + ' spline-line')
                            .attr('fill', 'none')
                            .attr('stroke', d => this.getColorByKey(d.key))
                            .attr('stroke-width', 2)
                            .attr('stroke-dashoffset', 0)
                            .attr('stroke-dasharray', d => this.getStrokeDasharray(d.key))
                            .attr('stroke-dashoffset', d => this.getStrokeDashoffset(d.key))
                            .on('mousemove', () => this.showTooltipLine())
                            .on('mouseleave', () => this.hideTooltipLine());
                        break;

                    case VisualizationProtocol.ChartType.Area:
                    case VisualizationProtocol.ChartType.AreaSpline:
                        area = gSelection
                            .selectAll('.areaFill')
                            .data(filteredSeries)
                            .enter()
                            .append('path')
                            .attr('class', d => d.key + ' areaFill')
                            .attr('fill', d => this.getColorByKey(d.key))
                            .attr('stroke', d => this.getColorByKey(d.key))
                            .attr('stroke-width', 4)
                            .style('opacity', .2)
                            .attr('d', d => this.drawArea(d.data, this.getSeriesOptions(d.key).yAxis, true, type === VisualizationProtocol.ChartType.AreaSpline))
                            .on('mousemove', () => this.showTooltipLine())
                            .on('mouseleave', () => this.hideTooltipLine());

                        path = gSelection
                            .selectAll('.areaContour')
                            .data(filteredSeries)
                            .enter()
                            .append('path')
                            .attr('class', d => d.key + ' areaContour')
                            .attr('fill', 'none')
                            .attr('stroke', d => this.getColorByKey(d.key))
                            .attr('stroke-width', 2)
                            .attr('stroke-dasharray', d => this.getStrokeDasharray(d.key))
                            .attr('stroke-dashoffset', d => this.getStrokeDashoffset(d.key))
                            .style('opacity', 1)
                            .attr('d', d => this.drawLine(d.data, this.getSeriesOptions(d.key).yAxis, true, type === VisualizationProtocol.ChartType.AreaSpline));
                        break;
                    default:
                        break;
                }
            }
            filteredSeries
                // .filter(oneSerie => oneSerie.addDots)
                .forEach(oneSerie => {
                    const dotsBackgroundContainer = d3.select(this.linesContainer.nativeElement)
                        .append('g')
                        .classed('dots-bg ' + oneSerie.key, true)
                        .selectAll('circle')
                        .data(oneSerie.data)
                        .enter()
                        .append('circle')
                        .classed('dot-line-background', true)
                        .attr('fill', d => this.getColorByKey(oneSerie.key))
                        // .attr('cx', d => this.getSeriesOptions(oneSerie.key).chartType === D3AxisPosition.Primary ? this.xPrimaryScale(d.label) : this.xSecondaryScale(d.label))
                        //@ts-ignore
                        .attr('cx', d => this.xPrimaryScale(d.label))
                        //@ts-ignore
                        .attr('cy', d => this.getSeriesOptions(oneSerie.key).yAxis === VisualizationProtocol.AxisKindY.Primary ? this.yPrimaryScale(d.value) : this.ySecondaryScale(d.value))
                        .attr('r', 8)
                        .attr('fill-opacity', 0);
                    const dotsContainer = d3.select(this.linesContainer.nativeElement)
                        .append('g')
                        .classed('dots ' + oneSerie.key, true)
                        .selectAll('circle')
                        .data(oneSerie.data)
                        .enter()
                        .append('circle')
                        .classed('dot-line', true)
                        .attr('fill', d => this.getColorByKey(oneSerie.key))
                        //@ts-ignore
                        .attr('cx', d => this.xPrimaryScale(d.label))
                        // .attr('cx', d => oneSerie.xAxisPosition === D3AxisPosition.Primary ? this.xPrimaryScale(d.label) : this.xSecondaryScale(d.label))
                        //@ts-ignore
                        .attr('cy', d => this.getSeriesOptions(oneSerie.key).yAxis === VisualizationProtocol.AxisKindY.Primary ? this.yPrimaryScale(d.value) : this.ySecondaryScale(d.value))
                        .attr('r', 2)
                        .attr('stroke', 'white')
                        .attr('stroke-width', 0.1);
                });
        });
    }

    drawTimeline(series: DataProtocol.NumericSeries[]): void {
        if (!series) return;
        d3.select(this.timeContextLine.nativeElement)
            .selectAll('path').remove();
        d3.select(this.timeContextLine.nativeElement)
            .selectAll('path')
            .data(series)
            .enter()
            .append('path')
            .attr('class', d => d.key + ' timeline')
            .attr('d', (d, i) => this.drawContextLine(d.data, i))
            .attr('fill', 'none')
            .attr('stroke', d => this.getColorByKey(d.key))
            .attr('stroke-width', 2)
            .style('opacity', 1);
    }

    drawLine(data: DataProtocol.NumericPoint[], yAxisPosition: VisualizationProtocol.AxisKindY, flag: boolean = true, curved: boolean = false) {
        if (!data) return;
        // const xAxisScale = xAxisPosition === D3CommonOptions.D3AxisPosition.Primary ? this.xPrimaryScale : this.xSecondaryScale,
        const xAxisScale = this.xPrimaryScale,
            yAxisScale = yAxisPosition === VisualizationProtocol.AxisKindY.Primary ? this.yPrimaryScale : this.ySecondaryScale;
        const line = d3.line<DataProtocol.NumericPoint>()
            .curve(curved ? d3.curveMonotoneX : d3.curveLinear)
            .x(d => xAxisScale(d.label))
            .y(d => flag ? yAxisScale(d.value) : yAxisScale(0));
        return line(data);
    }

    drawContextLine(data: DataProtocol.NumericPoint[], index: number) {
        const line = d3.line<DataProtocol.NumericPoint>()
            .x(d => this.xTimelineScale(d.label))
            .y(d => this.yTimelineScale(d.value));
        return line(data);
    }

    drawArea(data: DataProtocol.NumericPoint[], yAxisPosition: VisualizationProtocol.AxisKindY, flag: boolean, curved: boolean = false) {
        const xAxisScale = this.xPrimaryScale,
            yAxisScale = yAxisPosition === VisualizationProtocol.AxisKindY.Primary ? this.yPrimaryScale : this.ySecondaryScale,
            line = d3.area<DataProtocol.NumericPoint>()
                .curve(curved ? d3.curveMonotoneX : d3.curveLinear)
                .x(d => xAxisScale(d.label))
                .y(yAxisScale(0))
                .y1(d => flag ? yAxisScale(d.value) : yAxisScale(0));
        return line(data);
    }

    drawLineConfidence(data: DataProtocol.ConfidencePoint[], yAxisPosition: VisualizationProtocol.AxisKindY, flag: boolean = true, curved: boolean = false) {
        if (!data) return;
        const xAxisScale = this.xPrimaryScale,
            yAxisScale = yAxisPosition === VisualizationProtocol.AxisKindY.Primary ? this.yPrimaryScale : this.ySecondaryScale,
            line = d3.line<DataProtocol.ConfidencePoint>()
                .curve(curved ? d3.curveMonotoneX : d3.curveLinear)
                .x(d => xAxisScale(d.label))
                .y(d => flag ? yAxisScale(d.value) : yAxisScale(0));
        return line(data);
    }

    drawConfidenceArea(data: DataProtocol.ConfidencePoint[], yAxisPosition: VisualizationProtocol.AxisKindY, flag: boolean, curved: boolean = false) {
        const xAxisScale = this.xPrimaryScale,
            yAxisScale = yAxisPosition === VisualizationProtocol.AxisKindY.Primary ? this.yPrimaryScale : this.ySecondaryScale,
            area = d3.area<DataProtocol.ConfidencePoint>()
                .curve(curved ? d3.curveMonotoneX : d3.curveLinear)
                .x(d => xAxisScale(d.label))
                .y(d => flag ? yAxisScale(d.value) - ((d.stdError ? d.stdError : 1) * (d.tValue ? d.tValue : 1)) : yAxisScale(0))
                .y1(d => flag ? yAxisScale(d.value) + ((d.stdError ? d.stdError : 1) * (d.tValue ? d.tValue : 1)) : yAxisScale(0));
        return area(data);
    }

    drawBars(): void {
        // Bar Groups - Note: Keep General Update Pattern
        // 1. Join data
        const barGroupSelection = d3.select(this.barsContainer.nativeElement)
            .selectAll<SVGGElement, any>('g')
            .data(this.formattedBarSeries, d => d.label);

        // 2. Exit old elements
        barGroupSelection.exit().remove();

        // 3. Update old elements present in new data
        // 4. Enter new elements present in new data
        const barGroupSelectionEnter = barGroupSelection.enter()
            .append<SVGGElement>('g')
            .merge(barGroupSelection)
            .attr('transform', d => 'translate(' + this.xPrimaryScale(d.label) + ', 0)');

        // Bars - Note: Keep General Update Pattern
        // 1. Join data
        const barSelection = barGroupSelectionEnter
            .merge(barGroupSelection)
            .selectAll<SVGRectElement, any>('rect')
            .data<D3CommonOptions.D3BarPoint>(d => d.values);

        // 2. Exit old elements
        barSelection.exit().remove();

        // 3. Update old elements present in new data
        // 4. Enter new elements present in new data
        barSelection.enter()
            .append<SVGRectElement>('rect')
            .merge(barSelection)
            .attr('class', d => d.key + ' v-bars')
            .attr('x', d => this.xSubgroupScale(d.key))
            .attr('width', this.xSubgroupScale.bandwidth())
            .attr('fill', d => this.getColorByKey(d.key))
            .attr('y', d => this.plotProps.height)
            .attr('height', this.yPrimaryScale(0))
            .on('mouseover', d => this.showTooltipRect(d))
            .on('mouseleave', d => this.hideTooltipRect())
            .transition()
            .duration(800)
            .ease(d3.easeQuad)
            .attr('y', d => this.yPrimaryScale(d.value) === this.plotProps.height ? (this.plotProps.height - 1) : this.yPrimaryScale(d.value))
            .attr('height', d => (this.plotProps.height - this.yPrimaryScale(d.value)) === 0 ? 1 : (this.plotProps.height - this.yPrimaryScale(d.value)));
    }

    convertBarData(series: DataProtocol.NumericSeries[]): D3CommonOptions.D3BarSeries[] {
        const formattedData = [];
        series.forEach(serie =>
            serie.data.forEach((elem, elemIndex) => {
                if (formattedData.filter(item => item['label'] === elem.label).length === 0) {
                    formattedData.push({label: elem.label, values: []} as D3CommonOptions.D3BarSeries);
                }
                formattedData
                    .filter(item => item['label'] === elem.label)[0]['values']
                    .push({
                        key: serie.key,
                        value: elem.value,
                    } as D3CommonOptions.D3BarPoint);
            }));
        return formattedData;
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

    showTooltipLine(): void {
        if (this.existLineSeries) {
            // @ts-ignore
            const xCoordinate = d3.mouse(d3.event.target)[0],
                xPrimaryValues = this.drawingSeries
                    // .filter(item => item.xAxisPosition === D3AxisPosition.Primary)
                    .reduce((acc, val) => acc.concat(val.data), [])
                    .map(np => np.label);

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
                tooltipContentPage = this.tooltipRef.nativeElement.getBoundingClientRect(),
                key = this.getNearestSeriesKey(d3.mouse(d3.event.target)[1], needleX),
                needleSeries = this.drawingSeries.find(item => item.key === key),
                needleValue = needleSeries.data.find(elem => {
                    if (this.pristineOptions.xAxisType === VisualizationProtocol.AxisValueType.Date) return new Date(+elem.label).getTime() === new Date(+needleX).getTime();
                    else return elem.label === needleX;
                }).value,
                y = this.getSeriesOptions(needleSeries.key).yAxis === VisualizationProtocol.AxisKindY.Primary ? this.yPrimaryScale(needleValue) : this.ySecondaryScale(needleValue);

            this.tooltipPosition.left = needleXCoord + this.padding.left - tooltipContentPage.width / 2
                + (this.pristineOptions.xAxisType === VisualizationProtocol.AxisValueType.Text ? this.xPrimaryScale.bandwidth() / 2 : 0);
            this.tooltipPosition.top = y - tooltipContentPage.height + 10;
            this.cdr.detectChanges();
            this.tooltipXCoordinate$.next(needleX);

            d3.select(this.linesContainer.nativeElement).selectAll<SVGPathElement, any>('path')
                // @ts-ignore
                .filter(function () {
                    if (this.classList.contains(needleSeries.key)) {
                        this.setAttribute('style', 'opacity:1');
                        if (this.classList.contains('areaFill')) {
                            this.setAttribute('style', 'opacity:0.5');
                        }
                    } else {
                        this.setAttribute('style', 'opacity:0.2');
                    }
                });

            d3.select(this.linesContainer.nativeElement).selectAll<SVGGElement, any>('.dots-bg')
                .selectAll('circle')
                // @ts-ignore
                .attr<NumericPoint>('fill-opacity', 0);

            d3.select<SVGGElement, any>('.dots-bg.' + needleSeries.key)
                .selectAll('circle')
                // @ts-ignore
                .attr<NumericPoint>('fill-opacity', d => {
                    if (d.label instanceof Date && needleX instanceof Date) return d.label.getTime() === needleX.getTime() ? 0.25 : 0;
                    else return d.label === needleX ? 0.25 : 0;
                });
            d3.select(this.linesContainer.nativeElement).selectAll<SVGGElement, any>('.dots')
                // @ts-ignore
                .filter(function () {
                    if (this.classList.contains(needleSeries.key)) {
                        d3.select(this).selectAll('circle').attr('opacity', 1)
                            // @ts-ignore
                            .attr<NumericPoint>('r', d => {
                                if (d.label instanceof Date && needleX instanceof Date) return d.label.getTime() === needleX.getTime() ? 4 : 2;
                                else return d.label === needleX ? 4 : 2;
                            });
                    } else {
                        d3.select(this).selectAll('circle').attr('opacity', 0.2).attr('r', 2);
                    }
                });
        }
    }

    showTooltipRect(d) {
        // if (this.options$.getValue().showTooltip && this.existBarSeries) {
        if (this.existBarSeries) {
            const coordsElem = d3.event.target.getBoundingClientRect();
            this.selectedSeriesKey = true;
            const needleSeria = this.drawingSeries.filter(series => series.key === d.key)[0];
            this.tooltipContent = '';
            if (this.getSeriesOptions(needleSeria.key).name) {
                this.tooltipContent = this.getSeriesOptions(needleSeria.key).name + ': <strong>' + this.formatValue(d.value) + '</strong>';
            } else this.tooltipContent += this.formatValue(d.value) + '';
            this.cdr.detectChanges();
            d3.select(this.barsContainer.nativeElement).selectAll<SVGRectElement, any>('rect')
                // @ts-ignore
                .filter(function () {
                    if (this.classList.contains(needleSeria.key)) {
                        this.setAttribute('style', 'opacity:1');
                    } else {
                        this.setAttribute('style', 'opacity:0.2');
                    }
                });
            const tooltipContentPage = this.tooltipRefBar.nativeElement.getBoundingClientRect();
            this.tooltipPosition.top = coordsElem.top - tooltipContentPage.height - 6;
            this.tooltipPosition.left = coordsElem.left + coordsElem.width / 2 - tooltipContentPage.width / 2;
            this.cdr.detectChanges();
        }
    }

    hideTooltipRect(): void {
        this.selectedSeriesKey = false;
        d3.select(this.barsContainer.nativeElement).selectAll<SVGRectElement, any>('rect')
            // @ts-ignore
            .filter(function () {
                if (this.classList.contains('v-bars')) {
                    this.setAttribute('style', 'opacity:1');
                }
            });
        this.cdr.detectChanges();
    }

    hideTooltipLine(): void {
        this.tooltipXCoordinate$.next(null);
        d3.select(this.linesContainer.nativeElement).selectAll<SVGPathElement, any>('path')
            // @ts-ignore
            .filter(function (d, i) {
                if (this.classList.contains('areaContour')) {
                    this.setAttribute('style', 'opacity:1');
                }
                if (this.classList.contains('areaFill')) {
                    this.setAttribute('style', 'opacity:0.2');
                }
                if (this.classList.contains('spline-line')) {
                    this.setAttribute('style', 'opacity:1');
                }

            });
        d3.select(this.linesContainer.nativeElement).selectAll<SVGGElement, any>('.dots')
            // @ts-ignore
            .filter(function () {
                d3.select(this).selectAll('circle').attr('opacity', 1).attr('r', 2);
            });

        // d3.selectAll<SVGGElement, any>('.dots-bg')
        //     // @ts-ignore
        //     .filter(function () {
        //         d3.select(this).selectAll('circle').attr('opacity', 0);
        //     });
    }

    getNearestSeriesKey(yCoord: number, needleX: string | Date): string {
        const filteredMap = [];
        if (!this.drawingSeries.length) return null;
        this.drawingSeries
            .filter(series => this.legendSeriesMap.has(series.key))
            .map(series => series.data.filter(elem => {
                if (this.pristineOptions.xAxisType === VisualizationProtocol.AxisValueType.Date) return new Date(+elem.label).getTime() === new Date(+needleX).getTime();
                else return elem.label === needleX;
            })
                .forEach(np => filteredMap.push(
                    {
                        key: series.key,
                        value: Math.abs(yCoord - (this.getSeriesOptions(series.key).yAxis === VisualizationProtocol.AxisKindY.Primary ? this.yPrimaryScale(np.value) : this.ySecondaryScale(np.value)))
                    })));

        return filteredMap.sort((a, b) => a.value > b.value ? 1 : -1)[0].key;
    }

    private getTooltipContent(hoveredXLabel: string): string {
        if (!this.series$.getValue() || !hoveredXLabel) return '';
        this.tooltipHeader = this.pristineOptions.xAxisType === VisualizationProtocol.AxisValueType.Date ? new Date(+hoveredXLabel).toLocaleDateString('ru-RU') : hoveredXLabel;
        let result = '',
            stdError = 0,
            resMat = '',
            tValue = 0;

        this.drawingSeries.forEach((seriesData) => {
            const value = seriesData.data.filter(elem => {
                if (this.pristineOptions.xAxisType === VisualizationProtocol.AxisValueType.Date) return new Date(+elem.label).getTime() === new Date(+hoveredXLabel).getTime();
                else return elem.label === hoveredXLabel;
            })[0].value;

            if (seriesData instanceof DataProtocol.ConfidenceSeries) {
                stdError = seriesData.data.filter(elem => {
                    if (this.pristineOptions.xAxisType === VisualizationProtocol.AxisValueType.Date) return new Date(+elem.label).getTime() === new Date(+hoveredXLabel).getTime();
                    else return elem.label === hoveredXLabel;
                })[0]?.stdError;

                tValue = seriesData.data.filter(elem => {
                    if (this.pristineOptions.xAxisType === VisualizationProtocol.AxisValueType.Date) return new Date(+elem.label).getTime() === new Date(+hoveredXLabel).getTime();
                    else return elem.label === hoveredXLabel;
                })[0]?.tValue;
            }
            resMat = (stdError !== 0 && tValue !== 0) ? '<br><span style="margin: 0 3px; color:' + this.getColorByKey(seriesData.key) + '">●</span>' +
                'Upper endpoint: ' + '<b>' + ((value + (stdError * tValue)) < 10 ? Math.round((value + (stdError * tValue)) * 10) / 10 : Math.round((value + (stdError * tValue)))) + '</b>' +
                '<br><span style="margin: 0 3px; color:' + this.getColorByKey(seriesData.key) + '">●</span>' +
                'Lower endpoint: ' + '<b>' + ((value - (stdError * tValue)) < 10 ? Math.round((value - (stdError * tValue)) * 10) / 10 : Math.round((value - (stdError * tValue)))) + '</b>'
                : '';
            result +=
                '<li>' +
                '<span style="margin: 0 3px; color:' + this.getColorByKey(seriesData.key) + '">●</span>' +
                this.getSeriesOptions(seriesData.key).name + ': ' + '<b>' + (value < 10 ? Math.round(value * 10) / 10 : Math.round(value)) + '</b>' + resMat
            '</li>';
        });
        return result;
    }

    toggleSeries(key: string): void {
        if (!this.legendSeriesMap.has(key)) {
            const series = this.drawingSeries.find(item => item.key === key);
            this.legendSeriesMap.set(series.key, series);
        } else this.legendSeriesMap.delete(key);

        this.setUniqueAxisValues(this.drawingSeries.filter(item => this.legendSeriesMap.has(item.key)));

        // Change only Y Axis

        this.yPrimaryScale = this.yPrimaryScale ? this.setYScale(this.plotProps.height, this.yPrimaryValues, this.options$.getValue().yAxisType) : null;
        this.ySecondaryScale = this.ySecondaryScale ? this.setYScale(this.plotProps.height, this.ySecondaryValues, this.options$.getValue().secondaryYAxisType) : null;

        if (this.yPrimaryScale) {
            this.yPrimaryAxis = d3.axisLeft(this.yPrimaryScale);
        }
        if (this.ySecondaryScale) {
            this.ySecondaryAxis = d3.axisRight(this.ySecondaryScale);
        }
        if (this.yPrimaryAxis) d3.select(this.axisLeft.nativeElement).transition().duration(800).call(this.yPrimaryAxis);
        if (this.ySecondaryAxis) d3.select(this.axisRight.nativeElement).transition().duration(800).call(this.ySecondaryAxis);

        this.existBarSeries = this.drawingSeries.filter(elem => this.getSeriesOptions(elem.key).chartType === VisualizationProtocol.ChartType.VerticalBar && this.legendSeriesMap.has(elem.key)).length > 0;
        if (this.existBarSeries) {
            this.formattedBarSeries = this.convertBarData(this.drawingSeries.filter(
                elem => this.getSeriesOptions(elem.key).chartType === VisualizationProtocol.ChartType.VerticalBar && this.legendSeriesMap.has(elem.key)));
        }
        if (this.existBarSeries) {
            this.createSubgroupScale();
            this.drawBars();
        } else {
            this.clearBars();
        }

        this.commonDraw(false, this.drawingSeries.filter(item => this.getSeriesOptions(item.key).chartType !== VisualizationProtocol.ChartType.VerticalBar && this.legendSeriesMap.has(item.key)));
    }

    clearBars(): void {
        d3.select(this.barsContainer.nativeElement)
            .selectAll<SVGGElement, any>('g')
            .remove();
    }

    getColorByKey(key: string): string {
        const findIndex = this.drawingSeries.findIndex(item => item.key === key);
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

    setEstimate(){
        //@ts-ignore
        if (this.pristineOptions.showEstimate && this.pristineOptions.xAxisType === VisualizationProtocol.AxisValueType.Numeric && this.pristineOptions.yAxisType === VisualizationProtocol.AxisValueType.Numeric) {
            this.estimate = this.drawingSeries.map(data => {
                //@ts-ignore
                return Math.floor(data.data.map(item => item.value * item.label).reduce((acc, current) => {
                    return acc + current;
                })* 100) / 100;
            });
            this.estimateCoords = this.estimate.map((item, index) => {
                return this.getXScale(item, this.drawingSeries[index].data, this.pristineOptions, this.plotProps.width);
            })
        }

    }

    randomizeFloat(min, max) {
        if (max == null) {
            max = (min == null ? Number.MAX_VALUE : min);
            min = 0.0;
        }

        if (min >= max) {
            throw new Error("Incorrect arguments.");
        }

        return min + (max - min) * this.randomizeValue();
    }

    randomizeValue() {
        var value = (1 + 10E-16) * Math.random();

        if (value > 1.0) {
            return 1.0;
        }

        return value;
    }
}
