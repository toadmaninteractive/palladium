import * as d3 from 'd3';
import {ElementRef} from '@angular/core';
import * as VisualizationProtocol from '../../../protocol/visualization-protocol';
import {AxisValueType} from '../../../protocol/visualization-protocol';
import * as DataProtocol from '../../../protocol/data-protocol';

export class D3Basic {
    monthSet = new Set(['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']);

    axisBottom: ElementRef;
    axisTop: ElementRef;
    axisLeft: ElementRef;
    axisRight: ElementRef;

    xGrid: ElementRef;
    yGrid: ElementRef;

    axisTimelineBottom: ElementRef;
    axisTimelineLeft: ElementRef;
    timeContext: ElementRef;
    timeContextBrush: ElementRef;
    zoomContainer: ElementRef;

    margin = {top: 0, right: 0, bottom: 0, left: 0};
    leftAxisTitleTranslate = 0;
    rightAxisTitleTranslate = 0;
    bottomAxisTranslate = 0;
    padding = {top: 20, right: 0, bottom: 50, left: 0};
    plotProps = {width: 0, height: 0};
    dashLength = 10;

    colorPalette =  [
        '#6bcaff', '#ff7d85', '#ffd363', '#9edb89',
        '#b1a6fe', '#fca77d', '#86ead6', '#218fcc',
        '#ef5b64', '#dda820', '#6ac04c', '#cb7212',
        '#2fc3a5', '#0970aa', '#d83a43', '#ac7c00',
        '#54921f', '#501f76', '#a15402'];

    symbolTypes = [d3.symbolCircle, d3.symbolCross, d3.symbolDiamond, d3.symbolSquare, d3.symbolStar, d3.symbolTriangle, d3.symbolWye];

    xPrimaryScale: (d3.ScaleTime<number, number> | d3.ScaleBand<string> | d3.ScaleLinear<number, number>) & d3.AxisScale<any>;
    xSecondaryScale: (d3.ScaleTime<number, number> | d3.ScaleBand<string> | d3.ScaleLinear<number, number>) & d3.AxisScale<any>;
    yPrimaryScale: (d3.ScaleTime<number, number> | d3.ScaleBand<string> | d3.ScaleLinear<number, number>) & d3.AxisScale<any>;
    ySecondaryScale: (d3.ScaleTime<number, number> | d3.ScaleBand<string> | d3.ScaleLinear<number, number>) & d3.AxisScale<any>;

    xPrimaryAxis: d3.Axis<any>;
    yPrimaryAxis: d3.Axis<any>;
    xSecondaryAxis: d3.Axis<any>;
    ySecondaryAxis: d3.Axis<any>;

    xPrimaryValues: string[];
    yPrimaryValues: string[];
    xSecondaryValues: string[];
    ySecondaryValues: string[];

    xTimelineScale: (d3.ScaleTime<number, number> | d3.ScaleBand<string> | d3.ScaleLinear<number, number>) & d3.AxisScale<any>;
    yTimelineScale: (d3.ScaleTime<number, number> | d3.ScaleBand<string> | d3.ScaleLinear<number, number>) & d3.AxisScale<any>;
    xTimelineAxis: d3.Axis<any>;
    yTimelineAxis: d3.Axis<any>;

    pristineOptions = {} as VisualizationProtocol.VisualizationCommonPlot | VisualizationProtocol.VisualizationSynchronizedPlot;

    existBarSeries = false;
    existLineSeries = false;
    xSubgroupScale: d3.AxisScale<any>;

    zoom: any;
    brush: any;
    shiftState = false;

    dashedSeries: string[];


    constructor() {
    }

    setUniqueAxisValues(series: DataProtocol.NumericSeries[]): void {
        this.xPrimaryValues = [...new Set(series.flatMap( item => item.data.map(element => element.label.toString())))];
        this.xSecondaryValues = [];

        this.yPrimaryValues = [...new Set(
            series
                .filter(item => this.getSeriesOptions(item.key).yAxis === VisualizationProtocol.AxisKindY.Primary)
                .flatMap( item => item.data.map(element => element.value.toString())))
        ];

        this.ySecondaryValues = [...new Set(
            series
                .filter(item => this.getSeriesOptions(item.key) instanceof VisualizationProtocol.ChartSerie)
                .filter(item => this.getSeriesOptions(item.key).yAxis === VisualizationProtocol.AxisKindY.Secondary)
                .flatMap( item => item.data.map(element => element.value.toString())))
        ];
    }

    createScales(options: VisualizationProtocol.VisualizationCommonPlot | VisualizationProtocol.VisualizationSynchronizedPlot): void {
        this.xPrimaryScale = this.xPrimaryValues.length > 0
            ? this.setXScale(this.plotProps.width, this.xPrimaryValues, options.xAxisType)
            : null;

        this.xSecondaryScale = this.xSecondaryValues.length > 0
            ? this.setXScale(this.plotProps.width, this.xSecondaryValues, options.xAxisType)
            : null;

        this.yPrimaryScale = this.yPrimaryValues.length > 0
            ? this.setYScale(this.plotProps.height, this.yPrimaryValues, options.yAxisType)
            : null;

        this.ySecondaryScale = this.ySecondaryValues.length > 0
            ? this.setYScale(this.plotProps.height, this.ySecondaryValues, options.secondaryYAxisType)
            : null;
    }

    setXScale(width: number, labels: Array<string>, axisType: VisualizationProtocol.AxisValueType): d3.ScaleTime<number, number> | d3.ScaleBand<string> | d3.ScaleLinear<number, number> {
        switch (axisType) {
            case VisualizationProtocol.AxisValueType.Date:
                return d3
                    .scaleTime()
                    .range([0, width])
                    .domain(d3.extent(labels.map(item => new Date(+item))));

            case VisualizationProtocol.AxisValueType.Numeric:
                const tempLabels = labels.map(item => parseFloat(item));
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

    getXScale(value: number, labels: DataProtocol.NumericPoint[] | DataProtocol.ConfidencePoint[] , options: VisualizationProtocol.VisualizationCommonPlot | VisualizationProtocol.VisualizationSynchronizedPlot, width: number): number {
        if (options.xAxisType === VisualizationProtocol.AxisValueType.Numeric) {
            // @ts-ignore
            const tempLabels = labels.map(item => parseFloat(item.label));

            // @ts-ignore
            const xScale = d3.scaleLinear().domain([tempLabels[0], tempLabels[tempLabels.length - 1]]).range([0, width]);
            return xScale(value);
        }
    }

    setYScale(height: number, labels: Array<string>, axisType: VisualizationProtocol.AxisValueType): d3.ScaleTime<number, number> | d3.ScaleBand<string> | d3.ScaleLinear<number, number> {
        switch (axisType) {
            case VisualizationProtocol.AxisValueType.Date:
                return d3
                    .scaleTime()
                    .range([height, 0])
                    .domain(d3.extent(labels.map(item => new Date(+item))));

            case VisualizationProtocol.AxisValueType.Numeric:
                return d3
                    .scaleLinear()
                    .rangeRound([height, 0])
                    .domain([0, Math.max(...labels.map(item => parseFloat(item))) * 1.1]);

            case VisualizationProtocol.AxisValueType.Text:
                return d3
                    .scaleBand()
                    .range([height, 0])
                    .padding(0.1)
                    .domain(labels);
        }
    }

    createAxis(options: VisualizationProtocol.VisualizationCommonPlot | VisualizationProtocol.VisualizationSynchronizedPlot): void {
        if (this.xPrimaryScale) {
            this.xPrimaryAxis = d3.axisBottom(this.xPrimaryScale)
                .tickArguments([this.calculateTickCount(
                    this.xPrimaryValues.filter(date => new Date(+date) <= this.xPrimaryScale.domain()[1] && new Date(+date) >= this.xPrimaryScale.domain()[0]))])
                .tickFormat(options.xAxisType === VisualizationProtocol.AxisValueType.Date
                    ? this.tickFormat(VisualizationProtocol.AxisValueType.Date, this.xPrimaryScale)
                    : null);
        }
        if (this.xSecondaryScale) {
            this.xSecondaryAxis = d3.axisTop(this.xSecondaryScale);
        }
        if (this.yPrimaryScale) {
            this.yPrimaryAxis = d3.axisLeft(this.yPrimaryScale);
        }
        if (this.ySecondaryScale) {
            this.ySecondaryAxis = d3.axisRight(this.ySecondaryScale);
        }
    }

    addGridLines(options: VisualizationProtocol.VisualizationCommonPlot | VisualizationProtocol.VisualizationSynchronizedPlot
        | VisualizationProtocol.VisualizationHorizontalBars | VisualizationProtocol.VisualizationScatter): void {
        if (options.showXGridLine) {
            if (this.xPrimaryAxis) {
                d3.select(this.xGrid.nativeElement).call(this.xPrimaryAxis.tickArguments([this.calculateTickCount(this.xPrimaryValues) / 3]).tickSize(-this.plotProps.height));
            }
        }
        if (options.showYGridLine) {
            if (this.yPrimaryAxis) {
                d3.select(this.yGrid.nativeElement).call(this.yPrimaryAxis.tickArguments([5]).tickSize(-this.plotProps.width));
            }

            if (this.ySecondaryAxis) {
                d3.select(this.yGrid.nativeElement).call(this.ySecondaryAxis.tickArguments([5]).tickSize(this.plotProps.width));
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

    drawVerticalLine(xCoord: string | Date | null): string {
        if (!xCoord) return 'M0,0 0,0';
        let xValue = this.xPrimaryScale(xCoord);
        if (this.pristineOptions.xAxisType === VisualizationProtocol.AxisValueType.Text) {
            xValue += this.xPrimaryScale.bandwidth() / 2;
        }
        return 'M' + xValue + ',' + this.plotProps.height + ' ' + xValue + ',' + 0;
    }

    addTimelineAxis(elementRef: ElementRef, options: VisualizationProtocol.VisualizationCommonPlot | VisualizationProtocol.VisualizationSynchronizedPlot): void {
        this.xTimelineScale = d3.scaleTime().range([0, this.plotProps.width]).domain(d3.extent(this.xPrimaryValues.map(item => new Date(+item))));
        this.yTimelineScale = this.setYScale(40, this.yPrimaryValues, options.yAxisType);
        this.xTimelineAxis = d3.axisBottom(this.xTimelineScale)
            .tickArguments([this.calculateTickCount(this.xPrimaryValues)])
            .tickFormat(options.xAxisType === VisualizationProtocol.AxisValueType.Date
                ? this.tickFormat(VisualizationProtocol.AxisValueType.Date, this.xTimelineScale)
                : null);
        this.yTimelineAxis = d3.axisLeft(this.yTimelineScale);
        d3.select(elementRef.nativeElement).call(this.xTimelineAxis);
        this.addTimelineBrush();
    }

    addTimelineBrush(): void {
        this.brush = d3.brushX().extent([[0, 0], [this.plotProps.width, 40]]).on('brush end', () => this.brushed());

        d3.select(this.timeContextBrush.nativeElement)
            .selectAll('.handle--custom')
            .data(['w', 'e'])
            .enter().append('rect')
            .attr('class', d => 'handle--custom ' + 'handle--custom--' + d)
            .attr('transform', d => d === 'w' ? 'translate(0, 0)' :  'translate('  + this.plotProps.width + ', 0)')
            .attr('fill', '#555')
            .attr('rx', '2')
            .attr('ry', '2')
            .attr('cursor', 'ew-resize')
            .attr('width', 3)
            .attr('height', 40);

        d3.select(this.timeContextBrush.nativeElement)
            .selectAll('.handle--mini')
            .data(['w', 'e'])
            .enter().append('rect')
            .attr('class', d => 'handle--mini ' + 'handle--mini--' + d)
            .attr('transform', d => d === 'w' ? 'translate(-2, 12)' :  'translate('  + (this.plotProps.width - 2) + ', 12)')
            .attr('fill', 'white')
            .attr('rx', '3')
            .attr('ry', '3')
            .attr('cursor', 'ew-resize')
            .attr('stroke', '#555')
            .attr('stroke-width', '1')
            .attr('width', 7)
            .attr('height', 16);

        d3.select(this.timeContextBrush.nativeElement).call(this.brush).call(this.brush.move, this.xPrimaryScale.range());


    }

    addZoom(): void {
        this.zoom = d3.zoom()
            .scaleExtent([1, 100])
            .translateExtent([[0, 0], [this.plotProps.width, this.plotProps.height]])
            .extent([[0, 0], [this.plotProps.width, this.plotProps.height]]).on('zoom', () => this.zoomed());
        d3.select(this.zoomContainer.nativeElement).call(this.zoom);
    }

    symbolGenerator(index: number): string {
        return d3.symbol().type(this.symbolTypes[index % this.symbolTypes.length])();
    }

    brushed(): void {
    }

    zoomed(): void {
    }

    dateParse(dateString: string | Date, options: VisualizationProtocol.VisualizationCommonPlot | VisualizationProtocol.VisualizationSynchronizedPlot | null | undefined): string {
        if(options && options.xAxisType === AxisValueType.Date) {
            if (dateString instanceof Date) return dateString.getTime() + '';
            if (new Date(dateString) instanceof Date) return new Date(dateString).getTime() + '';
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
            }
        } else return dateString as string;
    }

    getMinimalTickDistance(scale: any): number {
        const ticks = scale.ticks(this.calculateTickCount(this.xPrimaryValues.filter(date => new Date(+date) <= this.xPrimaryScale.domain()[1] && new Date(+date) >= this.xPrimaryScale.domain()[0]))),
            spaces = Array<number>();
        for (let i = 0; i < ticks.length - 1; i++) {
            spaces.push(scale(ticks[i + 1]) - scale(ticks[i]));
        }
        return Math.min(...spaces);
    }

    strToTime(date: string): number {
        return new Date(date).getTime();
    }

    getSeriesOptions(key: string): VisualizationProtocol.ChartSerie {
        return this.pristineOptions.series.find(item => item.key === key);
    }


    getStrokeDasharray(key: string): string {
        if (this.dashedSeries) {
            const spaceBetweenDash = this.dashedSeries.length - 1;
            return this.dashedSeries.includes(key) ? `${this.dashLength},${this.dashLength * spaceBetweenDash}` : null;
        }
        return null;
    }

    getStrokeDashoffset(key: string): number {
        if (this.dashedSeries) {
            const position = this.dashedSeries.findIndex(seriesKey => seriesKey === key) + 1;
            return position * this.dashLength;
        }
        return null;
    }

    // getDashedSeries(series: D3SeriesDataSet[], legendMap: Map<string, D3SeriesDataSet>): string[] {
    //     return series.filter((oneSeries, i) => {
    //         const legendIsActive = legendMap.has(oneSeries.seriesKey),
    //         dataIsEqual = series.some((seriesItem, index) => {
    //             if (index === i) return false;
    //             return oneSeries.data.every((numericPoint, index2) => numericPoint.value === seriesItem.data[index2].value);
    //         });
    //         if (legendIsActive && dataIsEqual) return true;
    //     }).map(oneSeries => oneSeries.seriesKey);
    // }
}
