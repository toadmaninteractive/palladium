import { Component, Input, ChangeDetectorRef, ChangeDetectionStrategy, HostListener, ViewChild, ElementRef, OnDestroy } from '@angular/core';
import { BehaviorSubject, combineLatest, Subject } from 'rxjs';
import { debounceTime, filter, takeUntil } from 'rxjs/operators';
import { D3AbstractMapOptions } from '../shared/d3-abstract-map-options';
import { D3MapOptions } from '../shared/d3-map-options';
import * as DataProtocol from '../../../protocol/data-protocol';
import * as d3 from 'd3';

const SCALE_QUOTIENT = 1.2,
    PLACEHOLDER_IMAGE = '/assets/app/media/img/maps/placeholder_image.jpg',
    NOT_ENOUGH_DATA_IMAGE = '/assets/app/media/img/maps/not_enough_data.png';

@Component({
    selector: 'm-d3-heatmap',
    templateUrl: './d3-heatmap.component.html',
    styleUrls: ['./d3-heatmap.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class D3HeatmapComponent implements OnDestroy {
    @ViewChild('svg') private chartContainer: ElementRef;
    @ViewChild('svgbrush') private brushContainer: ElementRef;

    destroy$ = new Subject<any>();
    mapOptions$ = new BehaviorSubject<D3MapOptions | null | undefined>(null);
    @Input() isInline = false;

    @Input('mapOptions')
    set _mapOptions(value: D3MapOptions | null | undefined) {
        this.mapOptions$.next(value);
    }

    @Input('options')
    set _options(value: D3AbstractMapOptions | null | undefined) {
        this.options$.next({...this.defaultOptions, ...(value || {})});
    }

    defaultOptions: D3AbstractMapOptions = <D3AbstractMapOptions>{
        invertXAxis: false,
        invertYAxis: true,
        name: '',
        subtitle: '',
        description: '',
        zoomOutsideMap: false,
    };

    defaultMapOptions: D3MapOptions = <D3MapOptions>{};
    preZoomDefaultOptions: D3MapOptions = <D3MapOptions>{};
    options$ = new BehaviorSubject<D3AbstractMapOptions>({...this.defaultOptions});
    windowResize$ = new BehaviorSubject(false);

    @Input('data')
    set series(value: DataProtocol.HeatmapSeries | null) {
        value.data = value.data.filter(item => !(item.x === 0 && item.y === 0));
        this.series$.next(value);
    }

    series$ = new BehaviorSubject<DataProtocol.HeatmapSeries | null>(null);
    height = 650;
    width = 650;
    url = '';
    crossAirOn = false;
    zoomScale: any;
    svg: any;
    xScale: any;
    yScale: any;
    xAxisTop: any;
    yAxisRight: any;
    xAxisBottom: any;
    yAxisLeft: any;
    newXScale: any;
    newYScale: any;
    layer1: any;
    layer2: any;
    layer3: any;
    layer4: any;
    layer5: any;
    imagesvg: any;
    selectRectMap: any;
    resetMap = false;
    frequency = 23;
    radius = [7, 11, 15, 19, 23];
    currentZoomScale = 1;
    currentXPosition = 1;
    currentYPosition = 1;
    separatorColors = [];
    newData: Array<any>;

    constructor(
        private ref: ChangeDetectorRef,
    ) {
        combineLatest([
            this.options$.asObservable(),
            this.mapOptions$.asObservable(),
            this.series$.pipe(filter(opts => !!opts)),
            this.windowResize$.asObservable(),
        ]).pipe(
            takeUntil(this.destroy$),
            debounceTime(250),
        ).subscribe(([options, mapOptions, series, winresize]) => {
            const self = this;

            // If abnormalities to new max and min axis parameter
            if (!mapOptions || options.zoomOutsideMap) {
                if (!mapOptions && series.data.length === 0) {
                    this.defaultMapOptions.url = NOT_ENOUGH_DATA_IMAGE;
                }

                let minX = Number.MAX_SAFE_INTEGER,
                    minY = Number.MAX_SAFE_INTEGER,
                    maxX = Number.MIN_SAFE_INTEGER,
                    maxY = Number.MIN_SAFE_INTEGER;

                series.data.forEach(point => {
                    minX = point.x < minX ? point.x : minX;
                    minY = point.y < minY ? point.y : minY;
                    maxX = point.x > maxX ? point.x : maxX;
                    maxY = point.y > maxY ? point.y : maxY;
                });

                if (!mapOptions) {
                    this.defaultMapOptions.url = PLACEHOLDER_IMAGE;
                } else {
                    this.defaultMapOptions.url = mapOptions.url;

                    this.preZoomDefaultOptions.minX = mapOptions.minX < minX ? mapOptions.minX : minX * SCALE_QUOTIENT;
                    this.preZoomDefaultOptions.maxX = mapOptions.maxX > maxX ? mapOptions.maxX : maxX * SCALE_QUOTIENT;
                    this.preZoomDefaultOptions.minY = mapOptions.minY < minY ? mapOptions.minY : minY * SCALE_QUOTIENT;
                    this.preZoomDefaultOptions.maxY = mapOptions.maxY > maxY ? mapOptions.maxY : maxY * SCALE_QUOTIENT;

                    this.defaultMapOptions.minX = mapOptions.minX;
                    this.defaultMapOptions.minY = mapOptions.minY;
                    this.defaultMapOptions.maxX = mapOptions.maxX;
                    this.defaultMapOptions.maxY = mapOptions.maxY;
                }
            } else {
                this.defaultMapOptions.url = mapOptions.url || NOT_ENOUGH_DATA_IMAGE;

                this.defaultMapOptions.url = mapOptions.url;
                this.defaultMapOptions.minX = mapOptions.minX;
                this.defaultMapOptions.minY = mapOptions.minY;
                this.defaultMapOptions.maxX = mapOptions.maxX;
                this.defaultMapOptions.maxY = mapOptions.maxY;
            }

            // ## promise to get the current picture values width and height
            this.loadImage(this.defaultMapOptions.url)
                .then(value => {
                    self.url = this.defaultMapOptions.url;
                    self.width = 650;
                    self.height = 650;

                    // ## remove excess values beyond the size of the map
                    if (!options.zoomOutsideMap && mapOptions) {
                        series.data = series.data.filter(item => item.x >= mapOptions.minX && item.x <= mapOptions.maxX && item.y >= mapOptions.minY && item.y <= mapOptions.maxY);
                    }

                    self.clearMapAll();

                    // ## start render chart
                    self.renderChart(series, this.defaultMapOptions, options);
                    setTimeout(() => this.ref.detectChanges());

                    return self;
                });

            this.width = self.width;
            this.height = self.height;
        });
    }

    @HostListener('document:keydown', ['$event'])
    setBrushOn(event: KeyboardEvent) {
        switch (event.code) {
            case 'KeyS':
                this.brushContainer.nativeElement.setAttribute('style', 'display: block');
                break;
            case 'Escape':
                this.resetMap = true;
                this.brushed();
                break;
            case 'ControlLeft':
                this.crossAirOn = true;
                break;
        }
    }

    @HostListener('document:keyup', ['$event'])
    setBrushOff(event) {
        switch (event.code) {
            case 'KeyS':
                break;
            case 'Escape':
                this.resetMap = false;
                this.brushContainer.nativeElement.setAttribute('style', 'display: none');
                d3.select('#crossTextTooltip').remove();
                d3.select('#polygonTooltip').remove();
                this.brushend();
                break;
            case 'ControlLeft':
                this.crossAirOn = false;
                break;
        }
    }

    ngOnDestroy() {
        this.options$.unsubscribe();
        this.mapOptions$.unsubscribe();
        this.series$.unsubscribe();
        this.windowResize$.unsubscribe();
    }

    showTooltip(event: MouseEvent): void {
        const rect = this.chartContainer.nativeElement.getBoundingClientRect(),
              x = event.clientX - rect.left, // - this.padding.left,
              y = event.clientY - rect.top; // - this.padding.top;

        if (this.crossAirOn) {
            this.crossLineToolTip(x, y, null, null, null);
        } else {
            this.crossLineToolTipClear();
        }
    }

    distance(x1: number, y1: number, x2: number, y2: number): number {
        return Math.sqrt(Math.pow(x1 - x2, 2) + Math.pow(y1 - y2, 2));
    }

    hideTooltip(): void {
        this.crossLineToolTipClear();
    }

    loadImage(src: string) {
        return new Promise((resolve, reject) => {
            const imageElement = new Image();
            imageElement.addEventListener('load', () => resolve(imageElement));
            imageElement.addEventListener('error', err => reject(err));
            imageElement.src = src;
        });
    }

    renderChart(data, mapOptions: D3MapOptions, options: D3AbstractMapOptions) {
        let zoom;

        const svgElement = '.zoomableOf',
              svgImage = '.imagesvg',
              margin = {
                  top: 0,
                  bottom: 80,
                  left: 0,
                  right: 0
              };

        d3.select('.d3-expand-chart-button').attr('style', 'visibility:hidden');

        // ## position substrate image
        d3.select('.heatmap-inner-image')
            .attr('style', 'top:' + (-this.height) + 'px');

        this.imagesvg = d3.select(svgImage);

        // ## start initialization Selection
        this.selectRectMap = d3.brush().on('end', () => this.brushed());
        d3.select('.d3brushmapimage').call(this.selectRectMap);
        this.resetMap = false;
        this.brushContainer.nativeElement.setAttribute('style', 'display:none');
        this.brushend();
        // ## end initialization Selection

        // ## render chart
        if (this.imagesvg.select('.map.xy.box')['_groups'][0][0] === undefined) {
            this.imagesvg
                .append('svg:rect')
                .attr('class', 'map xy box')
                .attr('width', `${this.width}px`)
                .attr('height', `${this.height}px`)
                .attr('fill', 'url(#img1)')
                .attr('opacity', 0.6)
                .attr('transform', 'translate(0, 0) scale(1, 1)');
        }

        if (this.imagesvg.select('.innerImageTop1')['_groups'][0][0] === undefined) {
            this.imagesvg.append('g').attr('class', 'innerImageTop1');
        }

        if (this.imagesvg.select('.innerImageTop2')['_groups'][0][0] === undefined) {
            this.imagesvg.append('g').attr('class', 'innerImageTop2');
        }

        if (this.imagesvg.select('.innerImageTop3')['_groups'][0][0] === undefined) {
            this.imagesvg.append('g').attr('class', 'innerImageTop3');
        }

        if (this.imagesvg.select('.innerImageTop4')['_groups'][0][0] === undefined) {
            this.imagesvg.append('g').attr('class', 'innerImageTop4');
        }

        if (this.imagesvg.select('.innerImageTop5')['_groups'][0][0] === undefined) {
            this.imagesvg.append('g').attr('class', 'innerImageTop5');
        }

        // ##start  Draw Axis
        this.svg = d3.select(svgElement);

        const g = this.svg.append('g')
            .attr('id', 'containerMap')
            .attr('transform', `translate(${margin.left}, ${margin.top})`);

        g.append('g')
            .attr('class', 'x axis top')
            .attr('transform', `translate(0, ${this.height - 1})`);

        g.append('g')
            .attr('class', 'y axis right')
            .attr('transform', 'translate(0, 0)');

        g.append('g')
            .attr('class', 'x axis bottom')
            .attr('transform', 'translate(0, 0)');

        g.append('g')
            .attr('class', 'y axis left')
            .attr('transform', `translate(${this.width}, 0)`);

        // X axis scaling
        const scaleDomainXAxis = options.invertXAxis
            ? [mapOptions.maxX, mapOptions.minX]
            : [mapOptions.minX, mapOptions.maxX];

        this.xScale = d3.scaleLinear()
            .domain(scaleDomainXAxis)
            .range([0, this.height]);

        this.xAxisTop = d3.axisTop(this.xScale).tickFormat(() => '');
        this.xAxisBottom = d3.axisBottom(this.xScale).tickFormat(() => ''); // none label

        // Y axis scaling
        const scaleDomainYAxis = options.invertYAxis
            ? [mapOptions.maxY, mapOptions.minY]
            : [mapOptions.minY, mapOptions.maxY];

        this.yScale = d3.scaleLinear()
            .domain(scaleDomainYAxis)
            .range([this.width, 0]);

        this.yAxisRight = d3.axisRight(this.yScale).tickFormat(() => '');
        this.yAxisLeft = d3.axisLeft(this.yScale).tickFormat(() => ''); // .tickFormat(''); // none label

        this.svg.select('g.x.axis.top').call(this.xAxisTop);
        this.svg.select('g.x.axis.bottom').call(this.xAxisBottom);
        this.svg.select('g.y.axis.right').call(this.yAxisRight);
        this.svg.select('g.y.axis.left').call(this.yAxisLeft);

        this.zoomScale = { xScale: this.xScale.domain(), yScale: this.yScale.domain() };

        // ##end Draw Axis
        g.append('svg:rect')
            .attr('class', 'zoom xy box')
            .attr('width', this.width)
            .attr('height', this.height)
            .style('visibility', 'hidden')
            .attr('pointer-events', 'all');

        // ## converter and count data Point
        this.newData = data.data.map(point => {
            return {
                killerX: Math.round(point.x),
                killerY: Math.round(point.y),
                viewBoxX: this.xScale(point.x),
                viewBoxY: this.yScale(point.y),
                distanceAllPoint: (() => {
                    return data.data
                        .map(item => this.distance(this.xScale(point.x), this.yScale(point.y), this.xScale(item.x), this.yScale(item.y)))
                        .filter(item => item <= this.frequency);
                })(),
                frequencyAllPoint: (() => {
                    return data.data
                        .map(item => this.distance(this.xScale(point.x), this.yScale(point.y), this.xScale(item.x), this.yScale(item.y)))
                        .filter(item => item <= this.radius[0]);
                })(),
                rate: (() => {
                    const arr = [];

                    for (let i = 0; i < data.length; i++) {
                        const res = this.distance(this.xScale(point.x), this.yScale(point.y), this.xScale(data[i].x), this.yScale(data[i].y));

                        if (res === 0) {
                            arr.push(res);
                        }
                    }

                    return arr.length;
                })(),
            };
        });

        // ## delete points that match the screen
        const filteredArr = this.newData.reduce((acc, current) => {
            const x = acc.find(item => item.viewBoxX === current.viewBoxX && item.viewBoxY === current.viewBoxY);

            if (!x) {
                return acc.concat([current]);
            } else {
                x.rate = x.rate + 1;
                return acc;
            }
        }, []);

        this.newData = [...filteredArr];

        // ## calculate the color scheme rating
        const max = Math.max(...this.newData.map(point => point.distanceAllPoint.length)),
            maxSeparator = max / 5;

        this.separatorColors = [
            Math.ceil(max - maxSeparator * 4),
            Math.ceil(max - maxSeparator * 3),
            Math.ceil(max - maxSeparator * 2),
            Math.ceil(max - maxSeparator),
            Math.ceil(max)
        ];
        // ## render circle
        this.layer1 = this.imagesvg.select('.innerImageTop1')
            .selectAll('circle')
            .data(this.newData.filter(item => item.distanceAllPoint.length >= 1))
            .enter()
            .append('svg:circle')
            .attr('id', 'layer1')
            .attr('opacity', 0.2)
            .attr('filter', 'blurMe')
            .attr('fill', '#16cbe0');

        this.layer2 = this.imagesvg.select('.innerImageTop2')
            .selectAll('circle')
            .data(this.newData.filter(item => item.distanceAllPoint.length >= 11))
            .enter()
            .append('svg:circle')
            .attr('id', 'layer2')
            .attr('opacity', 0.2)
            .attr('filter', 'blurMe')
            .attr('fill', '#0fff94');

        this.layer3 = this.imagesvg.select('.innerImageTop3')
            .selectAll('circle')
            .data(this.newData.filter(item => item.distanceAllPoint.length >= 15))
            .enter()
            .append('svg:circle')
            .attr('id', 'layer3')
            .attr('opacity', 0.7)
            .attr('filter', 'blurMe')
            .attr('fill', '#fefe22');

        this.layer4 = this.imagesvg.select('.innerImageTop4')
            .selectAll('circle')
            .data(this.newData.filter(item => item.distanceAllPoint.length >= 19))
            .enter()
            .append('svg:circle')
            .attr('id', 'layer4')
            .attr('opacity', 0.2)
            .attr('filter', 'blurMe')
            .attr('fill', '#ffa000');

        this.layer5 = this.imagesvg.select('.innerImageTop5')
            .selectAll('circle')
            .data(this.newData.filter(item => item.distanceAllPoint.length >= this.frequency))
            .enter()
            .append('svg:circle')
            .attr('id', 'layer5')
            .attr('opacity', 0.2)
            .attr('filter', 'blurMe')
            .attr('fill', '#ff2400');

        // ## render legend
        if (this.imagesvg.select('.heatmap-gradient')['_groups'][0][0] === undefined) {
            const legend = this.imagesvg.append('rect')
                .attr('x', 30)
                .attr('y', this.height * 0.94)
                .attr('class', 'heatmap-gradient')
                .attr('width', this.width * 0.18)
                .attr('height', 20)
                .attr('fill', 'url(#legend1)')
                .attr('stroke', '#000000')
                .attr('stroke-width', 1);

            const legendmin = this.imagesvg.append('text')
                .attr('class', 'd3-font')
                .attr('id', 'legendMin')
                .attr('x', 30)
                .attr('y', this.height * 0.935)
                .text('min');

            const legendmax = this.imagesvg.append('text')
                .attr('class', 'd3-font')
                .attr('id', 'legendMax')
                .attr('x', this.width * 0.188)
                .attr('y', this.height * 0.935)
                .text('max');
        }
        // zoom - this option zoom max,min  scaleExtent  and call zoomed
        d3.zoom().on('zoom', null);

        if (!options.zoomOutsideMap) {
            // Normal render
            zoom = d3.zoom()
                .filter(() => d3.event.type === 'wheel' ? d3.event.shiftKey : true)
                .extent([[0, 0], [this.width, this.height]])
                .scaleExtent([1, 15])
                .translateExtent([[0, 0], [this.width, this.height]])
                .on('zoom', () => this.zoomed());
        } else {
            // Abnormal render
            zoom = d3.zoom()
                .filter(() => d3.event.type === 'wheel' ? d3.event.shiftKey : true)
                .extent([[0, 0], [this.width, this.height]])
                // .scaleExtent([1, 15])
                // .translateExtent([[0, 0], [this.width, this.height]])
                .on('zoom', () => this.zoomed());

            const mapScaleTextX1 = Math.ceil(Math.abs(this.xScale.domain()[0] - this.xScale.domain()[1])),
                mapScaleTextX2 = Math.ceil(Math.abs(this.preZoomDefaultOptions.minX - this.preZoomDefaultOptions.maxX)),
                mapScaleTextY1 = Math.ceil(Math.abs(this.yScale.domain()[0] - this.yScale.domain()[1])),
                mapScaleTextY2 = Math.ceil(Math.abs(this.preZoomDefaultOptions.minY - this.preZoomDefaultOptions.maxY));

            this.currentZoomScale = (mapScaleTextX1 / mapScaleTextX2) > (mapScaleTextY1 / mapScaleTextY2)
                ? (mapScaleTextY1 / mapScaleTextY2)
                : (mapScaleTextX1 / mapScaleTextX2);

            const xScaleDomain = options.invertXAxis
                ? [this.preZoomDefaultOptions.maxX, this.preZoomDefaultOptions.minX]
                : [this.preZoomDefaultOptions.minX, this.preZoomDefaultOptions.maxX];

            const yScaleDomain = options.invertYAxis
                ? [this.preZoomDefaultOptions.maxY, this.preZoomDefaultOptions.minY]
                : [this.preZoomDefaultOptions.minY, this.preZoomDefaultOptions.maxY];

            const parallelX = d3.scaleLinear().domain(xScaleDomain).range([0, this.height]),
                parallelY = d3.scaleLinear().domain(yScaleDomain).range([this.width, 0]);

            this.currentXPosition = parallelX(mapOptions.minX);
            this.currentYPosition = parallelY(mapOptions.maxY);
        }

        // ##start:  call zoom to point coords
        const zoomCatcher = this.svg
            .append('rect')
            .attr('class', 'zoomCatcher')
            .attr('x', margin.left)
            .attr('y', margin.top)
            .attr('width', this.width)
            .attr('height', this.height)
            .attr('fill', 'transparent')
            .attr('stroke', 'none')
            .call(zoom);

        // ## set the current position and zoom
        zoomCatcher.call(zoom.transform, d3.zoomIdentity.translate(this.currentXPosition, this.currentYPosition).scale(this.currentZoomScale));

        zoomCatcher
            .attr('transform', `translate(${margin.left}, ${margin.top})`)
            .attr('x', 0)
            .attr('y', 0);
        // ##end:  call zoom to point coords
    }

    zoomed() {
        const transform = d3.event.transform;
        this.currentZoomScale = transform.k;
        this.currentXPosition = transform.x;
        this.currentYPosition = transform.y;

        this.newXScale = transform.rescaleX(this.xScale);
        this.newYScale = transform.rescaleY(this.yScale);

        this.svg.select('g.x.axis.top')
            .transition()
            .duration(50);
            // .call(this.xAxisTop.scale(this.newXScale)); // FIXME: buggy

        this.svg.select('g.y.axis.right')
            .transition()
            .duration(50);
            // .call(this.yAxisRight.scale(this.newYScale)); // FIXME: buggy

        this.svg.select('g.x.axis.bottom')
            .transition()
            .duration(50);
            // .call(this.xAxisBottom.scale(this.newXScale)); // FIXME: buggy

        this.svg.select('g.y.axis.left')
            .transition()
            .duration(50);
            // .call(this.yAxisLeft.scale(this.newYScale)); // FIXME: buggy

        // ## calculate  current value of data
        this.newData = this.newData.map(point => {
            const pt = {
                killerX: point.killerX,
                killerY: point.killerY,
                viewBoxX: this.newXScale(point.killerX),
                viewBoxY: this.newYScale(point.killerY),
                distanceAllPoint: point.distanceAllPoint,
                radiusOne: 0,
                radiusTwo: 0,
                radiusThree: 0,
                radiusFour: 0,
                radiusFive: 0,
                rate: point.rate,
                frequencyAllPoint: point.frequencyAllPoint,
            };

            if (pt.distanceAllPoint.length + point.rate >= 0 && pt.distanceAllPoint.length + point.rate <= this.separatorColors[0]) {
                pt.radiusOne = this.radius[0];
            } else {
                if (pt.distanceAllPoint.length + point.rate >= this.separatorColors[0] && pt.distanceAllPoint.length + point.rate <= this.separatorColors[1]) {
                    pt.radiusOne = this.radius[1];
                } else {
                    if (pt.distanceAllPoint.length + point.rate >= this.separatorColors[1] && pt.distanceAllPoint.length + point.rate <= this.separatorColors[2]) {
                        pt.radiusOne = this.radius[2];
                    } else {
                        if (pt.distanceAllPoint.length + point.rate >= this.separatorColors[2] && pt.distanceAllPoint.length + point.rate <= this.separatorColors[3]) {
                            pt.radiusOne = this.radius[3];
                        } else {
                            if (pt.distanceAllPoint.length + point.rate >= this.separatorColors[3] && pt.distanceAllPoint.length + point.rate <= this.separatorColors[4]) {
                                pt.radiusOne = this.radius[4];
                            }
                        }
                    }
                }
            }

            if (pt.distanceAllPoint.length + point.rate >= this.separatorColors[0] && pt.distanceAllPoint.length + point.rate <= this.separatorColors[1]) {
                pt.radiusTwo = this.radius[0];
            } else {
                if (pt.distanceAllPoint.length + point.rate >= this.separatorColors[1] && pt.distanceAllPoint.length + point.rate <= this.separatorColors[2]) {
                    pt.radiusTwo = this.radius[1];
                } else {
                    if (pt.distanceAllPoint.length + point.rate >= this.separatorColors[2] && pt.distanceAllPoint.length + point.rate <= this.separatorColors[3]) {
                        pt.radiusTwo = this.radius[2];
                    } else {
                        if (pt.distanceAllPoint.length + point.rate >= this.separatorColors[3] && pt.distanceAllPoint.length + point.rate <= this.separatorColors[4]) {
                            pt.radiusTwo = this.radius[3];
                        }
                    }
                }
            }

            if (pt.distanceAllPoint.length + point.rate >= this.separatorColors[1] && pt.distanceAllPoint.length + point.rate <= this.separatorColors[2]) {
                pt.radiusThree = this.radius[0];
            } else {
                if (pt.distanceAllPoint.length + point.rate >= this.separatorColors[2] && pt.distanceAllPoint.length + point.rate <= this.separatorColors[3]) {
                    pt.radiusThree = this.radius[1];
                } else {
                    if (pt.distanceAllPoint.length + point.rate >= this.separatorColors[3] && pt.distanceAllPoint.length + point.rate <= this.separatorColors[4]) {
                        pt.radiusThree = this.radius[2];
                    }
                }
            }

            if (pt.distanceAllPoint.length + point.rate >= this.separatorColors[2] && pt.distanceAllPoint.length + point.rate <= this.separatorColors[3]) {
                pt.radiusFour = this.radius[0];
            } else {
                if (pt.distanceAllPoint.length + point.rate >= this.separatorColors[3] && pt.distanceAllPoint.length + point.rate <= this.separatorColors[4]) {
                    pt.radiusFour = this.radius[1];
                }
            }

            if (pt.distanceAllPoint.length + point.rate >= this.separatorColors[3] && pt.distanceAllPoint.length + point.rate <= this.separatorColors[4]) {
                pt.radiusFive = this.radius[0];
            }

            return pt;
        });

        // ## Redraw Circle
        this.layer1.data(this.newData.filter(item => item.distanceAllPoint.length >= 0));
        this.layer1.attr('r', d => d.radiusOne);
        this.layer1.attr('cx', d => this.newXScale(d.killerX));
        this.layer1.attr('cy', d => this.newYScale(d.killerY));

        this.layer2.data(this.newData.filter(item => item.distanceAllPoint.length >= this.radius[1]));
        this.layer2.attr('r', d => d.radiusTwo);
        this.layer2.attr('cx', d => this.newXScale(d.killerX));
        this.layer2.attr('cy', d => this.newYScale(d.killerY));

        this.layer3.data(this.newData.filter(item => item.distanceAllPoint.length >= this.radius[2]));
        this.layer3.attr('r', d => d.radiusThree);
        this.layer3.attr('cx', d => this.newXScale(d.killerX));
        this.layer3.attr('cy', d => this.newYScale(d.killerY));

        this.layer4.data(this.newData.filter(item => item.distanceAllPoint.length >= this.radius[3]));
        this.layer4.attr('r', d => d.radiusFour);
        this.layer4.attr('cx', d => this.newXScale(d.killerX));
        this.layer4.attr('cy', d => this.newYScale(d.killerY));

        this.layer5.data(this.newData.filter(item => item.distanceAllPoint.length >= this.radius[4]));
        this.layer5.attr('r', d => d.radiusFive);
        this.layer5.attr('cx', d => this.newXScale(d.killerX));
        this.layer5.attr('cy', d => this.newYScale(d.killerY));

        // transform image
        this.svg.select('rect.map.xy.box').attr('transform', 'translate(' + transform.x + ',' + transform.y + ') scale(' + transform.k + ',' + transform.k + ')');
        this.imagesvg.select('rect.map.xy.box').attr('transform', 'translate(' + transform.x + ',' + transform.y + ') scale(' + transform.k + ',' + transform.k + ')');
        this.zoomScale = { xScale: this.newXScale.domain(), yScale: this.newYScale.domain() };

        const mapScaleText = Math.ceil(Math.abs(this.xScale.domain()[0] - this.xScale.domain()[1]) / transform.k / 10),
            mapScaleSize = this.width / 10;

        d3.select('.d3-legend').remove();
        d3.select('#TextLegend').remove();

        this.imagesvg.append('path')
            .attr('d', this.legendPath(this.width, this.height, mapScaleSize))
            .attr('stroke-width', '2px')
            .attr('stroke', '#000000')
            .attr('class', 'd3-legend');

        this.imagesvg
            .append('text')
            .attr('class', 'd3-font')
            .attr('id', 'TextLegend')
            .attr('x', this.width * 0.86)
            .attr('y', this.height * 0.97)
            .append('tspan')
            .text(mapScaleText);

        // ## close Loader
        d3.select('.d3-loader').attr('style', 'display:none');
        d3.select('.d3-chart-col-2').attr('style', 'visibility: visible;');
    }

    brushed() {
        // ## selection
        let extent = [[], []];

        if (!this.resetMap) {
            extent = d3.event.selection;
        } else {
            extent[0][0] = this.mapOptions$.getValue().minX;
            extent[1][0] = this.mapOptions$.getValue().maxX;
            extent[0][1] = this.mapOptions$.getValue().minY;
            extent[1][1] = this.mapOptions$.getValue().maxY;
        }

        let counter = 0;

        if (this.newData && this.newData.length && extent) {
            this.newData.forEach(point => {
                if (
                    this.newXScale(point.killerX) >= extent[0][0]
                    && this.newXScale(point.killerX) <= extent[1][0]
                    && this.newYScale(point.killerY) >= extent[0][1]
                    && this.newYScale(point.killerY) <= extent[1][1]
                ) {
                    if (point.rate === 0) {
                        counter++;
                    } else {
                        counter += point.rate + 1;
                    }
                }
            });

            // ## shadow selection
            this.layer1.attr('style', d => this.isSelectedCircle(extent, d.killerX, d.killerY) ? 'fill:#16cbe0' : 'fill:#ababab');
            this.layer2.attr('style', d => this.isSelectedCircle(extent, d.killerX, d.killerY) ? 'fill:#0fff94' : 'fill:#ababab');
            this.layer3.attr('style', d => this.isSelectedCircle(extent, d.killerX, d.killerY) ? 'fill:#fefe22' : 'fill:#ababab');
            this.layer4.attr('style', d => this.isSelectedCircle(extent, d.killerX, d.killerY) ? 'fill:#ffa000' : 'fill:#ababab');
            this.layer5.attr('style', d => this.isSelectedCircle(extent, d.killerX, d.killerY) ? 'fill:#ff2400' : 'fill:#ababab');

            // ## start tooltip
            this.crossLineToolTip(extent[1][0], extent[0][1] + ((extent[1][1] - extent[0][1]) / 2), 'selected', counter + ' points', extent[0][0]);
        }
    }

    isSelectedCircle(extent, cx, cy) {
        let result = false;

        if (
            this.newXScale(cx) >= extent[0][0]
            && this.newXScale(cx) <= extent[1][0]
            && this.newYScale(cy) >= extent[0][1]
            && this.newYScale(cy) <= extent[1][1]
        ) {
            result = true;
        }

        return result;
    }

    brushend() {
        d3.select('.d3brushmapimage')
            .call(this.selectRectMap.move, null);
    }

    crossLineToolTip(x, y, text1, text2, invertXCoord) {
        d3.select('#crossCenterX').remove();
        d3.select('#crossCenterY').remove();
        d3.select('#crossTextTooltip').remove();
        d3.select('#polygonTooltip').remove();

        const distanceX = Math.abs(this.zoomScale.xScale[0] - this.zoomScale.xScale[1]),
            distanceY = Math.abs(this.zoomScale.yScale[0] - this.zoomScale.yScale[1]);

        // Refactor various situations
        let xCurrent = x / this.width * distanceX,
            yCurrent = y / this.height * distanceY;

        const xScaleIndex = this.zoomScale.xScale[0] < this.zoomScale.xScale[1] ? 0 : 1;
        xCurrent = this.zoomScale.xScale[xScaleIndex] + xCurrent;

        const yScaleIndex = this.zoomScale.yScale[0] > this.zoomScale.yScale[1] ? 0 : 1,
            yScaleFactor = yScaleIndex ? -1 : 1;

        yCurrent = this.zoomScale.yScale[yScaleIndex] + yScaleFactor * yCurrent;

        if (!text1 && !text2) {
            const cross = d3.select('.imagesvg');

            cross.append('g')
                .attr('id', 'crossCenterX')
                .append('svg:line')
                .attr('x1', x)
                .attr('y1', 0)
                .attr('x2', x)
                .attr('y2', this.height)
                .attr('class', 'svg-killmap-cross-air');

            cross.append('g')
                .attr('id', 'crossCenterY')
                .append('svg:line')
                .attr('x1', 0)
                .attr('y1', y)
                .attr('x2', this.width)
                .attr('y2', y)
                .attr('class', 'svg-killmap-cross-air');
        }

        const spaceXText = xCurrent > 0 ? ' ' : '',
            spaceYText = yCurrent > 0 ? ' ' : '';

        if (!text1 && !text2) {
            text1 = `Y:${spaceYText}${Math.round(yCurrent)}`;
            text2 = `X:${spaceXText}${Math.round(xCurrent)}`;
        }

        let marginText = 0;

        const tooltipWidth = text1.length >= text2.length ? text1.length * 10 : text2.length * 10,
            tooltipHeight = 18;

        if (this.width - x > tooltipWidth) {
            marginText = 12;

            d3.select('.imagesvg')
                .append('polyline')
                .attr('id', 'polygonTooltip')
                .attr('stroke', '#3B4298')
                .attr('fill', 'white')
                .attr('stroke-linecap', 'round')
                .attr('points', x + ',' + y + ' ' + (x + 5) + ',' + (y - 5) + ' ' + (x + 5) + ',' +
                    (y - tooltipHeight) + ' ' + (x + tooltipWidth) + ',' + (y - tooltipHeight) + ' ' +
                    (x + tooltipWidth) + ',' + (y + tooltipHeight) + ' ' + (x + 5) + ',' + (y + tooltipHeight) + ' ' +
                    (x + 5) + ',' + (y + tooltipHeight) + ' ' + (x + 5) + ',' + (y + 10) + ' ' + (x + 5) + ',' +
                    (y + 5) + ' ' + x + ',' + y);
        } else {
            if (invertXCoord) {
                x = invertXCoord;
            }

            marginText = -tooltipWidth + 5;

            d3.select('.imagesvg')
                .append('polyline')
                .attr('id', 'polygonTooltip')
                .attr('stroke', '#3B4298')
                .attr('fill', 'white')
                .attr('stroke-linecap', 'round')
                .attr('points', x + ',' + y + ' ' + (x - 5) + ',' + (y - 5) + ' ' + (x - 5) + ',' +
                    (y - 15) + ' ' + (x - tooltipWidth) + ',' + (y - 15) + ' ' + (x - tooltipWidth) + ',' +
                    (y + 18) + ' ' + (x - 5) + ',' + (y + 18) + ' ' + (x - 5) + ',' + (y + 18) + ' ' + (x - 5) + ',' +
                    (y + 10) + ' ' + (x - 5) + ',' + (y + 5) + ' ' + x + ',' + y);
        }

        d3.select('.imagesvg')
            .append('text')
            .attr('class', 'title')
            .attr('id', 'crossTextTooltip')
            .attr('x', x + marginText)
            .attr('y', y + 14)
            .attr('font-size', 14)
            .attr('fill', 'black')
            .attr('font-family', 'monospace')
            .append('tspan')
            .text(text1);

        d3.select('#crossTextTooltip')
            .append('tspan')
            .attr('x', x + marginText)
            .attr('y', y - 2)
            .text(text2);
    }

    legendPath(width: number, height: number, scaleSize: number): string {
        const w1 = width * 0.94,
            w2 = w1 - scaleSize,
            h1 = height * 0.95,
            h2 = height * 0.94;

        return `M ${w1},${h1} ${w1},${h2} M ${w1},${h1} ${w2},${h1} ${w2},${h2} ${w2},${h1}`;
    }

    crossLineToolTipClear() {
        d3.select('#crossCenterX').remove();
        d3.select('#crossCenterY').remove();
        d3.select('#crossTextTooltip').remove();
        d3.select('#polygonTooltip').remove();
    }

    clearMapAll() {
        d3.select('.innerImageTop').html('');
    }
}
