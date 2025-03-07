import { Component, Input, ChangeDetectorRef, ChangeDetectionStrategy, HostListener, ViewChild, ElementRef, OnDestroy } from '@angular/core';
import { BehaviorSubject, combineLatest, Subject } from 'rxjs';
import { debounceTime, filter, takeUntil } from 'rxjs/operators';
import { D3AbstractMapOptions } from '../shared/d3-abstract-map-options';
import { D3KillmapPoint } from '../shared/d3-killmap-point';
import { D3MapOptions } from '../shared/d3-map-options';
import { D3KillmapType } from '../shared/d3-killmap-type';
import * as d3 from 'd3';

const SCALE_QUOTIENT = 1.2,
    PLACEHOLDER_IMAGE = '/assets/app/media/img/maps/placeholder_image.jpg',
    NOT_ENOUGH_DATA_IMAGE = '/assets/app/media/img/maps/not_enough_data.png',
    KILLER_COLOR = '#161515',
    VICTIM_COLOR = '#FF3131';

@Component({
    selector: 'm-d3-killmap',
    templateUrl: './d3-killmap.component.html',
    styleUrls: ['./d3-killmap.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class D3KillmapComponent implements OnDestroy {
    @ViewChild('svg') private chartContainer: ElementRef;
    @ViewChild('svgbrush') private brushContainer: ElementRef;

    destroy$ = new Subject<any>();
    mapOptions$ = new BehaviorSubject<D3MapOptions | null | undefined>(null);
    data$ = new BehaviorSubject<D3KillmapPoint[] | null>([]);
    killmapType$ = new BehaviorSubject<D3KillmapType>(D3KillmapType.All);

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
        teamColors: {},
        radius: 3,
        zoomOutsideMap: false,
    };

    defaultMapOptions: D3MapOptions = <D3MapOptions>{ };
    preZoomDefaultOptions: D3MapOptions = <D3MapOptions>{ };
    options$ = new BehaviorSubject<D3AbstractMapOptions>({ ...this.defaultOptions });

    @Input('data')
    set _data(value: D3KillmapPoint[] | null) {
        const newDataNoKillEvent = (value || []).filter(p => p.killerX !== 0 && p.killerY !== 0);
        this.data$.next(newDataNoKillEvent);
    }

    killmapEnum = D3KillmapType;
    height = 650;
    width = 650;
    url = '';
    crossAirOn = false;
    caption: string;
    subtitle: string;
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
    killerPoints: any;
    svgImage: any;
    victimCrosses: any;
    linesKill: any;
    crossScale = 4;
    teamColors = new Map<string, string>();
    selectRectMap: any;
    currentZoomScale = 1;
    currentXPosition = 1;
    currentYPosition = 1;
    resetMap = false;
    killmapType: D3KillmapType;
    margin = {
        top: 0,
        bottom: 80,
        left: 0,
        right: 0
    };
    colorPalette = [
        '#34BFA3', '#716ACA', '#f4516C', '#EECC02',
        '#00c5dc', '#3B4298', '#BD1F75', '#F1941E',
        '#9A66E1', '#22A3A6', '#1474E5', '#EC5B63',
        '#8CDE4B', '#CB7212', '#4DAEFA', '#39994A',
        '#E1B7E1', '#D83A43', '#75B53E'
    ];

    constructor(private cdr: ChangeDetectorRef) {
        combineLatest([
            this.options$.asObservable(),
            this.mapOptions$.asObservable(),
            this.killmapType$.asObservable(),
            this.data$.pipe(filter(data => data instanceof Array))
        ]).pipe(
            takeUntil(this.destroy$),
            debounceTime(250),
        ).subscribe(([options, mapOptions, killmapType, data]) => {
            const self = this;

            // If abnormalities to new max and min axis parameter
            if (!mapOptions || options.zoomOutsideMap) {
                if (!mapOptions && data.length === 0) {
                    this.defaultMapOptions.url = NOT_ENOUGH_DATA_IMAGE;
                } else {
                    let minX = Number.MAX_SAFE_INTEGER,
                        minY = Number.MAX_SAFE_INTEGER,
                        maxX = Number.MIN_SAFE_INTEGER,
                        maxY = Number.MIN_SAFE_INTEGER;

                    data.forEach(point => {
                        minX = point.killerX < minX ? point.killerX : minX;
                        minY = point.killerY < minY ? point.killerY : minY;
                        maxX = point.killerX > maxX ? point.killerX : maxX;
                        maxY = point.killerY > maxY ? point.killerY : maxY;
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
                }
            } else {
                this.defaultMapOptions.url = mapOptions.url || NOT_ENOUGH_DATA_IMAGE;

                this.defaultMapOptions.minX = mapOptions.minX;
                this.defaultMapOptions.minY = mapOptions.minY;
                this.defaultMapOptions.maxX = mapOptions.maxX;
                this.defaultMapOptions.maxY = mapOptions.maxY;
            }

            // Promise to get the current picture values width and height
            this.loadImage(this.defaultMapOptions.url)
                .then(image => {
                    self.url = this.defaultMapOptions.url;
                    self.width = 650;
                    self.height = 650;
                    self.clearMapAll();

                    // Remove excess values beyond the size of the map
                    if (!options.zoomOutsideMap && mapOptions !== null) {
                        data = data.filter(item => item.killerX >= mapOptions.minX
                            && item.killerX <= mapOptions.maxX
                            && item.killerY >= mapOptions.minY
                            && item.killerY <= mapOptions.maxY
                            && item.victimX >= mapOptions.minX
                            && item.victimX <= mapOptions.maxX
                            && item.victimY >= mapOptions.minY
                            && item.victimY <= mapOptions.maxY);
                    }

                    self.renderChart(data, this.defaultMapOptions, killmapType, options);
                    setTimeout(() => this.cdr.detectChanges());
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
        this.killmapType$.unsubscribe();
        this.data$.unsubscribe();
    }

    showTooltip(event: MouseEvent): void {
        const rect = this.chartContainer.nativeElement.getBoundingClientRect(),
            x = event.clientX - rect.left, // - this.padding.left,
            y = event.clientY - rect.top; // - this.padding.top;

        this.crossAirOn
            ? this.crossLineToolTip(x, y, null, null, null)
            : this.crossLineToolTipClear();
    }

    hideTooltip(): void {
        this.crossLineToolTipClear();
    }

    loadImage(src: string): Promise<HTMLImageElement | ErrorEvent> {
        return new Promise((resolve, reject) => {
            const imageElement = new Image();
            imageElement.addEventListener('load', () => resolve(imageElement));
            imageElement.addEventListener('error', err => reject(err));
            imageElement.src = src;
        });
    }

    renderChart(data, mapOptions, killmapType: D3KillmapType, options: D3AbstractMapOptions) {
        let zoom;
        this.killmapType = killmapType;
        d3.select('.d3-expand-chart-button').attr('style', 'visibility:hidden');
        this.svgImage = d3.select('.d3KillMapImage');
        this.svg = d3.select('.d3KillMapZoomableOf');

        // Start selection initialization Selection
        this.selectRectMap = d3.brush().on('end', () => this.brushed());
        d3.select('#layoutbrush').call(this.selectRectMap);
        d3.select('.d3-killmap-inner-image').attr('style', 'top:' + (-this.height) + 'px');
        this.resetMap = false;
        this.brushContainer.nativeElement.setAttribute('style', 'display:none');
        this.brushend();
        // End selection initialization

        // Render chart
        if (this.svgImage.select('.map.xy.box')['_groups'][0][0] === undefined) {
            this.svgImage
                .append('svg:rect')
                .attr('class', 'map xy box')
                .attr('width', `${this.width}px`)
                .attr('height', `${this.height}px`)
                .attr('fill', 'url(#img1)')
                .attr('opacity', 0.4)
                .attr('transform', 'translate(0, 0) scale(1, 1)');
        }

        if (this.svgImage.select('.innerImageTop')['_groups'][0][0] === undefined) {
            this.svgImage.append('g').attr('class', 'innerImageTop');
        }

        // Draw axis
        if (this.svg.select('#containerMap')['_groups'][0][0] === undefined) {
            const g = this.svg.append('g')
                .attr('id', 'containerMap')
                .attr('transform', `translate(${this.margin.left}, ${this.margin.top})`);

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
            this.zoomScale = {xScale: this.xScale.domain(), yScale: this.yScale.domain()};

            // End draw axis
            g.append('svg')
                .attr('class', 'zoom xy box')
                .attr('width', this.width)
                .attr('height', this.height)
                .style('visibility', 'hidden')
                .attr('pointer-events', 'all');
        }

        this.teamColors.clear();

        // Convert and count data points
        if (options.teamColors) {
            const teamSet = new Set<string>();

            data.forEach((point: D3KillmapPoint) => {
                teamSet.add(point.killerTeam);
                teamSet.add(point.victimTeam);
            });

            let colorIndex = 0;

            teamSet.forEach((team: string) => {
                if (options.teamColors[team]) {
                    this.teamColors.set(team, options.teamColors[team]);
                } else {
                    this.teamColors.set(team, this.colorPalette[colorIndex]);
                    colorIndex = colorIndex < this.colorPalette.length - 1 ? colorIndex + 1 : 0;
                }
            });
        }

        const defaultColors = !options.teamColors;

        const newData = data.map(point => <D3KillmapPoint>{
            killerX: Math.round(point.killerX),
            killerY: Math.round(point.killerY),
            killerTeam: defaultColors ? KILLER_COLOR : this.teamColors.get(point.killerTeam),
            victimX: Math.round(point.victimX),
            victimY: Math.round(point.victimY),
            victimTeam: defaultColors ? VICTIM_COLOR : this.teamColors.get(point.victimTeam),
            deathReason: point.deathReason,
        });

        this.svgImage.select('.innerImageTop')
            .select('circle')
            .remove();

        // render circle, line, crosses
        if (killmapType === D3KillmapType.All) {
            this.linesKill = this.svgImage.select('.innerImageTop')
                .selectAll('line')
                .data(newData.filter(item => item.deathReason === 'kill'))
                .enter()
                .append('line')
                .attr('x1', d => this.xScale(d.killerX))
                .attr('x2', d => this.xScale(d.victimX))
                .attr('y1', d => this.yScale(d.killerY))
                .attr('y2', d => this.yScale(d.victimY))
                .attr('opacity', 0.4)
                .attr('class', 'svg-killmap-line')
                .attr('style', d => `stroke: ${d.victimTeam};`);
        }

        if (killmapType === D3KillmapType.All || killmapType === D3KillmapType.Killers) {
            this.killerPoints = this.svgImage.select('.innerImageTop')
                .selectAll('circle')
                .data(newData.filter(item => item.deathReason === 'kill'))
                .enter()
                .append('circle')
                .attr('class', 'd3-killmap-circle')
                .attr('cx', d => this.xScale(d.killerX))
                .attr('cy', d => this.yScale(d.killerY))
                .attr('r', options.radius)
                .attr('opacity', 1)
                .attr('fill', d => d.killerTeam);
        }

        if (killmapType === D3KillmapType.All || killmapType === D3KillmapType.Victims || killmapType === D3KillmapType.Suicide) {
            this.victimCrosses = this.svgImage.select('.innerImageTop')
                .selectAll('path')
                .data(killmapType === D3KillmapType.Suicide ? newData.filter(item => item.deathReason === 'suicide') : newData.filter(item => item.deathReason === 'kill'))
                .enter()
                .append('path')
                .attr('d', d => this.crossPath(d.victimX, d.victimY, this.crossScale, this.xScale, this.yScale))
                .attr('opacity', 1)
                .attr('class', 'svgKillMapCross')
                .attr('style', d => `stroke: ${d.victimTeam}; stroke-width: 3px;`);
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
        this.svg.select('.zoomCatcher').remove();
        const zoomCatcher = this.svg
            .append('rect')
            .attr('class', 'zoomCatcher')
            .attr('x', this.margin.left)
            .attr('y', this.margin.top)
            .attr('width', this.width)
            .attr('height', this.height)
            .attr('fill', 'transparent')
            .attr('stroke', 'none')
            .call(zoom);

        // set the current position and zoom
        zoomCatcher.call(zoom.transform, d3.zoomIdentity.translate(this.currentXPosition, this.currentYPosition).scale(this.currentZoomScale));
        zoomCatcher
            .attr('transform', `translate(${this.margin.left}, ${this.margin.top})`)
            .attr('x', 0)
            .attr('y', 0);

        // ##end:  call zoom to point coords
    }

    brushed() {
        // selection
        let extent = [[], []];

        if (!this.resetMap) {
            extent = d3.event.selection;
        } else {
            extent[0][0] = this.mapOptions$.getValue().minX;
            extent[1][0] = this.mapOptions$.getValue().maxX;
            extent[0][1] = this.mapOptions$.getValue().minY;
            extent[1][1] = this.mapOptions$.getValue().maxY;
        }

        if (extent && this.currentZoomScale === 1) {
            let killers = 0, victims = 0;

            // shadow selection
            if (this.killmapType$.getValue() === D3KillmapType.All || this.killmapType$.getValue() === D3KillmapType.Killers) {
                this.killerPoints.attr('style', d => {
                    if (this.isSelectedCircleNoScale(extent, d.killerX, d.killerY)) {
                        killers++;
                        return '';
                    } else {
                        return 'fill: #ABABAB;';
                    }
                });
            }

            if (this.killmapType$.getValue() === D3KillmapType.All || this.killmapType$.getValue() === D3KillmapType.Victims || this.killmapType$.getValue() === D3KillmapType.Suicide) {
                this.victimCrosses.attr('style', (d) => {
                    if (this.isSelectedCircleNoScale(extent, d.victimX, d.victimY)) {
                        victims++;
                        return 'stroke: #FF3131; stroke-width: 3;';
                    } else {
                        return 'opacity: 0.3; stroke: #ABABAB; stroke-width: 3;';
                    }
                });
            }

            if (this.killmapType$.getValue() === D3KillmapType.All) {
                this.linesKill.attr('style', (d) => {
                    if (this.isSelectedLineNoScale(extent, d.killerX, d.killerY, d.victimX, d.victimY)) {
                        return 'opacity: .4; stroke: #FF3131; stroke-width: 1.5;';
                    } else {
                        return 'opacity: .4; stroke: #FF3131; stroke-width: 1.5; stroke :#ABABAB;';
                    }
                });
            }

            this.crossLineToolTip(extent[1][0], extent[0][1] + ((extent[1][1] - extent[0][1]) / 2), 'killers:' + killers, 'victims:' + victims, extent[0][0]);
        } else {
            if (extent) {
                let killers = 0, victims = 0;

                if (this.killmapType$.getValue() === D3KillmapType.All || this.killmapType$.getValue() === D3KillmapType.Killers) {
                    this.killerPoints.attr('style', (d) => {
                        if (this.isSelectedCircle(extent, d.killerX, d.killerY)) {
                            killers++;
                            return '';
                        } else {
                            return 'fill: #ABABAB;';
                        }
                    });
                }

                if (this.killmapType$.getValue() === D3KillmapType.All || this.killmapType$.getValue() === D3KillmapType.Victims || this.killmapType$.getValue() === D3KillmapType.Suicide) {
                    this.victimCrosses.attr('style', (d) => {
                        if (this.isSelectedCircle(extent, d.victimX, d.victimY)) {
                            victims++;
                            return 'stroke: #FF3131; stroke-width: 3;';
                        } else {
                            return 'opacity: .4; stroke: #ABABAB; stroke-width: 3;';
                        }
                    });
                }

                if (this.killmapType$.getValue() === D3KillmapType.All) {
                    this.linesKill.attr('style', (d) => {
                        if (this.isSelectedLine(extent, d.killerX, d.killerY, d.victimX, d.victimY)) {
                            return 'opacity: .4; stroke: #FF3131; stroke-width: 1.5;';
                        } else {
                            return 'opacity: .4; stroke: #FF3131; stroke-width: 1.5; stroke: #ABABAB;';
                        }
                    });
                }

                this.crossLineToolTip(extent[1][0], extent[0][1] + ((extent[1][1] - extent[0][1]) / 2), 'killers:' + killers, 'victims:' + victims, extent[0][0]);
            }
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

    isSelectedLine(extent, cx1, cy1, cx2, cy2) {
        let result = false;

        if (
            this.newXScale(cx1) >= extent[0][0]
            && this.newXScale(cx1) <= extent[1][0]
            && this.newYScale(cy1) >= extent[0][1]
            && this.newYScale(cy1) <= extent[1][1]
            && this.newXScale(cx2) >= extent[0][0]
            && this.newXScale(cx2) <= extent[1][0]
            && this.newYScale(cy2) >= extent[0][1]
            && this.newYScale(cy2) <= extent[1][1]
        ) {
            result = true;
        }

        return result;
    }

    isSelectedCircleNoScale(extent, cx, cy) {
        let result = false;

        if (
            this.xScale(cx) >= extent[0][0]
            && this.xScale(cx) <= extent[1][0]
            && this.yScale(cy) >= extent[0][1]
            && this.yScale(cy) <= extent[1][1]
        ) {
            result = true;
        }

        return result;
    }

    isSelectedLineNoScale(extent, cx1, cy1, cx2, cy2) {
        let result = false;

        if (
            this.xScale(cx1) >= extent[0][0]
            && this.xScale(cx1) <= extent[1][0]
            && this.yScale(cy1) >= extent[0][1]
            && this.yScale(cy1) <= extent[1][1]
            && this.xScale(cx2) >= extent[0][0]
            && this.xScale(cx2) <= extent[1][0]
            && this.yScale(cy2) >= extent[0][1]
            && this.yScale(cy2) <= extent[1][1]
        ) {
            result = true;
        }

        return result;
    }

    brushend() {
        d3.select('#layoutbrush')
            .call(this.selectRectMap.move, null);
    }

    zoomed() {
        if (d3.event.type === 'zoom') {
            const transform = d3.event.transform;
            this.currentZoomScale = transform.k;
            this.currentXPosition = transform.x;
            this.currentYPosition = transform.y;

            this.newXScale = transform.rescaleX(this.xScale);
            this.newYScale = transform.rescaleY(this.yScale);

            this.svg.select('g.x.axis.top')
                .transition()
                .duration(0);
                // .call(this.xAxisTop.scale(transform.rescaleX(this.xScale))); // FIXME: buggy

            this.svg.select('g.y.axis.right')
                .transition()
                .duration(0);
                // .call(this.yAxisRight.scale(transform.rescaleY(this.yScale))); // FIXME: buggy

            this.svg.select('g.x.axis.bottom')
                .transition()
                .duration(0);
                // .call(this.xAxisBottom.scale(transform.rescaleX(this.xScale))); // FIXME: buggy

            this.svg.select('g.y.axis.left')
                .transition()
                .duration(0);
                // .call(this.yAxisLeft.scale(transform.rescaleY(this.yScale))); // FIXME: buggy

            // Redraw circle, line, cross
            if (this.killmapType === D3KillmapType.All || this.killmapType === D3KillmapType.Killers) {
                this.killerPoints.attr('cx', d => this.newXScale(d.killerX));
                this.killerPoints.attr('cy', d => this.newYScale(d.killerY));
            }

            if (this.killmapType === D3KillmapType.All || this.killmapType === D3KillmapType.Victims || this.killmapType === D3KillmapType.Suicide) {
                this.victimCrosses.attr('d', d => this.crossPath(d.victimX, d.victimY, this.crossScale, this.newXScale, this.newYScale));
            }

            if (this.killmapType === D3KillmapType.All) {
                this.linesKill.attr('x1', d => this.newXScale(d.killerX));
                this.linesKill.attr('y1', d => this.newYScale(d.killerY));
                this.linesKill.attr('x2', d => this.newXScale(d.victimX));
                this.linesKill.attr('y2', d => this.newYScale(d.victimY));
            }

            // Transform image
            const imageTranslate = `translate(${transform.x}, ${transform.y}) scale(${transform.k}, ${transform.k})`;

            this.svg.select('rect.map.xy.box')
                .attr('transform', imageTranslate);

            this.svgImage.select('rect.map.xy.box')
                .attr('transform', imageTranslate);

            this.zoomScale = { xScale: this.newXScale.domain(), yScale: this.newYScale.domain() };

            const mapScaleText = Math.ceil(Math.abs(this.xScale.domain()[0] - this.xScale.domain()[1]) / transform.k / 10),
                mapScaleSize = this.width / 10;

            d3.select('.d3-legend').remove();
            d3.select('#TextLegend').remove();

            // Redraw legend
            this.svgImage.append('path')
                .attr('d', this.legendPath(this.width, this.height, mapScaleSize))
                .attr('stroke-width', '2px')
                .attr('stroke', '#000000')
                .attr('class', 'd3-legend');

            this.svgImage
                .append('text')
                .attr('class', 'd3-font')
                .attr('id', 'TextLegend')
                .attr('x', this.width * 0.86)
                .attr('y', this.height * 0.97)
                .append('tspan')
                .text(mapScaleText);
        }

        // Close loader
        d3.select('.d3-loader').attr('style', 'display:none');
        d3.select('.d3-tab-button-group').attr('style', 'display:flex');
        d3.select('.d3-chart-col-2').attr('style', 'visibility: visible;');
    }

    crossLineToolTip(x, y, text1, text2, invertX) {
        d3.select('#crossCenterX').remove();
        d3.select('#crossCenterY').remove();
        d3.select('#crossTextTooltip').remove();
        d3.select('#polygonTooltip').remove();

        const distanceX = Math.abs(this.zoomScale.xScale[0] - this.zoomScale.xScale[1]);
        const distanceY = Math.abs(this.zoomScale.yScale[0] - this.zoomScale.yScale[1]);

        // Refactor various situations
        let xCurrent = x / this.width * distanceX,
            yCurrent = y / this.height * distanceY;

        const xScaleIndex = this.zoomScale.xScale[0] < this.zoomScale.xScale[1] ? 0 : 1;
        xCurrent = this.zoomScale.xScale[xScaleIndex] + xCurrent;

        const yScaleIndex = this.zoomScale.yScale[0] > this.zoomScale.yScale[1] ? 0 : 1,
            yScaleFactor = yScaleIndex ? -1 : 1;

        yCurrent = this.zoomScale.yScale[yScaleIndex] + yScaleFactor * yCurrent;

        if (text1 === null && text2 === null) {
            const cross = d3.select('.d3KillMapImage');

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

        if (text1 === null && text2 === null) {
            text1 = `Y:${spaceYText}${Math.round(yCurrent)}`;
            text2 = `X:${spaceXText}${Math.round(xCurrent)}`;
        }

        let marginText;
        const tooltipWidth = text1.length >= text2.length ? text1.length * 10 : text2.length * 10;
        const tooltipHeight = 18;

        if (this.width - x > tooltipWidth) {
            marginText = 12;

            d3.select('.d3KillMapImage')
                .append('polyline')
                .attr('id', 'polygonTooltip')
                .attr('stroke', '#3B4298')
                .attr('fill', 'white')
                .attr('stroke-linecap', 'round')
                .attr('points', x + ',' + y + ' ' + (x + 5) + ',' + (y - 5) + ' ' +
                    (x + 5) + ',' + (y - tooltipHeight) + ' ' + (x + tooltipWidth) + ',' +
                    (y - tooltipHeight) + ' ' + (x + tooltipWidth) + ',' + (y + tooltipHeight) + ' ' +
                    (x + 5) + ',' + (y + tooltipHeight) + ' ' + (x + 5) + ',' +
                    (y + tooltipHeight) + ' ' + (x + 5) + ',' + (y + 10) + ' ' + (x + 5) + ',' +
                    (y + 5) + ' ' + x + ',' + y);
        } else {
            if (invertX) {
                x = invertX;
            }

            marginText = -tooltipWidth + 5;

            d3.select('.d3KillMapImage')
                .append('polyline')
                .attr('id', 'polygonTooltip')
                .attr('stroke', '#3B4298')
                .attr('fill', 'white')
                .attr('stroke-linecap', 'round')
                .attr('points', x + ',' + y + ' ' + (x - 5) + ',' + (y - 5) + ' ' +
                    (x - 5) + ',' + (y - 15) + ' ' + (x - tooltipWidth) + ',' + (y - 15) + ' ' +
                    (x - tooltipWidth) + ',' + (y + 18) + ' ' + (x - 5) + ',' + (y + 18) + ' ' +
                    (x - 5) + ',' + (y + 18) + ' ' + (x - 5) + ',' + (y + 10) + ' ' +
                    (x - 5) + ',' + (y + 5) + ' ' + x + ',' + y);
        }

        d3.select('.d3KillMapImage')
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

    crossPath(x: number, y: number, crossScale: number, xScaleFn: (number) => number, yScaleFn: (number) => number): string {
        const xsp = xScaleFn(x) + crossScale,
            xsm = xScaleFn(x) - crossScale,
            ysp = yScaleFn(y) + crossScale,
            ysm = yScaleFn(y) - crossScale;

        return `M ${xsm},${ysm} ${xsp},${ysp} m ${xsm},${ysp} M ${xsm},${ysp} ${xsp},${ysm}`;
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

    changeMode(newMode: D3KillmapType, currentMode: D3KillmapType): void {
        if (newMode !== currentMode) {
            this.killmapType$.next(newMode);
        }
    }
}
