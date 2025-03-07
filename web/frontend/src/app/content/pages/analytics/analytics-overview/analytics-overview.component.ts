import { AfterViewInit, ChangeDetectionStrategy, ChangeDetectorRef, Component, OnDestroy, ViewChild } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { FormControl } from '@angular/forms';
import { ActivatedRoute, ParamMap, QueryParamsHandling, Router } from '@angular/router';
import { MatPaginator } from '@angular/material/paginator';
import { MatSort, Sort } from '@angular/material/sort';
import { MatTableDataSource } from '@angular/material/table';
import { BehaviorSubject, combineLatest, of, Subject, Subscription } from 'rxjs';
import { debounceTime, filter, finalize, startWith, switchMap, takeUntil } from 'rxjs/operators';
import { PalladiumQueryService } from '../../../../protocol/web-query-protocol.service';
import { ActivatedProject } from '../../../../shared/interfaces/activated-project';
import { compare } from '../../../../shared/functions/compare';
import { Json } from '../../../../protocol/igor';
import { D3MapOptions } from '../../../../components/charts/shared/d3-map-options';
import { ChartBoxExpansionService } from '../../../../core/services/chart-box-expansion.service';
import { AnalyticsErrorTypes } from '../../../../shared/error-types/analytics-error-types';
import * as CardProtocol from '../../../../protocol/card-protocol';
import * as DataProtocol from '../../../../protocol/data-protocol';
import * as WebProtocol from '../../../../protocol/web-protocol';
import * as VisualizationProtocol from '../../../../protocol/visualization-protocol';
import JsonValue = Json.JsonValue;

export interface ISimpleData {
    label: string;
    value: any;
}

export interface IResultNode {
    data: ISimpleData | any;
    map: D3MapOptions;
    visualization: VisualizationProtocol.Visualization
        | VisualizationProtocol.VisualizationIndicator
        | VisualizationProtocol.VisualizationCommonPlot
        | VisualizationProtocol.VisualizationSynchronizedPlot
        | VisualizationProtocol.VisualizationHorizontalBars
        | VisualizationProtocol.VisualizationFunnel
        | VisualizationProtocol.VisualizationHeatmap
        | VisualizationProtocol.VisualizationKillmap
        | VisualizationProtocol.VisualizationScatter;
}

@Component({
    selector: 'm-analytics-overview',
    templateUrl: './analytics-overview.component.html',
    styleUrls: ['./analytics-overview.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AnalyticsOverviewComponent implements AfterViewInit, OnDestroy {
    @ViewChild(MatPaginator) paginator: MatPaginator;
    @ViewChild(MatSort, { static: true }) set matSort(ms: MatSort) {
        this.dataSource.sort = ms;
    }
    destroy$ = new Subject<any>();
    project: WebProtocol.ProjectConfig;
    database: string;
    analyticsData$ = new BehaviorSubject<WebProtocol.AnalyticsData>(null);
    metricMap = new Map<string, boolean>(); // metric id as key
    activeMetrics$ = new BehaviorSubject<Set<string>>(new Set<string>()); // metric id as value
    gameMaps = new Map<string, CardProtocol.CardMap>(); // map name as key
    projectParamMap = new Map<string, CardProtocol.CardQueryParam>(); // query param id as key
    queryParamMap = new Map<string, CardProtocol.CardQueryParam>(); // query param key as key
    injectedParams = new Array<CardProtocol.CardQueryParam>();
    availableParams = new Array<CardProtocol.CardQueryParam>();
    requiredParams$ = new BehaviorSubject<Set<string>>(new Set<string>()); // query param id as value
    dataMap = new Map<string, any>(); // filter / query param key as key
    dynamicDataMap = new Map<string, any>(); // filter / query param key as key
    dynamicSubMap = new Map<string, Subscription>(); // filter / query param key as key
    readonly queryKey = 'query';
    queryFilterCtrl = new FormControl();
    filteredQueries$ = new BehaviorSubject<Array<CardProtocol.CardQuery>>(null);
    activeQuery$ = new BehaviorSubject<CardProtocol.CardQuery>(null);
    queryInProgress$ = new BehaviorSubject<boolean>(false);
    queryError$ = new BehaviorSubject<string | any>(null);
    queryResult$ = new BehaviorSubject<IResultNode[]>([]);
    nodeKind = VisualizationProtocol.VisualizationKind;
    displayedColumns: string[];
    dataSource = new MatTableDataSource([]);
    subData: Subscription;
    ErrorTypes = AnalyticsErrorTypes;

    constructor (
        private router: Router,
        private route: ActivatedRoute,
        private queryService: PalladiumQueryService,
        private cdr: ChangeDetectorRef,
        public chartBoxExpansionService: ChartBoxExpansionService
    ) {
        combineLatest(this.route.data, this.route.queryParamMap)
            .pipe(
                takeUntil(this.destroy$),
                switchMap(([data, queryParams]) => {
                    const activatedProject = <ActivatedProject>data.activeProject,
                        projectChanged = !this.project || this.project.id !== activatedProject.project.id;

                    if (projectChanged) {
                        this.analyticsData$.next(null);
                    }

                    return combineLatest(
                        of(activatedProject.project),
                        of(activatedProject.database),
                        of(<ParamMap>queryParams),
                        projectChanged ? this.queryService.getAnalyticsData(activatedProject.project.id) : of(null),
                    ).pipe(takeUntil(this.destroy$));
                }),
            )
            .subscribe(([project, database, queryParams, analyticsData]) => {
                // If project is changed then analyticsData holds non-null value
                if (analyticsData) {
                    // Update project configuration and database
                    this.project = project;
                    this.database = database;

                    // Initialize all
                    this.initialize(analyticsData);
                }


                // Use latest analytics data
                analyticsData = analyticsData || this.analyticsData$.getValue();

                // Check query params
                const requestedQueryKey = queryParams.has(this.queryKey) ? queryParams.get(this.queryKey) : null,
                    requestedQuery = requestedQueryKey ? analyticsData.queries.filter(q => q.key === requestedQueryKey)[0] : null;

                if (analyticsData.queries.length === 0) {
                    // No queries defined in project
                    if (queryParams.keys.length > 0) {
                        // Just clear the query string
                        this.navigate(null);
                    }
                } else if (requestedQueryKey && requestedQuery) {
                    // Query key is valid: assign active query, validate filters and params
                    const activeQuery = this.activeQuery$.getValue();

                    // Assign active query
                    this.activeQuery$.next(requestedQuery);

                    // Reset query result and error
                    this.queryResult$.next([]);
                    this.queryError$.next(null);

                    // Update required parameters
                    const requiredParams = new Set<string>([...requestedQuery.requiredParams]);
                    this.requiredParams$.next(requiredParams);

                    // Update query parameters
                    this.queryParamMap.clear();

                    requestedQuery.params
                        .map(paramId => this.projectParamMap.get(paramId))
                        .forEach(qp => this.queryParamMap.set(qp.key, qp));

                    // Check for invalid query filters / parameters
                    const invalidKeySet = new Set<string>();

                    queryParams.keys
                        .filter(key => !(key === this.queryKey || this.queryParamMap.has(key)))
                        .forEach(key => invalidKeySet.add(key));

                    if (invalidKeySet.size > 0) {
                        // Strip invalid parameters from URL
                        const params: Object = {};

                        queryParams.keys
                            .filter(qpk => !invalidKeySet.has(qpk))
                            .forEach(qpk => params[qpk] = queryParams.get(qpk));

                        this.navigate(requestedQuery.key, params);
                        return;
                    }

                    // Validate filters and query params
                    const changeMap = new Map<string, string>();
                    let dynamicValuesMissing = false;

                    [...Array.from(this.queryParamMap.values())]
                        .forEach(param => {
                            const rawValue = queryParams.has(param.key) ? queryParams.get(param.key) : undefined,
                                value = rawValue !== undefined ? this.deserialize(param, rawValue) : undefined,
                                isValid = value !== undefined && this.validate(param, value),
                                isRequired = requiredParams.has(param.id);

                            if (isRequired && !isValid) {
                                changeMap.set(param.key, this.serialize(param, this.defaultValue(param)));
                            } else if (!isRequired && !isValid && rawValue !== undefined) {
                                changeMap.set(param.key, undefined);
                            }
                        });

                    if (changeMap.size > 0) {
                        // We have to update our URL
                        const params: Object = {};

                        changeMap.forEach((value, key) => {
                            if (value === undefined) {
                                delete params[key];
                            } else {
                                params[key] = value;
                            }
                        });

                        this.navigate(requestedQuery.key, params);
                        return;
                    } else {
                        // We're all fine: set query values
                        this.dataMap.clear();
                        this.injectedParams.length = 0;
                        this.availableParams.length = 0;

                        queryParams.keys
                            .filter(key => key !== this.queryKey)
                            .map(key => this.queryParamMap.get(key))
                            .forEach(param => this.dataMap.set(param.key, this.deserialize(param, queryParams.get(param.key))));

                        [...Array.from(this.queryParamMap.values())]
                            .forEach(param => this.dataMap.has(param.key) ? this.injectedParams.push(param) : this.availableParams.push(param));

                        this.injectedParams
                            .filter(param => param.inputSql)
                            .forEach(param => {
                                const params: Object = {};

                                if (requiredParams.has(param.id) && this.dataMap.has(param.key) && !this.dataMap.get(param.key)) {
                                    dynamicValuesMissing = true;
                                }

                                if (param.dependsOn instanceof Array) {
                                    param.dependsOn.forEach(dependencyId => {
                                        const dependency = this.projectParamMap.get(dependencyId),
                                            dependencyValue: any = this.dataMap.has(dependency.key) ? this.dataMap.get(dependency.key) : undefined;

                                        if (dependency instanceof CardProtocol.CardQueryParam && dependencyValue !== undefined && dependencyValue !== '') {
                                            params[dependency.key] = this.toRequestJson(dependency, dependencyValue);
                                        }
                                    });
                                }

                                if (this.dynamicSubMap.has(param.key) && this.dynamicSubMap.get(param.key) instanceof Subscription) {
                                    this.dynamicSubMap.get(param.key).unsubscribe();
                                    this.dynamicSubMap.delete(param.key);
                                }

                                const obs = this.queryService.retrieveParameterValues(params as JsonValue, this.project.id, this.database, param.key);

                                const sub = obs
                                    .pipe(takeUntil(this.destroy$))
                                    .subscribe(response => this.dynamicDataMap.set(param.key, response.items));

                                this.dynamicSubMap.set(param.key, sub);
                            });

                        // TODO: WTF is this code?
                        // CHECK IF NO MAP_ID IS SELECTED FOR KILL MAP / HEAT MAP
                        const isMapQuery = queryParams.keys.find(key => key === 'map_id');

                        if (isMapQuery && !queryParams.get('map_id')) {
                            this.queryError$.next(this.ErrorTypes.MapIsNotSelected);
                        }

                        if (!dynamicValuesMissing) {
                            this.sendQuery();
                        }
                    }
                } else {
                    // Query key is not set or invalid: switch to first
                    const params: Object = {};

                    this.navigate(analyticsData.queries[0].key, params);
                }
            });
    }

    ngAfterViewInit(): void {
        combineLatest(
            this.activeMetrics$.asObservable(),
            this.activeQuery$.asObservable(),
            this.queryFilterCtrl.valueChanges.pipe(startWith<string>('')),
            this.analyticsData$.pipe(filter(item => !!item))
        ).pipe(
            takeUntil(this.destroy$),
            debounceTime(150),
        ).subscribe(([activeMetrics, activeQuery, needle, analyticsData]) => {
            const queries = analyticsData.queries
                .map(query => {
                    const matchesMetrics = this.metricMap.size === 0
                        || query.metrics.length === 0
                        || query.metrics.filter(m => activeMetrics.has(m)).length > 0;

                    const trimmedNeedle = (needle || '').trim().toLocaleLowerCase(),
                        matchesNeedle = trimmedNeedle.length === 0 || query.name.toLocaleLowerCase().indexOf(trimmedNeedle) !== -1;

                    query['_isOmitted'] = !(matchesMetrics && matchesNeedle);
                    query['_isActive'] = activeQuery && query.key === activeQuery.key;

                    return query;
                })
                .filter(query => query['_isActive'] || !query['_isOmitted']);

            this.filteredQueries$.next(queries);
        });
    }

    ngOnDestroy(): void {
        if (this.subData instanceof Subscription) {
            this.subData.unsubscribe();
        }

        this.dynamicSubMap.forEach((sub, key) => sub.unsubscribe());
        this.destroy$.next();
        this.destroy$.complete();
    }

    // Initialization routines
    private initialize(data: WebProtocol.AnalyticsData): void {
        // Initialize metrics
        const activeMetrics = new Set<string>();
        this.metricMap.clear();

        data.metrics.forEach(m => {
            this.metricMap.set(m.id, true);
            activeMetrics.add(m.id);
        });

        this.activeMetrics$.next(activeMetrics);

        // Initialize maps
        this.gameMaps.clear();
        data.maps.forEach(m => this.gameMaps.set(m.name, m));

        // Initialize parameters
        this.projectParamMap.clear();
        data.params.forEach(p => this.projectParamMap.set(p.id, p));

        // Initialize queries
        this.filteredQueries$.next(data.queries);

        // Clear injected and params
        this.injectedParams.length = 0;
        this.availableParams.length = 0;

        // Initialize data
        this.dataMap.clear();

        // Cleanup dynamic data subscriptions
        this.dynamicSubMap.forEach((sub, key) => sub.unsubscribe());
        this.dynamicSubMap.clear();

        // Reset injected and available params
        this.injectedParams.length = 0;
        this.availableParams.length = 0;

        // Initialize analytics data
        this.analyticsData$.next(data);
    }

    // Internal navigation
    private navigate(queryKey: string | null, queryParams: Object = {}, replaceUrl = true, queryParamsHandling: QueryParamsHandling = null): Promise<boolean> {
        if (typeof queryKey === 'string') {
            queryParams[this.queryKey] = queryKey;
        } else {
            delete queryParams[this.queryKey];
        }

        // Useful extra option: queryParamsHandling: 'merge' | 'preserve' | ''
        return this.router.navigate([], { queryParams: queryParams, replaceUrl: replaceUrl, queryParamsHandling: queryParamsHandling });
    }

    // Filter / query parameter serialization / deserialization, validation and default value generation
    private serialize(param: CardProtocol.CardQueryParam, value: any): string {
        switch (param.type) {
            case DataProtocol.QueryParamType.Boolean:
                return `${!!value}`;
            case DataProtocol.QueryParamType.Numeric:
                return `${value}`;
            case DataProtocol.QueryParamType.Date:
                return Json.DateSerializer.toJson(value) as string;
            case DataProtocol.QueryParamType.DateRange:
                return `${Json.DateSerializer.toJson(value.begin)}:${Json.DateSerializer.toJson(value.end)}`;
            case DataProtocol.QueryParamType.TimePartition:
                return DataProtocol.TimePartition.toJson(value) as string;
            case DataProtocol.QueryParamType.Select:
                return `${value}`;
            default:
                return `${value}`;
        }
    }

    private deserialize(param: CardProtocol.CardQueryParam, value: string): any {
        switch (param.type) {
            case DataProtocol.QueryParamType.Boolean:
                return value === 'true';
            case DataProtocol.QueryParamType.Numeric:
                return parseFloat(value);
            case DataProtocol.QueryParamType.Date:
                return Json.DateSerializer.fromJson(value);
            case DataProtocol.QueryParamType.DateRange:
                const [begin, end] = value.split(':');
                return {begin: Json.DateSerializer.fromJson(begin), end: Json.DateSerializer.fromJson(end)};
            case DataProtocol.QueryParamType.TimePartition:
                return DataProtocol.TimePartition.fromJson(value);
            case DataProtocol.QueryParamType.Select:
                switch (param.valueType) {
                    case DataProtocol.PrimitiveType.Bool:
                        return value === 'true';
                    case DataProtocol.PrimitiveType.Number:
                        return parseFloat(value);
                    case DataProtocol.PrimitiveType.String:
                        return value;
                    default:
                        return value;
                }
            default:
                return value;
        }
    }

    private toRequestJson(param: CardProtocol.CardQueryParam, value: any): any {
        switch (param.type) {
            case DataProtocol.QueryParamType.Boolean:
            case DataProtocol.QueryParamType.Numeric:
            case DataProtocol.QueryParamType.Select:
                return value;
            case DataProtocol.QueryParamType.Date:
                return Json.DateTimeSerializer.toJson(value);
            case DataProtocol.QueryParamType.DateRange:
                return {from: Json.DateTimeSerializer.toJson(value.begin), to: Json.DateTimeSerializer.toJson(value.end)};
            case DataProtocol.QueryParamType.TimePartition:
                return DataProtocol.TimePartition.toJson(value);
            default:
                return `${value}`;
        }
    }

    private validate(param: CardProtocol.CardQueryParam, value: any): boolean {
        switch (param.type) {
            case DataProtocol.QueryParamType.Boolean:
                return typeof value === 'boolean';
            case DataProtocol.QueryParamType.Numeric:
                return typeof value === 'number' && !isNaN(value);
            case DataProtocol.QueryParamType.Date:
                return value instanceof Date;
            case DataProtocol.QueryParamType.DateRange:
                return value && value.begin instanceof Date && value.end instanceof Date;
            case DataProtocol.QueryParamType.TimePartition:
                return DataProtocol.TimePartition.toJson(value) !== null;
            case DataProtocol.QueryParamType.Select:
                if (param.values instanceof Array) {
                    return param.values.filter(item => item.value === value).length > 0;
                } else {
                    switch (param.valueType) {
                        case DataProtocol.PrimitiveType.Bool:
                            return typeof value === 'boolean';
                        case DataProtocol.PrimitiveType.Number:
                            return typeof value === 'number';
                        case DataProtocol.PrimitiveType.String:
                            return typeof value === 'string';
                        default:
                            return false;
                    }
                }
            default:
                return false;
        }
    }

    private defaultValue(param: CardProtocol.CardQueryParam): any {
        switch (param.type) {
            case DataProtocol.QueryParamType.Boolean:
                return true;
            case DataProtocol.QueryParamType.Numeric:
                return 0;
            case DataProtocol.QueryParamType.Date:
                return new Date();
            case DataProtocol.QueryParamType.DateRange:
                const today = new Date(),
                    firstDay = new Date(today);

                firstDay.setMonth(firstDay.getMonth() - 1);
                return {begin: firstDay, end: today};
            case DataProtocol.QueryParamType.TimePartition:
                return DataProtocol.TimePartition.Day;
            case DataProtocol.QueryParamType.Select:
                if (param.inputSql) {
                    return '';
                } else if (param.values instanceof Array) {
                    return param.values.length > 0 ? param.values[0].value : null;
                }

                switch (param.valueType) {
                    case DataProtocol.PrimitiveType.Bool:
                        return true;
                    case DataProtocol.PrimitiveType.Number:
                        return 0;
                    case DataProtocol.PrimitiveType.String:
                        return '';
                    default:
                        return null;
                }
            default:
                return null;
        }
    }

    // Metrics (query types)
    isAllMetricActive(): boolean {
        if (this.metricMap.size === 0)
            return true;

        return Array.from(this.metricMap.values()).filter(active => !active).length === 0;
    }

    toggleAllMetric(): void {
        const newValue = !this.isAllMetricActive();
        Array.from(this.metricMap.keys()).forEach(metricId => this.metricMap.set(metricId, newValue));
        this.activeMetrics$.next(newValue ? new Set<string>(this.metricMap.keys()) : new Set<string>());
    }

    toggleMetric(metricId: string): void {
        const newValue = !this.metricMap.get(metricId);
        this.metricMap.set(metricId, newValue);

        const activeMetrics = this.activeMetrics$.getValue();
        newValue ? activeMetrics.add(metricId) : activeMetrics.delete(metricId);
        this.activeMetrics$.next(activeMetrics);
    }

    // Filter and query parameter manipulation
    onParamAdd(param: CardProtocol.CardQueryParam): void {
        const params: Object = {};
        params[param.key] = this.serialize(param, this.defaultValue(param));
        this.navigate(this.activeQuery$.getValue().key, params, true, 'merge');
    }

    onParamChange(param: CardProtocol.CardQueryParam, value: any): void {
        const params: Object = {};
        params[param.key] = this.serialize(param, value);
        this.navigate(this.activeQuery$.getValue().key, params, true, 'merge');
    }

    onParamRemove(param: CardProtocol.CardQueryParam): void {
        const params: Object = {};
        params[param.key] = undefined;
        this.navigate(this.activeQuery$.getValue().key, params, true, 'merge');
    }

    // Query management
    onQueryChange(query: CardProtocol.CardQuery): void {
        this.navigate(query.key, {}, false, 'merge');
    }

    sendQuery(): void {
        if (this.subData instanceof Subscription) {
            this.subData.unsubscribe();
            this.subData = null;
        }

        const activeQuery = this.activeQuery$.getValue();

        if (activeQuery) {
            if (this.queryResult$.getValue().length > 0) {
                this.queryResult$.next([]);
            }

            this.queryError$.next(null);
            this.queryInProgress$.next(true);
            const request: Object = {};

            if (!activeQuery.nodes.map(node => this.checkOptions(node)).every(bool => bool)) {
                this.queryError$.next(this.ErrorTypes.OptionsAreIncorrect);
                this.queryInProgress$.next(false);
                return;
            }

            this.dataMap.forEach((value: any, key: string) => {
                request[key] = this.toRequestJson( this.queryParamMap.get(key), value);
            });

            this.subData = this.queryService
                .sendAnalyticsRequest(request as JsonValue, this.project.id, this.database, activeQuery.key)
                .pipe(
                    takeUntil(this.destroy$),
                    finalize(() => this.queryInProgress$.next(false)),
                )
                .subscribe(response => {
                    if (!response) {
                        this.queryError$.next(this.ErrorTypes.AnalyticsQueryFailed);
                    }

                    // Check if values are zero
                    const valuesAreZero = response.result
                        .map(nodeResult =>  nodeResult.result
                            .map(numericSeries => numericSeries['data']
                                .map(numericPoint => numericPoint.value).filter(value => value !== 0)))
                                .flat(3).length === 0;

                    if (valuesAreZero) {
                        this.queryError$.next(this.ErrorTypes.ValuesAreZero);
                    } else {
                        const queryResult = activeQuery.nodes
                            .map((node, index) => {
                                const nodeResponse = response.result[index];

                                const resultNode = <IResultNode>{
                                    data: null,
                                    map: null,
                                    visualization: (<DataProtocol.QueryNode>node).output,
                                };

                                if (!(nodeResponse instanceof WebProtocol.AnalyticsQueryNodeResult)) {
                                    return resultNode;
                                }

                                switch (resultNode.visualization.kind) {
                                    case VisualizationProtocol.VisualizationKind.Indicator:
                                        const point = (<DataProtocol.NumericSeries>nodeResponse.result[0]).data[0];
                                        resultNode.data = <ISimpleData>{label: point.label, value: point.value};
                                        break;

                                    case VisualizationProtocol.VisualizationKind.CommonPlot:
                                        resultNode.data = nodeResponse.result;
                                        break;

                                    case VisualizationProtocol.VisualizationKind.SynchronizedPlot:
                                        resultNode.data = nodeResponse.result;
                                        break;

                                    case VisualizationProtocol.VisualizationKind.HorizontalBars:
                                        resultNode.data = nodeResponse.result;
                                        break;

                                    case VisualizationProtocol.VisualizationKind.Funnel:
                                        resultNode.data = nodeResponse.result;
                                        break;

                                    case VisualizationProtocol.VisualizationKind.Heatmap:
                                        const mapNames = (<DataProtocol.HeatmapSeries>nodeResponse.result[0]).maps,
                                            heatmapName = mapNames.length === 1 ? mapNames[0] : null;
                                        resultNode.map = this.gameMaps.has(heatmapName) ? this.gameMaps.get(heatmapName) : null;
                                        resultNode.data = nodeResponse.result[0];
                                        break;

                                    case VisualizationProtocol.VisualizationKind.Killmap:
                                        const killmapName = (<DataProtocol.KillmapSeries>nodeResponse.result[0]).map;
                                        resultNode.map = this.gameMaps.has(killmapName) ? this.gameMaps.get(killmapName) : null;
                                        resultNode.data = (<DataProtocol.KillmapSeries>nodeResponse.result[0]).data;
                                        break;
                                    case VisualizationProtocol.VisualizationKind.Scatter:
                                        resultNode.data = nodeResponse.result;
                                        break;
                                }
                                return resultNode;
                            });
                        this.queryResult$.next(queryResult);

                        // Update data table
                        response.result = response.result.map(item => item === null ? undefined : item);
                        this.refreshTable(response.result);
                    }
                }, error => {
                        if (error instanceof HttpErrorResponse) {
                            switch (error.status) {
                                case 403: this.queryError$.next('Error: 403 Forbidden'); break;
                                case 404: this.queryError$.next('Error: 404 File not Found'); break;
                                case 500: this.queryError$.next('Error: 500 Internal Server Error'); break;
                                default: this.queryError$.next(error);
                            }
                        }
                });
        }
    }

    refreshTable(value: WebProtocol.AnalyticsQueryNodeResult[]): void {
        const firstItem = value[0].result,
            queryResult = this.queryResult$.getValue(),
            activeQuery = this.activeQuery$.getValue(),
            tableArray = [];
        this.displayedColumns = [];
        this.dataSource = new MatTableDataSource([]);

        if (firstItem[0] !== undefined) {
            firstItem.forEach((series, index) => {
                switch (activeQuery.nodes[0].output.kind) {
                    case VisualizationProtocol.VisualizationKind.Indicator:
                    case VisualizationProtocol.VisualizationKind.CommonPlot:
                    case VisualizationProtocol.VisualizationKind.SynchronizedPlot:
                    case VisualizationProtocol.VisualizationKind.HorizontalBars:
                    case VisualizationProtocol.VisualizationKind.Funnel:
                        this.displayedColumns = Object.keys(queryResult[0].data[0].data[0]);
                        (series as DataProtocol.NumericSeries).data.forEach(numericPoint => {
                            if (tableArray.filter(item => item.label === numericPoint.label)[0]) {
                                tableArray.filter(item => item.label === numericPoint.label)[0][this.displayedColumns[index + 1]] = numericPoint.value;
                            } else {
                                tableArray.push({
                                    label: numericPoint.label,
                                    [this.displayedColumns[index + 1]]: numericPoint.value
                                });
                            }
                        });
                        break;
                    case VisualizationProtocol.VisualizationKind.Heatmap:
                        this.displayedColumns = Object.keys(queryResult[0].data.data[0]);
                        (series as DataProtocol.HeatmapSeries).data.forEach(point => tableArray.push(point));
                        break;
                    case VisualizationProtocol.VisualizationKind.Killmap:
                        this.displayedColumns = Object.keys(queryResult[0].data[0]);
                        (series as DataProtocol.KillmapSeries).data.forEach(point => tableArray.push(point));
                        break;
                }
            });

            this.dataSource = new MatTableDataSource(tableArray);
        }

        this.dataSource.paginator = this.paginator;
        this.dataSource.sort = this.matSort;
    }

    sortTable(sort: Sort): void {
        const data = this.dataSource.data.slice();

        if (!sort.active || sort.direction === '') {
            this.dataSource.data = data;
            return;
        }

        this.dataSource.data = data.sort((a, b) => {
            const isAsc = sort.direction === 'asc';
            return compare(a[sort.active], b[sort.active], isAsc);
        });

        setTimeout(() => this.cdr.detectChanges());
    }

    checkOptions(query: DataProtocol.QueryNode): boolean {
        if (query.output instanceof VisualizationProtocol.VisualizationCommonPlot) {
            return query.output.series.map(oneSerie => {
                // @ts-ignore
                return !((oneSerie.yAxis === VisualizationProtocol.AxisKindY.Primary && !query.output.yAxisType)
                    // @ts-ignore
                    || (oneSerie.yAxis === VisualizationProtocol.AxisKindY.Secondary && !query.output.secondaryYAxisType));
            }).every(bool => bool);
        }
        return true;
    }
}
