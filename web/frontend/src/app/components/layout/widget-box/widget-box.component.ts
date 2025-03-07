import { ChangeDetectionStrategy, Component, Input, OnDestroy, OnInit, } from '@angular/core';
import { DatePipe } from '@angular/common';
import { MatDatepicker } from '@angular/material/datepicker';
import { BehaviorSubject, combineLatest, Observable, of, Subject } from 'rxjs';
import { concatMap, filter, takeUntil } from 'rxjs/operators';
import { WindowRefService } from '../../../core/services/window-ref.service';
import { PalladiumQueryService } from '../../../protocol/web-query-protocol.service';
import { WidgetQueryResult } from '../../../protocol/web-protocol';
import { Json } from '../../../protocol/igor';
import * as CardProtocol from '../../../protocol/card-protocol';
import * as DataProtocol from '../../../protocol/data-protocol';
import * as Visualization from '../../../protocol/visualization-protocol';
import JsonValue = Json.JsonValue;

interface AggregateData {
    widgetData: CardProtocol.CardWidget;
    params: CardProtocol.CardQueryParam[];
    project: string;
    database: string;
    uniqId: string;
}

@Component({
    selector: 'm-widget-box',
    templateUrl: './widget-box.component.html',
    styleUrls: ['./widget-box.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class WidgetBoxComponent implements OnInit, OnDestroy {
    // Async props
    destroy$ = new Subject<any>();
    data$ = new BehaviorSubject<WidgetQueryResult>(null);
    previousData$ = new BehaviorSubject<WidgetQueryResult>(null);

    widgetData$ = new BehaviorSubject<CardProtocol.CardWidget>(null);
    @Input('widgetData')
    set _widgetData(value: CardProtocol.CardWidget) {
        this.widgetData$.next(value);
        this.visualization = value.query.output;
    }

    params$ = new BehaviorSubject<CardProtocol.CardQueryParam[]>(null);
    @Input('params')
    set _params(value: CardProtocol.CardQueryParam[]) {
        this.params$.next(value);
    }

    project$ = new BehaviorSubject<string>(null);
    @Input('project')
    set _project(value: string) {
        this.project$.next(value);
    }

    database$ = new BehaviorSubject<string>(null);
    @Input('database')
    set _database(value: string) {
        this.database$.next(value);
    }

    uniqId$ = new BehaviorSubject<string>(null);
    @Input('uniqId')
    set _uniqId(value: string) {
        this.uniqId = value;
        this.uniqId$.next(value);
    }

    // Sync props
    aggregateData: AggregateData;
    date?: Date;
    userOS: string;
    paramDataMap = new Map<string, Array<any>>(null);
    selectedParamValueMap = new Map<string, any>(null);

    uniqId: string;

    visualization: Visualization.Visualization;
    partitionEnum = DataProtocol.TimePartition;
    paramTypeEnum = DataProtocol.QueryParamType;

    constructor(
        private windowRefService: WindowRefService,
        private queryService: PalladiumQueryService,
        private datePipe: DatePipe,
    ) {
        this.userOS = this.windowRefService.detectOperatingSystem() + '-os';
    }

    ngOnInit(): void {
        const paramObsKeys = new Array<string>();

        combineLatest([this.widgetData$, this.params$, this.project$, this.database$, this.uniqId$])
            .pipe(
                takeUntil(this.destroy$),
                filter(([widgetData, params, project, database]) => !!widgetData && !!params && !!project && !!database),
                concatMap((([widgetData, params, project, database, uniqId]) => {
                    this.aggregateData = {
                        widgetData: widgetData,
                        params: params,
                        project: project,
                        database: database,
                        uniqId: uniqId
                    };

                    const paramObsArr = params
                        .filter(param => !!param.inputSql)
                        .map(param => {
                            paramObsKeys.push(param.key);
                            return this.queryService.retrieveParameterValues({}, project, database, param.key);
                        });

                    if (paramObsArr.length > 0) {
                        return combineLatest([...paramObsArr]).pipe(takeUntil(this.destroy$));
                    } else {
                        return of([]);
                    }
                })),
                concatMap(((paramValues: Array<DataProtocol.Collection<DataProtocol.JsonPoint>>) => {
                    const { widgetData, params, project, database, uniqId } = this.aggregateData;
                    let request = {}, primaryData: Observable<WidgetQueryResult>, previousData: Observable<WidgetQueryResult>;
                    this.setupParameterOptions(params, paramValues, paramObsKeys);
                    request = this.setupPreselParamVals(params);
                    primaryData = this.queryService.sendWidgetRequest(request, project, database, widgetData.key);
                    previousData =
                        this.visualization.kind === Visualization.VisualizationKind.Indicator ?
                        this.getPreviousData(request, project, database, widgetData.key)
                        : of(null);

                    return combineLatest([primaryData, previousData]).pipe(takeUntil(this.destroy$));
                })),
            )
            .subscribe(([primaryData, previousData]) => {
                this.data$.next(primaryData);
                if (previousData) this.previousData$.next(previousData);
            });
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
    }

    onDateChanged(param: CardProtocol.CardQueryParam, event: any): void {
        const dateObj = {begin: new Date(event.value.begin), end: new Date(event.value.end)};
        this.selectedParamValueMap.set(this.uniqId + '_' + param.key, dateObj)
        this.widgetData$.next(this.widgetData$.getValue());
    }

    yearHandler(param: CardProtocol.CardQueryParam, selectedYear: Date) {
        this.selectedParamValueMap.set(this.uniqId + '_' + param.key, selectedYear.setFullYear(selectedYear.getFullYear()));
    }

    monthHandler(param: CardProtocol.CardQueryParam, selectedMonth: Date, datepicker: MatDatepicker<Date>) {
        this.selectedParamValueMap.set(this.uniqId + '_' + param.key, new Date(selectedMonth.setMonth(selectedMonth.getMonth())));
        datepicker.close();
        this.widgetData$.next(this.widgetData$.getValue());
    }

    onReload() {
        this.widgetData$.next(this.widgetData$.getValue());
    }

    private toRequestJson(param: CardProtocol.CardQueryParam, value: any): any {
        switch (param.type) {
            case DataProtocol.QueryParamType.Boolean:
            case DataProtocol.QueryParamType.Numeric:
            case DataProtocol.QueryParamType.Select:
                return value;
            case DataProtocol.QueryParamType.Date:
                return Json.DateTimeSerializer.toJson(value);
            case DataProtocol.QueryParamType.Month:
                return +(this.datePipe.transform(value, 'yyyyMM'));
            case DataProtocol.QueryParamType.DateRange:
                return {from: Json.DateTimeSerializer.toJson(value.begin), to: Json.DateTimeSerializer.toJson(value.end)};
            case DataProtocol.QueryParamType.TimePartition:
                return DataProtocol.TimePartition.toJson(value);
            default:
                return `${value}`;
        }
    }

    getPreviousData(request: Object, project: string, database: string, widgetDataKey: string): Observable<WidgetQueryResult> {
        let req = {...request};

        if (req.hasOwnProperty('date')) {
            const curDate = new Date(req['date']),
                newDate = new Date(curDate);

            newDate.setTime(curDate.getTime() - 24 * 60 * 60 * 1000);
            req['date'] = Json.DateTimeSerializer.toJson(newDate);
        }

        if (req.hasOwnProperty('month')) {
            const curDate = new Date(Math.floor(req['month']) + '-' + req['month'] % 100),
                newDate = new Date(curDate),
                year = newDate.getFullYear(),
                subtractYear = newDate.getMonth() === 0;

            newDate.setDate(1);

            if (subtractYear) {
                newDate.setMonth(11);
                newDate.setFullYear(year - 1);
            } else {
                newDate.setMonth(newDate.getMonth() - 1);
            }

            req['month'] = +(this.datePipe.transform(newDate, 'yyyyMM'));
        }

        if (req.hasOwnProperty('time')) {
            const dayInMilliSec = 1000 * 3600 * 24,
                dateHolder = {begin: new Date(req['time'].from), end: new Date(req['time'].to)},
                daysDiffering = Math.abs((dateHolder.begin.getTime() - dateHolder.end.getTime()) / (dayInMilliSec)),
                prevEndDate = new Date(dateHolder.begin.getTime() - dayInMilliSec),
                prevStartDate = new Date(prevEndDate.getTime() - (daysDiffering * 1000 * 3600 * 24));

            req = {
                time: {
                    from: Json.DateTimeSerializer.toJson(prevStartDate),
                    to: Json.DateTimeSerializer.toJson(prevEndDate)
                }
            } as Object;
        }
        return this.queryService.sendWidgetRequest(req as JsonValue, project, database, widgetDataKey);
    }

    setupParameterOptions(params: CardProtocol.CardQueryParam[], retrievedParams: Array<DataProtocol.Collection<DataProtocol.JsonPoint>>, paramObsKeys: string[]): void {
        // Map data that has been retrieved
        paramObsKeys.forEach((paramObsKey, i) => {
            if (!this.paramDataMap.has(paramObsKey)) this.paramDataMap.set(paramObsKey, retrievedParams[i].items.map(item => item.value));
        });
        // Map data that does not need retrieving
        params
            .filter(param => !this.paramDataMap.has(param.key))
            .forEach(param => {
                this.paramDataMap.set( param.key, this.getPredefParamVals(param));
        });
    }

    setupPreselParamVals(params: CardProtocol.CardQueryParam[]): Object {
        const request = {};
        params.forEach(param => {
            // set default values for request and values map
            if (this.selectedParamValueMap.has(this.uniqId + '_' + param.key)) {
                // If value changed by user do nothing, but convert and make sure right format, we have two-way binding via [ngModel]
            } else {
                // First load
                // Set selection to first value in data map on first load
                this.selectedParamValueMap.set(this.uniqId + '_' + param.key, this.paramDataMap.get(param.key)[0]);
            }
            request[param.key] = this.toRequestJson(param, this.selectedParamValueMap.get(this.uniqId + '_' + param.key));
        });
        return request;
    }

    getPredefParamVals(param: CardProtocol.CardQueryParam) {
        switch (param.type) {
            case DataProtocol.QueryParamType.Boolean:
                return [true];
            case DataProtocol.QueryParamType.Numeric:
                return [0];
            case DataProtocol.QueryParamType.Date:
                return [new Date()];
            case DataProtocol.QueryParamType.Month:
                return [new Date()];
            case DataProtocol.QueryParamType.DateRange:
                const today = new Date(),
                    firstDay = new Date(today);

                firstDay.setMonth(firstDay.getMonth() - 1);
                return [{begin: firstDay, end: today}];
            case DataProtocol.QueryParamType.TimePartition:
                return [
                    DataProtocol.TimePartition.Day,
                    DataProtocol.TimePartition.Week,
                    DataProtocol.TimePartition.Month,
                    DataProtocol.TimePartition.Quarter,
                ];
            case DataProtocol.QueryParamType.Select:
                if (param.inputSql) {
                    return [''];
                } else if (param.values instanceof Array) {
                    return param.values.length > 0 ? param.values.map(elem => elem.value) : [null];
                }

                switch (param.valueType) {
                    case DataProtocol.PrimitiveType.Bool:
                        return [true];
                    case DataProtocol.PrimitiveType.Number:
                        return [0];
                    case DataProtocol.PrimitiveType.String:
                        return [''];
                    default:
                        return [null];
                }
            default:
                return [null];
        }
    }
}
