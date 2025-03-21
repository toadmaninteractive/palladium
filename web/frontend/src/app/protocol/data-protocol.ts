// Author: Igor compiler
// Compiler version: igorc 2.1.4
// DO NOT EDIT THIS FILE - it is machine generated

import * as Igor from './igor';
import * as CommonProtocol from './common-protocol';
import * as VisualizationProtocol from './visualization-protocol';

export enum OrderDirection {
    Asc = 1,
    Desc = 2,
}

export namespace OrderDirection {
    export function toJson(value: OrderDirection): Igor.Json.JsonValue {
        return toJsonKey(value);
    }

    export function fromJson(json: Igor.Json.JsonValue): OrderDirection {
        return fromJsonKey(json);
    }

    export function toJsonKey(value: OrderDirection): Igor.Json.JsonValue {
        switch (value) {
            case OrderDirection.Asc: return 'asc';
            case OrderDirection.Desc: return 'desc';
            default: throw new Error(`Invalid OrderDirection value: ${value}`);
        }
    }

    export function fromJsonKey(json: Igor.Json.JsonValue): OrderDirection {
        switch (json) {
            case 'asc': return OrderDirection.Asc;
            case 'desc': return OrderDirection.Desc;
            default: throw new Error(`Invalid OrderDirection value: ${json}`);
        }
    }
}

export class Collection<T> {
    items!: Array<T>;

    static fromJson<T>(json: Igor.Json.JsonValue, t: Igor.Json.IJsonSerializer<T>): Collection<T> {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new Collection<T>();
        obj.items = Igor.Json.List(t).fromJson(jsonObject['items']);
        return obj;
    }

    static toJson<T>(value: Collection<T>, t: Igor.Json.IJsonSerializer<T>): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['items'] = Igor.Json.List(t).toJson(value.items);
        return result;
    }

    toJson(t: Igor.Json.IJsonSerializer<T>): Igor.Json.JsonValue {
        return Collection.toJson<T>(this, t);
    }

    static instanceJsonSerializer<T>(t: Igor.Json.IJsonSerializer<T>): Igor.Json.IJsonSerializer<Collection<T>> {
        return {
            toJson(value: Collection<T>): Igor.Json.JsonValue {
                return value.toJson(t);
            },

            fromJson(json: Igor.Json.JsonValue): Collection<T> {
                return Collection.fromJson<T>(json, t);
            }
        };
    }
}

export class CollectionSlice<T> {
    total!: number;
    items!: Array<T>;

    static fromJson<T>(json: Igor.Json.JsonValue, t: Igor.Json.IJsonSerializer<T>): CollectionSlice<T> {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new CollectionSlice<T>();
        obj.total = jsonObject['total'] as number;
        obj.items = Igor.Json.List(t).fromJson(jsonObject['items']);
        return obj;
    }

    static toJson<T>(value: CollectionSlice<T>, t: Igor.Json.IJsonSerializer<T>): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['total'] = value.total;
        result['items'] = Igor.Json.List(t).toJson(value.items);
        return result;
    }

    toJson(t: Igor.Json.IJsonSerializer<T>): Igor.Json.JsonValue {
        return CollectionSlice.toJson<T>(this, t);
    }

    static instanceJsonSerializer<T>(t: Igor.Json.IJsonSerializer<T>): Igor.Json.IJsonSerializer<CollectionSlice<T>> {
        return {
            toJson(value: CollectionSlice<T>): Igor.Json.JsonValue {
                return value.toJson(t);
            },

            fromJson(json: Igor.Json.JsonValue): CollectionSlice<T> {
                return CollectionSlice.fromJson<T>(json, t);
            }
        };
    }
}

export enum PrimitiveType {
    Bool = 1,
    Number = 2,
    String = 3,
}

export namespace PrimitiveType {
    export function toJson(value: PrimitiveType): Igor.Json.JsonValue {
        return toJsonKey(value);
    }

    export function fromJson(json: Igor.Json.JsonValue): PrimitiveType {
        return fromJsonKey(json);
    }

    export function toJsonKey(value: PrimitiveType): Igor.Json.JsonValue {
        switch (value) {
            case PrimitiveType.Bool: return 'bool';
            case PrimitiveType.Number: return 'number';
            case PrimitiveType.String: return 'string';
            default: throw new Error(`Invalid PrimitiveType value: ${value}`);
        }
    }

    export function fromJsonKey(json: Igor.Json.JsonValue): PrimitiveType {
        switch (json) {
            case 'bool': return PrimitiveType.Bool;
            case 'number': return PrimitiveType.Number;
            case 'string': return PrimitiveType.String;
            default: throw new Error(`Invalid PrimitiveType value: ${json}`);
        }
    }
}

export class IntValue {
    value!: number;

    static fromJson(json: Igor.Json.JsonValue): IntValue {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new IntValue();
        obj.value = jsonObject['value'] as number;
        return obj;
    }

    static toJson(value: IntValue): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['value'] = value.value;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return IntValue.toJson(this);
    }
}

export class NumericPoint {
    label!: string;
    value!: number;

    static fromJson(json: Igor.Json.JsonValue): NumericPoint {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new NumericPoint();
        obj.label = jsonObject['label'] as string;
        obj.value = jsonObject['value'] as number;
        return obj;
    }

    static toJson(value: NumericPoint): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['label'] = value.label;
        result['value'] = value.value;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return NumericPoint.toJson(this);
    }
}

export class BoxPlotPoint {
    label!: string;
    value!: Array<number>;

    static fromJson(json: Igor.Json.JsonValue): BoxPlotPoint {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new BoxPlotPoint();
        obj.label = jsonObject['label'] as string;
        obj.value = Igor.Json.List(Igor.Json.Number).fromJson(jsonObject['value']);
        return obj;
    }

    static toJson(value: BoxPlotPoint): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['label'] = value.label;
        result['value'] = Igor.Json.List(Igor.Json.Number).toJson(value.value);
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return BoxPlotPoint.toJson(this);
    }
}

export class ConfidencePoint {
    label!: string;
    value!: number;
    tValue!: number;
    stdError!: number;

    static fromJson(json: Igor.Json.JsonValue): ConfidencePoint {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new ConfidencePoint();
        obj.label = jsonObject['label'] as string;
        obj.value = jsonObject['value'] as number;
        obj.tValue = jsonObject['t_value'] as number;
        obj.stdError = jsonObject['std_error'] as number;
        return obj;
    }

    static toJson(value: ConfidencePoint): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['label'] = value.label;
        result['value'] = value.value;
        result['t_value'] = value.tValue;
        result['std_error'] = value.stdError;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return ConfidencePoint.toJson(this);
    }
}

export class StringPoint {
    label!: string;
    value!: string;

    static fromJson(json: Igor.Json.JsonValue): StringPoint {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new StringPoint();
        obj.label = jsonObject['label'] as string;
        obj.value = jsonObject['value'] as string;
        return obj;
    }

    static toJson(value: StringPoint): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['label'] = value.label;
        result['value'] = value.value;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return StringPoint.toJson(this);
    }
}

export class DatePoint {
    date!: Date;
    count!: number;

    static fromJson(json: Igor.Json.JsonValue): DatePoint {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new DatePoint();
        obj.date = Igor.Json.DateSerializer.fromJson(jsonObject['date']);
        obj.count = jsonObject['count'] as number;
        return obj;
    }

    static toJson(value: DatePoint): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['date'] = Igor.Json.DateSerializer.toJson(value.date);
        result['count'] = value.count;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return DatePoint.toJson(this);
    }
}

export class JsonPoint {
    label!: string;
    value!: Igor.Json.JsonValue;

    static fromJson(json: Igor.Json.JsonValue): JsonPoint {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new JsonPoint();
        obj.label = jsonObject['label'] as string;
        obj.value = jsonObject['value'];
        return obj;
    }

    static toJson(value: JsonPoint): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['label'] = value.label;
        result['value'] = value.value;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return JsonPoint.toJson(this);
    }
}

export class Point2D {
    x!: number;
    y!: number;

    static fromJson(json: Igor.Json.JsonValue): Point2D {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new Point2D();
        obj.x = jsonObject['x'] as number;
        obj.y = jsonObject['y'] as number;
        return obj;
    }

    static toJson(value: Point2D): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['x'] = value.x;
        result['y'] = value.y;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return Point2D.toJson(this);
    }
}

export class Point3D {
    x!: number;
    y!: number;
    z!: number;

    static fromJson(json: Igor.Json.JsonValue): Point3D {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new Point3D();
        obj.x = jsonObject['x'] as number;
        obj.y = jsonObject['y'] as number;
        obj.z = jsonObject['z'] as number;
        return obj;
    }

    static toJson(value: Point3D): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['x'] = value.x;
        result['y'] = value.y;
        result['z'] = value.z;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return Point3D.toJson(this);
    }
}

export class KillPoint {
    killerX!: number;
    killerY!: number;
    killerZ: number = 0;
    killerTeam!: string;
    victimX!: number;
    victimY!: number;
    victimZ: number = 0;
    victimTeam!: string;
    deathReason!: string;
    matchTime!: number;

    static fromJson(json: Igor.Json.JsonValue): KillPoint {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new KillPoint();
        obj.killerX = jsonObject['killer_x'] as number;
        obj.killerY = jsonObject['killer_y'] as number;
        obj.killerZ = ('killer_z' in jsonObject && jsonObject['killer_z'] != null) ? jsonObject['killer_z'] as number : 0;
        obj.killerTeam = jsonObject['killer_team'] as string;
        obj.victimX = jsonObject['victim_x'] as number;
        obj.victimY = jsonObject['victim_y'] as number;
        obj.victimZ = ('victim_z' in jsonObject && jsonObject['victim_z'] != null) ? jsonObject['victim_z'] as number : 0;
        obj.victimTeam = jsonObject['victim_team'] as string;
        obj.deathReason = jsonObject['death_reason'] as string;
        obj.matchTime = jsonObject['match_time'] as number;
        return obj;
    }

    static toJson(value: KillPoint): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['killer_x'] = value.killerX;
        result['killer_y'] = value.killerY;
        result['killer_z'] = value.killerZ;
        result['killer_team'] = value.killerTeam;
        result['victim_x'] = value.victimX;
        result['victim_y'] = value.victimY;
        result['victim_z'] = value.victimZ;
        result['victim_team'] = value.victimTeam;
        result['death_reason'] = value.deathReason;
        result['match_time'] = value.matchTime;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return KillPoint.toJson(this);
    }
}

export class ScatterPoint {
    x!: string;
    y!: string;
    value!: number;

    static fromJson(json: Igor.Json.JsonValue): ScatterPoint {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new ScatterPoint();
        obj.x = jsonObject['x'] as string;
        obj.y = jsonObject['y'] as string;
        obj.value = jsonObject['value'] as number;
        return obj;
    }

    static toJson(value: ScatterPoint): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['x'] = value.x;
        result['y'] = value.y;
        result['value'] = value.value;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return ScatterPoint.toJson(this);
    }
}

export enum SeriesKind {
    Value = 1,
    Numeric = 2,
    Date = 3,
    Heatmap = 4,
    Killmap = 5,
    Scatter = 6,
    Confidence = 7,
    BoxPlot = 8,
    Raw = 9,
}

export namespace SeriesKind {
    export function toJson(value: SeriesKind): Igor.Json.JsonValue {
        return toJsonKey(value);
    }

    export function fromJson(json: Igor.Json.JsonValue): SeriesKind {
        return fromJsonKey(json);
    }

    export function toJsonKey(value: SeriesKind): Igor.Json.JsonValue {
        switch (value) {
            case SeriesKind.Value: return 'value';
            case SeriesKind.Numeric: return 'numeric';
            case SeriesKind.Date: return 'date';
            case SeriesKind.Heatmap: return 'heatmap';
            case SeriesKind.Killmap: return 'killmap';
            case SeriesKind.Scatter: return 'scatter';
            case SeriesKind.Confidence: return 'confidence';
            case SeriesKind.BoxPlot: return 'box_plot';
            case SeriesKind.Raw: return 'raw';
            default: throw new Error(`Invalid SeriesKind value: ${value}`);
        }
    }

    export function fromJsonKey(json: Igor.Json.JsonValue): SeriesKind {
        switch (json) {
            case 'value': return SeriesKind.Value;
            case 'numeric': return SeriesKind.Numeric;
            case 'date': return SeriesKind.Date;
            case 'heatmap': return SeriesKind.Heatmap;
            case 'killmap': return SeriesKind.Killmap;
            case 'scatter': return SeriesKind.Scatter;
            case 'confidence': return SeriesKind.Confidence;
            case 'box_plot': return SeriesKind.BoxPlot;
            case 'raw': return SeriesKind.Raw;
            default: throw new Error(`Invalid SeriesKind value: ${json}`);
        }
    }
}

export abstract class Series {
    kind!: SeriesKind;
    key?: string | null = null;

    static fromJson(json: Igor.Json.JsonValue): Series {
        const jsonObject = json as Igor.Json.JsonObject;
        const kind = SeriesKind.fromJson(jsonObject['kind']);
        switch(kind) {
            case SeriesKind.Value:
                return NumericValue.fromJson(json);
            case SeriesKind.BoxPlot:
                return BoxPlotSeries.fromJson(json);
            case SeriesKind.Numeric:
                return NumericSeries.fromJson(json);
            case SeriesKind.Date:
                return DateSeries.fromJson(json);
            case SeriesKind.Heatmap:
                return HeatmapSeries.fromJson(json);
            case SeriesKind.Killmap:
                return KillmapSeries.fromJson(json);
            case SeriesKind.Scatter:
                return ScatterSeries.fromJson(json);
            case SeriesKind.Confidence:
                return ConfidenceSeries.fromJson(json);
            case SeriesKind.Raw:
                return RawSeries.fromJson(json);
            default:
                throw new Error(`Invalid SeriesKind value: ${kind}`);
        }
    }

    static toJson(value: Series): Igor.Json.JsonValue {
        return value.toJson();
    }

    abstract toJson(): Igor.Json.JsonValue;
}

export class NumericValue extends Series {
    kind: SeriesKind = SeriesKind.Value;
    key?: string | null = null;
    data!: NumericPoint;

    static fromJson(json: Igor.Json.JsonValue): NumericValue {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new NumericValue();
        obj.key = ('key' in jsonObject && jsonObject['key'] != null) ? jsonObject['key'] as string : null;
        obj.data = NumericPoint.fromJson(jsonObject['data']);
        return obj;
    }

    static toJson(value: NumericValue): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['kind'] = SeriesKind.toJson(value.kind);
        if (value.key != null) result['key'] = value.key;
        result['data'] = value.data.toJson();
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return NumericValue.toJson(this);
    }
}

export class BoxPlotSeries extends Series {
    kind: SeriesKind = SeriesKind.BoxPlot;
    key?: string | null = null;
    data!: Array<BoxPlotPoint>;

    static fromJson(json: Igor.Json.JsonValue): BoxPlotSeries {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new BoxPlotSeries();
        obj.key = ('key' in jsonObject && jsonObject['key'] != null) ? jsonObject['key'] as string : null;
        obj.data = Igor.Json.List(BoxPlotPoint).fromJson(jsonObject['data']);
        return obj;
    }

    static toJson(value: BoxPlotSeries): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['kind'] = SeriesKind.toJson(value.kind);
        if (value.key != null) result['key'] = value.key;
        result['data'] = Igor.Json.List(BoxPlotPoint).toJson(value.data);
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return BoxPlotSeries.toJson(this);
    }
}

export class NumericSeries extends Series {
    kind: SeriesKind = SeriesKind.Numeric;
    key?: string | null = null;
    data!: Array<NumericPoint>;

    static fromJson(json: Igor.Json.JsonValue): NumericSeries {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new NumericSeries();
        obj.key = ('key' in jsonObject && jsonObject['key'] != null) ? jsonObject['key'] as string : null;
        obj.data = Igor.Json.List(NumericPoint).fromJson(jsonObject['data']);
        return obj;
    }

    static toJson(value: NumericSeries): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['kind'] = SeriesKind.toJson(value.kind);
        if (value.key != null) result['key'] = value.key;
        result['data'] = Igor.Json.List(NumericPoint).toJson(value.data);
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return NumericSeries.toJson(this);
    }
}

export class DateSeries extends Series {
    kind: SeriesKind = SeriesKind.Date;
    key?: string | null = null;
    data!: Array<DatePoint>;

    static fromJson(json: Igor.Json.JsonValue): DateSeries {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new DateSeries();
        obj.key = ('key' in jsonObject && jsonObject['key'] != null) ? jsonObject['key'] as string : null;
        obj.data = Igor.Json.List(DatePoint).fromJson(jsonObject['data']);
        return obj;
    }

    static toJson(value: DateSeries): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['kind'] = SeriesKind.toJson(value.kind);
        if (value.key != null) result['key'] = value.key;
        result['data'] = Igor.Json.List(DatePoint).toJson(value.data);
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return DateSeries.toJson(this);
    }
}

export class HeatmapSeries extends Series {
    kind: SeriesKind = SeriesKind.Heatmap;
    key?: string | null = null;
    maps: Array<string> = [];
    data!: Array<Point2D>;

    static fromJson(json: Igor.Json.JsonValue): HeatmapSeries {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new HeatmapSeries();
        obj.key = ('key' in jsonObject && jsonObject['key'] != null) ? jsonObject['key'] as string : null;
        obj.maps = ('maps' in jsonObject && jsonObject['maps'] != null) ? Igor.Json.List(Igor.Json.String).fromJson(jsonObject['maps']) : [];
        obj.data = Igor.Json.List(Point2D).fromJson(jsonObject['data']);
        return obj;
    }

    static toJson(value: HeatmapSeries): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['kind'] = SeriesKind.toJson(value.kind);
        if (value.key != null) result['key'] = value.key;
        result['maps'] = Igor.Json.List(Igor.Json.String).toJson(value.maps);
        result['data'] = Igor.Json.List(Point2D).toJson(value.data);
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return HeatmapSeries.toJson(this);
    }
}

export class KillmapSeries extends Series {
    kind: SeriesKind = SeriesKind.Killmap;
    key?: string | null = null;
    map!: string;
    data!: Array<KillPoint>;

    static fromJson(json: Igor.Json.JsonValue): KillmapSeries {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new KillmapSeries();
        obj.key = ('key' in jsonObject && jsonObject['key'] != null) ? jsonObject['key'] as string : null;
        obj.map = jsonObject['map'] as string;
        obj.data = Igor.Json.List(KillPoint).fromJson(jsonObject['data']);
        return obj;
    }

    static toJson(value: KillmapSeries): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['kind'] = SeriesKind.toJson(value.kind);
        if (value.key != null) result['key'] = value.key;
        result['map'] = value.map;
        result['data'] = Igor.Json.List(KillPoint).toJson(value.data);
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return KillmapSeries.toJson(this);
    }
}

export class ScatterSeries extends Series {
    kind: SeriesKind = SeriesKind.Scatter;
    key?: string | null = null;
    data!: Array<ScatterPoint>;

    static fromJson(json: Igor.Json.JsonValue): ScatterSeries {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new ScatterSeries();
        obj.key = ('key' in jsonObject && jsonObject['key'] != null) ? jsonObject['key'] as string : null;
        obj.data = Igor.Json.List(ScatterPoint).fromJson(jsonObject['data']);
        return obj;
    }

    static toJson(value: ScatterSeries): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['kind'] = SeriesKind.toJson(value.kind);
        if (value.key != null) result['key'] = value.key;
        result['data'] = Igor.Json.List(ScatterPoint).toJson(value.data);
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return ScatterSeries.toJson(this);
    }
}

export class ConfidenceSeries extends Series {
    kind: SeriesKind = SeriesKind.Confidence;
    key?: string | null = null;
    data!: Array<ConfidencePoint>;

    static fromJson(json: Igor.Json.JsonValue): ConfidenceSeries {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new ConfidenceSeries();
        obj.key = ('key' in jsonObject && jsonObject['key'] != null) ? jsonObject['key'] as string : null;
        obj.data = Igor.Json.List(ConfidencePoint).fromJson(jsonObject['data']);
        return obj;
    }

    static toJson(value: ConfidenceSeries): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['kind'] = SeriesKind.toJson(value.kind);
        if (value.key != null) result['key'] = value.key;
        result['data'] = Igor.Json.List(ConfidencePoint).toJson(value.data);
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return ConfidenceSeries.toJson(this);
    }
}

export class RawSeries extends Series {
    kind: SeriesKind = SeriesKind.Raw;
    key?: string | null = null;
    data!: Array<Igor.Json.JsonValue>;

    static fromJson(json: Igor.Json.JsonValue): RawSeries {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new RawSeries();
        obj.key = ('key' in jsonObject && jsonObject['key'] != null) ? jsonObject['key'] as string : null;
        obj.data = Igor.Json.List(Igor.Json.Json).fromJson(jsonObject['data']);
        return obj;
    }

    static toJson(value: RawSeries): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['kind'] = SeriesKind.toJson(value.kind);
        if (value.key != null) result['key'] = value.key;
        result['data'] = Igor.Json.List(Igor.Json.Json).toJson(value.data);
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return RawSeries.toJson(this);
    }
}

export enum TimePartition {
    Day = 1,
    Week = 2,
    Month = 3,
    Quarter = 4,
}

export namespace TimePartition {
    export function toJson(value: TimePartition): Igor.Json.JsonValue {
        return toJsonKey(value);
    }

    export function fromJson(json: Igor.Json.JsonValue): TimePartition {
        return fromJsonKey(json);
    }

    export function toJsonKey(value: TimePartition): Igor.Json.JsonValue {
        switch (value) {
            case TimePartition.Day: return 'day';
            case TimePartition.Week: return 'week';
            case TimePartition.Month: return 'month';
            case TimePartition.Quarter: return 'quarter';
            default: throw new Error(`Invalid TimePartition value: ${value}`);
        }
    }

    export function fromJsonKey(json: Igor.Json.JsonValue): TimePartition {
        switch (json) {
            case 'day': return TimePartition.Day;
            case 'week': return TimePartition.Week;
            case 'month': return TimePartition.Month;
            case 'quarter': return TimePartition.Quarter;
            default: throw new Error(`Invalid TimePartition value: ${json}`);
        }
    }
}

export interface IKeyName {
    name: string;
    key: string;
}

export enum QueryParamType {
    Boolean = 1,
    Numeric = 2,
    Month = 3,
    Date = 4,
    DateRange = 5,
    TimePartition = 6,
    Select = 7,
}

export namespace QueryParamType {
    export function toJson(value: QueryParamType): Igor.Json.JsonValue {
        return toJsonKey(value);
    }

    export function fromJson(json: Igor.Json.JsonValue): QueryParamType {
        return fromJsonKey(json);
    }

    export function toJsonKey(value: QueryParamType): Igor.Json.JsonValue {
        switch (value) {
            case QueryParamType.Boolean: return 'boolean';
            case QueryParamType.Numeric: return 'numeric';
            case QueryParamType.Month: return 'month';
            case QueryParamType.Date: return 'date';
            case QueryParamType.DateRange: return 'date_range';
            case QueryParamType.TimePartition: return 'time_partition';
            case QueryParamType.Select: return 'select';
            default: throw new Error(`Invalid QueryParamType value: ${value}`);
        }
    }

    export function fromJsonKey(json: Igor.Json.JsonValue): QueryParamType {
        switch (json) {
            case 'boolean': return QueryParamType.Boolean;
            case 'numeric': return QueryParamType.Numeric;
            case 'month': return QueryParamType.Month;
            case 'date': return QueryParamType.Date;
            case 'date_range': return QueryParamType.DateRange;
            case 'time_partition': return QueryParamType.TimePartition;
            case 'select': return QueryParamType.Select;
            default: throw new Error(`Invalid QueryParamType value: ${json}`);
        }
    }
}

export class QueryNode {
    inputSql!: string;
    output!: VisualizationProtocol.Visualization;

    static fromJson(json: Igor.Json.JsonValue): QueryNode {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new QueryNode();
        obj.inputSql = jsonObject['input_sql'] as string;
        obj.output = VisualizationProtocol.Visualization.fromJson(jsonObject['output']);
        return obj;
    }

    static toJson(value: QueryNode): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['input_sql'] = value.inputSql;
        result['output'] = value.output.toJson();
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return QueryNode.toJson(this);
    }
}
