// Author: Igor compiler
// Compiler version: igorc 2.1.4
// DO NOT EDIT THIS FILE - it is machine generated

import * as Igor from './igor';

export enum AxisKindY {
    Primary = 1,
    Secondary = 2,
}

export namespace AxisKindY {
    export function toJson(value: AxisKindY): Igor.Json.JsonValue {
        return toJsonKey(value);
    }

    export function fromJson(json: Igor.Json.JsonValue): AxisKindY {
        return fromJsonKey(json);
    }

    export function toJsonKey(value: AxisKindY): Igor.Json.JsonValue {
        switch (value) {
            case AxisKindY.Primary: return 'primary';
            case AxisKindY.Secondary: return 'secondary';
            default: throw new Error(`Invalid AxisKindY value: ${value}`);
        }
    }

    export function fromJsonKey(json: Igor.Json.JsonValue): AxisKindY {
        switch (json) {
            case 'primary': return AxisKindY.Primary;
            case 'secondary': return AxisKindY.Secondary;
            default: throw new Error(`Invalid AxisKindY value: ${json}`);
        }
    }
}

export enum AxisValueType {
    Numeric = 1,
    Date = 2,
    Text = 3,
}

export namespace AxisValueType {
    export function toJson(value: AxisValueType): Igor.Json.JsonValue {
        return toJsonKey(value);
    }

    export function fromJson(json: Igor.Json.JsonValue): AxisValueType {
        return fromJsonKey(json);
    }

    export function toJsonKey(value: AxisValueType): Igor.Json.JsonValue {
        switch (value) {
            case AxisValueType.Numeric: return 'numeric';
            case AxisValueType.Date: return 'date';
            case AxisValueType.Text: return 'text';
            default: throw new Error(`Invalid AxisValueType value: ${value}`);
        }
    }

    export function fromJsonKey(json: Igor.Json.JsonValue): AxisValueType {
        switch (json) {
            case 'numeric': return AxisValueType.Numeric;
            case 'date': return AxisValueType.Date;
            case 'text': return AxisValueType.Text;
            default: throw new Error(`Invalid AxisValueType value: ${json}`);
        }
    }
}

export enum ChartType {
    Line = 1,
    Spline = 2,
    Area = 3,
    AreaSpline = 4,
    VerticalBar = 5,
    Confidence = 6,
}

export namespace ChartType {
    export function toJson(value: ChartType): Igor.Json.JsonValue {
        return toJsonKey(value);
    }

    export function fromJson(json: Igor.Json.JsonValue): ChartType {
        return fromJsonKey(json);
    }

    export function toJsonKey(value: ChartType): Igor.Json.JsonValue {
        switch (value) {
            case ChartType.Line: return 'line';
            case ChartType.Spline: return 'spline';
            case ChartType.Area: return 'area';
            case ChartType.AreaSpline: return 'area_spline';
            case ChartType.VerticalBar: return 'vertical_bar';
            case ChartType.Confidence: return 'confidence';
            default: throw new Error(`Invalid ChartType value: ${value}`);
        }
    }

    export function fromJsonKey(json: Igor.Json.JsonValue): ChartType {
        switch (json) {
            case 'line': return ChartType.Line;
            case 'spline': return ChartType.Spline;
            case 'area': return ChartType.Area;
            case 'area_spline': return ChartType.AreaSpline;
            case 'vertical_bar': return ChartType.VerticalBar;
            case 'confidence': return ChartType.Confidence;
            default: throw new Error(`Invalid ChartType value: ${json}`);
        }
    }
}

export interface ISerie {
    name?: string | null;
    key?: string | null;
}

export class SimpleSerie implements ISerie {
    name?: string | null = null;
    key?: string | null = null;

    static fromJson(json: Igor.Json.JsonValue): SimpleSerie {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new SimpleSerie();
        obj.name = ('name' in jsonObject && jsonObject['name'] != null) ? jsonObject['name'] as string : null;
        obj.key = ('key' in jsonObject && jsonObject['key'] != null) ? jsonObject['key'] as string : null;
        return obj;
    }

    static toJson(value: SimpleSerie): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        if (value.name != null) result['name'] = value.name;
        if (value.key != null) result['key'] = value.key;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return SimpleSerie.toJson(this);
    }
}

export class ChartSerie implements ISerie {
    name?: string | null = null;
    key?: string | null = null;
    groupIndex?: number | null = null;
    color?: string | null = null;
    chartType!: ChartType;
    yAxis: AxisKindY = AxisKindY.Primary;
    showValues: boolean = true;

    static fromJson(json: Igor.Json.JsonValue): ChartSerie {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new ChartSerie();
        obj.name = ('name' in jsonObject && jsonObject['name'] != null) ? jsonObject['name'] as string : null;
        obj.key = ('key' in jsonObject && jsonObject['key'] != null) ? jsonObject['key'] as string : null;
        obj.groupIndex = ('group_index' in jsonObject && jsonObject['group_index'] != null) ? jsonObject['group_index'] as number : null;
        obj.color = ('color' in jsonObject && jsonObject['color'] != null) ? jsonObject['color'] as string : null;
        obj.chartType = ChartType.fromJson(jsonObject['chart_type']);
        obj.yAxis = ('y_axis' in jsonObject && jsonObject['y_axis'] != null) ? AxisKindY.fromJson(jsonObject['y_axis']) : AxisKindY.Primary;
        obj.showValues = ('show_values' in jsonObject && jsonObject['show_values'] != null) ? jsonObject['show_values'] as boolean : true;
        return obj;
    }

    static toJson(value: ChartSerie): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        if (value.name != null) result['name'] = value.name;
        if (value.key != null) result['key'] = value.key;
        if (value.groupIndex != null) result['group_index'] = value.groupIndex;
        if (value.color != null) result['color'] = value.color;
        result['chart_type'] = ChartType.toJson(value.chartType);
        result['y_axis'] = AxisKindY.toJson(value.yAxis);
        result['show_values'] = value.showValues;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return ChartSerie.toJson(this);
    }
}

export class ScatterSerie implements ISerie {
    name?: string | null = null;
    key?: string | null = null;
    color?: string | null = null;
    radius?: number | null = null;

    static fromJson(json: Igor.Json.JsonValue): ScatterSerie {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new ScatterSerie();
        obj.name = ('name' in jsonObject && jsonObject['name'] != null) ? jsonObject['name'] as string : null;
        obj.key = ('key' in jsonObject && jsonObject['key'] != null) ? jsonObject['key'] as string : null;
        obj.color = ('color' in jsonObject && jsonObject['color'] != null) ? jsonObject['color'] as string : null;
        obj.radius = ('radius' in jsonObject && jsonObject['radius'] != null) ? jsonObject['radius'] as number : null;
        return obj;
    }

    static toJson(value: ScatterSerie): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        if (value.name != null) result['name'] = value.name;
        if (value.key != null) result['key'] = value.key;
        if (value.color != null) result['color'] = value.color;
        if (value.radius != null) result['radius'] = value.radius;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return ScatterSerie.toJson(this);
    }
}

export enum VisualizationKind {
    Indicator = 1,
    CommonPlot = 2,
    SynchronizedPlot = 3,
    HorizontalBars = 4,
    Funnel = 5,
    Heatmap = 6,
    Killmap = 7,
    Scatter = 8,
    BoxPlot = 9,
    Raw = 10,
}

export namespace VisualizationKind {
    export function toJson(value: VisualizationKind): Igor.Json.JsonValue {
        return toJsonKey(value);
    }

    export function fromJson(json: Igor.Json.JsonValue): VisualizationKind {
        return fromJsonKey(json);
    }

    export function toJsonKey(value: VisualizationKind): Igor.Json.JsonValue {
        switch (value) {
            case VisualizationKind.Indicator: return 'indicator';
            case VisualizationKind.CommonPlot: return 'common_plot';
            case VisualizationKind.SynchronizedPlot: return 'synchronized_plot';
            case VisualizationKind.HorizontalBars: return 'horizontal_bars';
            case VisualizationKind.Funnel: return 'funnel';
            case VisualizationKind.Heatmap: return 'heatmap';
            case VisualizationKind.Killmap: return 'killmap';
            case VisualizationKind.Scatter: return 'scatter';
            case VisualizationKind.BoxPlot: return 'box_plot';
            case VisualizationKind.Raw: return 'raw';
            default: throw new Error(`Invalid VisualizationKind value: ${value}`);
        }
    }

    export function fromJsonKey(json: Igor.Json.JsonValue): VisualizationKind {
        switch (json) {
            case 'indicator': return VisualizationKind.Indicator;
            case 'common_plot': return VisualizationKind.CommonPlot;
            case 'synchronized_plot': return VisualizationKind.SynchronizedPlot;
            case 'horizontal_bars': return VisualizationKind.HorizontalBars;
            case 'funnel': return VisualizationKind.Funnel;
            case 'heatmap': return VisualizationKind.Heatmap;
            case 'killmap': return VisualizationKind.Killmap;
            case 'scatter': return VisualizationKind.Scatter;
            case 'box_plot': return VisualizationKind.BoxPlot;
            case 'raw': return VisualizationKind.Raw;
            default: throw new Error(`Invalid VisualizationKind value: ${json}`);
        }
    }
}

export interface ISeries<T> {
    series: Array<T>;
}

export interface IShowAxis {
    showXAxis: boolean;
    showYAxis: boolean;
}

export interface IShowGridlines {
    showXGridLine: boolean;
    showYGridLine: boolean;
}

export interface IAxisTypes {
    xAxisType: AxisValueType;
    yAxisType: AxisValueType;
}

export interface IAxisTitles {
    xAxisTitle: string;
    yAxisTitle: string;
}

export interface ISecondaryAxis {
    secondaryYAxisTitle?: string | null;
    secondaryYAxisType?: AxisValueType | null;
}

export interface IInvertAxis {
    invertXAxis: boolean;
    invertYAxis: boolean;
}

export interface ITimeline {
    showTimeline: boolean;
}

export interface ILegend {
    showLegend: boolean;
}

export interface IZoomable {
    zoomOutsideMap: boolean;
}

export abstract class Visualization {
    kind!: VisualizationKind;
    name!: string;
    subtitle?: string | null = null;
    description!: string;

    static fromJson(json: Igor.Json.JsonValue): Visualization {
        const jsonObject = json as Igor.Json.JsonObject;
        const kind = VisualizationKind.fromJson(jsonObject['kind']);
        switch(kind) {
            case VisualizationKind.Indicator:
                return VisualizationIndicator.fromJson(json);
            case VisualizationKind.CommonPlot:
                return VisualizationCommonPlot.fromJson(json);
            case VisualizationKind.SynchronizedPlot:
                return VisualizationSynchronizedPlot.fromJson(json);
            case VisualizationKind.HorizontalBars:
                return VisualizationHorizontalBars.fromJson(json);
            case VisualizationKind.Funnel:
                return VisualizationFunnel.fromJson(json);
            case VisualizationKind.Heatmap:
                return VisualizationHeatmap.fromJson(json);
            case VisualizationKind.Killmap:
                return VisualizationKillmap.fromJson(json);
            case VisualizationKind.Scatter:
                return VisualizationScatter.fromJson(json);
            case VisualizationKind.BoxPlot:
                return VisualizationBoxPlot.fromJson(json);
            case VisualizationKind.Raw:
                return VisualizationRaw.fromJson(json);
            default:
                throw new Error(`Invalid VisualizationKind value: ${kind}`);
        }
    }

    static toJson(value: Visualization): Igor.Json.JsonValue {
        return value.toJson();
    }

    abstract toJson(): Igor.Json.JsonValue;
}

export class VisualizationIndicator extends Visualization {
    kind: VisualizationKind = VisualizationKind.Indicator;
    name!: string;
    subtitle?: string | null = null;
    description!: string;
    label!: string;

    static fromJson(json: Igor.Json.JsonValue): VisualizationIndicator {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new VisualizationIndicator();
        obj.name = jsonObject['name'] as string;
        obj.subtitle = ('subtitle' in jsonObject && jsonObject['subtitle'] != null) ? jsonObject['subtitle'] as string : null;
        obj.description = jsonObject['description'] as string;
        obj.label = jsonObject['label'] as string;
        return obj;
    }

    static toJson(value: VisualizationIndicator): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['kind'] = VisualizationKind.toJson(value.kind);
        result['name'] = value.name;
        if (value.subtitle != null) result['subtitle'] = value.subtitle;
        result['description'] = value.description;
        result['label'] = value.label;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return VisualizationIndicator.toJson(this);
    }
}

export class VisualizationCommonPlot extends Visualization implements ISeries<ChartSerie>, IShowAxis, IShowGridlines, IAxisTypes, IAxisTitles, ISecondaryAxis, ITimeline, ILegend {
    kind: VisualizationKind = VisualizationKind.CommonPlot;
    name!: string;
    subtitle?: string | null = null;
    description!: string;
    series!: Array<ChartSerie>;
    showXAxis: boolean = true;
    showYAxis: boolean = true;
    showXGridLine: boolean = true;
    showYGridLine: boolean = true;
    xAxisType!: AxisValueType;
    yAxisType!: AxisValueType;
    xAxisTitle: string = "";
    yAxisTitle: string = "";
    secondaryYAxisTitle?: string | null = null;
    secondaryYAxisType?: AxisValueType | null = null;
    showTimeline: boolean = false;
    showLegend: boolean = true;
    showEstimate: boolean = false;

    static fromJson(json: Igor.Json.JsonValue): VisualizationCommonPlot {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new VisualizationCommonPlot();
        obj.name = jsonObject['name'] as string;
        obj.subtitle = ('subtitle' in jsonObject && jsonObject['subtitle'] != null) ? jsonObject['subtitle'] as string : null;
        obj.description = jsonObject['description'] as string;
        obj.series = Igor.Json.List(ChartSerie).fromJson(jsonObject['series']);
        obj.showXAxis = ('show_x_axis' in jsonObject && jsonObject['show_x_axis'] != null) ? jsonObject['show_x_axis'] as boolean : true;
        obj.showYAxis = ('show_y_axis' in jsonObject && jsonObject['show_y_axis'] != null) ? jsonObject['show_y_axis'] as boolean : true;
        obj.showXGridLine = ('show_x_grid_line' in jsonObject && jsonObject['show_x_grid_line'] != null) ? jsonObject['show_x_grid_line'] as boolean : true;
        obj.showYGridLine = ('show_y_grid_line' in jsonObject && jsonObject['show_y_grid_line'] != null) ? jsonObject['show_y_grid_line'] as boolean : true;
        obj.xAxisType = AxisValueType.fromJson(jsonObject['x_axis_type']);
        obj.yAxisType = AxisValueType.fromJson(jsonObject['y_axis_type']);
        obj.xAxisTitle = ('x_axis_title' in jsonObject && jsonObject['x_axis_title'] != null) ? jsonObject['x_axis_title'] as string : "";
        obj.yAxisTitle = ('y_axis_title' in jsonObject && jsonObject['y_axis_title'] != null) ? jsonObject['y_axis_title'] as string : "";
        obj.secondaryYAxisTitle = ('secondary_y_axis_title' in jsonObject && jsonObject['secondary_y_axis_title'] != null) ? jsonObject['secondary_y_axis_title'] as string : null;
        obj.secondaryYAxisType = ('secondary_y_axis_type' in jsonObject && jsonObject['secondary_y_axis_type'] != null) ? AxisValueType.fromJson(jsonObject['secondary_y_axis_type']) : null;
        obj.showTimeline = ('show_timeline' in jsonObject && jsonObject['show_timeline'] != null) ? jsonObject['show_timeline'] as boolean : false;
        obj.showLegend = ('show_legend' in jsonObject && jsonObject['show_legend'] != null) ? jsonObject['show_legend'] as boolean : true;
        obj.showEstimate = ('show_estimate' in jsonObject && jsonObject['show_estimate'] != null) ? jsonObject['show_estimate'] as boolean : false;
        return obj;
    }

    static toJson(value: VisualizationCommonPlot): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['kind'] = VisualizationKind.toJson(value.kind);
        result['name'] = value.name;
        if (value.subtitle != null) result['subtitle'] = value.subtitle;
        result['description'] = value.description;
        result['series'] = Igor.Json.List(ChartSerie).toJson(value.series);
        result['show_x_axis'] = value.showXAxis;
        result['show_y_axis'] = value.showYAxis;
        result['show_x_grid_line'] = value.showXGridLine;
        result['show_y_grid_line'] = value.showYGridLine;
        result['x_axis_type'] = AxisValueType.toJson(value.xAxisType);
        result['y_axis_type'] = AxisValueType.toJson(value.yAxisType);
        result['x_axis_title'] = value.xAxisTitle;
        result['y_axis_title'] = value.yAxisTitle;
        if (value.secondaryYAxisTitle != null) result['secondary_y_axis_title'] = value.secondaryYAxisTitle;
        if (value.secondaryYAxisType != null) result['secondary_y_axis_type'] = AxisValueType.toJson(value.secondaryYAxisType);
        result['show_timeline'] = value.showTimeline;
        result['show_legend'] = value.showLegend;
        result['show_estimate'] = value.showEstimate;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return VisualizationCommonPlot.toJson(this);
    }
}

export class VisualizationSynchronizedPlot extends Visualization implements ISeries<ChartSerie>, IShowAxis, IShowGridlines, IAxisTypes, IAxisTitles, ISecondaryAxis, ITimeline, ILegend {
    kind: VisualizationKind = VisualizationKind.SynchronizedPlot;
    name!: string;
    subtitle?: string | null = null;
    description!: string;
    series!: Array<ChartSerie>;
    showXAxis: boolean = true;
    showYAxis: boolean = true;
    showXGridLine: boolean = true;
    showYGridLine: boolean = true;
    xAxisType!: AxisValueType;
    yAxisType!: AxisValueType;
    xAxisTitle: string = "";
    yAxisTitle: string = "";
    secondaryYAxisTitle?: string | null = null;
    secondaryYAxisType?: AxisValueType | null = null;
    showTimeline: boolean = false;
    showLegend: boolean = true;

    static fromJson(json: Igor.Json.JsonValue): VisualizationSynchronizedPlot {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new VisualizationSynchronizedPlot();
        obj.name = jsonObject['name'] as string;
        obj.subtitle = ('subtitle' in jsonObject && jsonObject['subtitle'] != null) ? jsonObject['subtitle'] as string : null;
        obj.description = jsonObject['description'] as string;
        obj.series = Igor.Json.List(ChartSerie).fromJson(jsonObject['series']);
        obj.showXAxis = ('show_x_axis' in jsonObject && jsonObject['show_x_axis'] != null) ? jsonObject['show_x_axis'] as boolean : true;
        obj.showYAxis = ('show_y_axis' in jsonObject && jsonObject['show_y_axis'] != null) ? jsonObject['show_y_axis'] as boolean : true;
        obj.showXGridLine = ('show_x_grid_line' in jsonObject && jsonObject['show_x_grid_line'] != null) ? jsonObject['show_x_grid_line'] as boolean : true;
        obj.showYGridLine = ('show_y_grid_line' in jsonObject && jsonObject['show_y_grid_line'] != null) ? jsonObject['show_y_grid_line'] as boolean : true;
        obj.xAxisType = AxisValueType.fromJson(jsonObject['x_axis_type']);
        obj.yAxisType = AxisValueType.fromJson(jsonObject['y_axis_type']);
        obj.xAxisTitle = ('x_axis_title' in jsonObject && jsonObject['x_axis_title'] != null) ? jsonObject['x_axis_title'] as string : "";
        obj.yAxisTitle = ('y_axis_title' in jsonObject && jsonObject['y_axis_title'] != null) ? jsonObject['y_axis_title'] as string : "";
        obj.secondaryYAxisTitle = ('secondary_y_axis_title' in jsonObject && jsonObject['secondary_y_axis_title'] != null) ? jsonObject['secondary_y_axis_title'] as string : null;
        obj.secondaryYAxisType = ('secondary_y_axis_type' in jsonObject && jsonObject['secondary_y_axis_type'] != null) ? AxisValueType.fromJson(jsonObject['secondary_y_axis_type']) : null;
        obj.showTimeline = ('show_timeline' in jsonObject && jsonObject['show_timeline'] != null) ? jsonObject['show_timeline'] as boolean : false;
        obj.showLegend = ('show_legend' in jsonObject && jsonObject['show_legend'] != null) ? jsonObject['show_legend'] as boolean : true;
        return obj;
    }

    static toJson(value: VisualizationSynchronizedPlot): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['kind'] = VisualizationKind.toJson(value.kind);
        result['name'] = value.name;
        if (value.subtitle != null) result['subtitle'] = value.subtitle;
        result['description'] = value.description;
        result['series'] = Igor.Json.List(ChartSerie).toJson(value.series);
        result['show_x_axis'] = value.showXAxis;
        result['show_y_axis'] = value.showYAxis;
        result['show_x_grid_line'] = value.showXGridLine;
        result['show_y_grid_line'] = value.showYGridLine;
        result['x_axis_type'] = AxisValueType.toJson(value.xAxisType);
        result['y_axis_type'] = AxisValueType.toJson(value.yAxisType);
        result['x_axis_title'] = value.xAxisTitle;
        result['y_axis_title'] = value.yAxisTitle;
        if (value.secondaryYAxisTitle != null) result['secondary_y_axis_title'] = value.secondaryYAxisTitle;
        if (value.secondaryYAxisType != null) result['secondary_y_axis_type'] = AxisValueType.toJson(value.secondaryYAxisType);
        result['show_timeline'] = value.showTimeline;
        result['show_legend'] = value.showLegend;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return VisualizationSynchronizedPlot.toJson(this);
    }
}

export class VisualizationHorizontalBars extends Visualization implements ISeries<SimpleSerie>, IShowAxis, IShowGridlines, IAxisTitles, ILegend {
    kind: VisualizationKind = VisualizationKind.HorizontalBars;
    name!: string;
    subtitle?: string | null = null;
    description!: string;
    series!: Array<SimpleSerie>;
    showXAxis: boolean = true;
    showYAxis: boolean = true;
    showXGridLine: boolean = true;
    showYGridLine: boolean = true;
    xAxisTitle: string = "";
    yAxisTitle: string = "";
    showLegend: boolean = true;

    static fromJson(json: Igor.Json.JsonValue): VisualizationHorizontalBars {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new VisualizationHorizontalBars();
        obj.name = jsonObject['name'] as string;
        obj.subtitle = ('subtitle' in jsonObject && jsonObject['subtitle'] != null) ? jsonObject['subtitle'] as string : null;
        obj.description = jsonObject['description'] as string;
        obj.series = Igor.Json.List(SimpleSerie).fromJson(jsonObject['series']);
        obj.showXAxis = ('show_x_axis' in jsonObject && jsonObject['show_x_axis'] != null) ? jsonObject['show_x_axis'] as boolean : true;
        obj.showYAxis = ('show_y_axis' in jsonObject && jsonObject['show_y_axis'] != null) ? jsonObject['show_y_axis'] as boolean : true;
        obj.showXGridLine = ('show_x_grid_line' in jsonObject && jsonObject['show_x_grid_line'] != null) ? jsonObject['show_x_grid_line'] as boolean : true;
        obj.showYGridLine = ('show_y_grid_line' in jsonObject && jsonObject['show_y_grid_line'] != null) ? jsonObject['show_y_grid_line'] as boolean : true;
        obj.xAxisTitle = ('x_axis_title' in jsonObject && jsonObject['x_axis_title'] != null) ? jsonObject['x_axis_title'] as string : "";
        obj.yAxisTitle = ('y_axis_title' in jsonObject && jsonObject['y_axis_title'] != null) ? jsonObject['y_axis_title'] as string : "";
        obj.showLegend = ('show_legend' in jsonObject && jsonObject['show_legend'] != null) ? jsonObject['show_legend'] as boolean : true;
        return obj;
    }

    static toJson(value: VisualizationHorizontalBars): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['kind'] = VisualizationKind.toJson(value.kind);
        result['name'] = value.name;
        if (value.subtitle != null) result['subtitle'] = value.subtitle;
        result['description'] = value.description;
        result['series'] = Igor.Json.List(SimpleSerie).toJson(value.series);
        result['show_x_axis'] = value.showXAxis;
        result['show_y_axis'] = value.showYAxis;
        result['show_x_grid_line'] = value.showXGridLine;
        result['show_y_grid_line'] = value.showYGridLine;
        result['x_axis_title'] = value.xAxisTitle;
        result['y_axis_title'] = value.yAxisTitle;
        result['show_legend'] = value.showLegend;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return VisualizationHorizontalBars.toJson(this);
    }
}

export class VisualizationFunnel extends Visualization implements ISeries<SimpleSerie> {
    kind: VisualizationKind = VisualizationKind.Funnel;
    name!: string;
    subtitle?: string | null = null;
    description!: string;
    series!: Array<SimpleSerie>;

    static fromJson(json: Igor.Json.JsonValue): VisualizationFunnel {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new VisualizationFunnel();
        obj.name = jsonObject['name'] as string;
        obj.subtitle = ('subtitle' in jsonObject && jsonObject['subtitle'] != null) ? jsonObject['subtitle'] as string : null;
        obj.description = jsonObject['description'] as string;
        obj.series = Igor.Json.List(SimpleSerie).fromJson(jsonObject['series']);
        return obj;
    }

    static toJson(value: VisualizationFunnel): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['kind'] = VisualizationKind.toJson(value.kind);
        result['name'] = value.name;
        if (value.subtitle != null) result['subtitle'] = value.subtitle;
        result['description'] = value.description;
        result['series'] = Igor.Json.List(SimpleSerie).toJson(value.series);
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return VisualizationFunnel.toJson(this);
    }
}

export class VisualizationHeatmap extends Visualization implements IInvertAxis, IZoomable {
    kind: VisualizationKind = VisualizationKind.Heatmap;
    name!: string;
    subtitle?: string | null = null;
    description!: string;
    invertXAxis: boolean = false;
    invertYAxis: boolean = false;
    zoomOutsideMap: boolean = false;

    static fromJson(json: Igor.Json.JsonValue): VisualizationHeatmap {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new VisualizationHeatmap();
        obj.name = jsonObject['name'] as string;
        obj.subtitle = ('subtitle' in jsonObject && jsonObject['subtitle'] != null) ? jsonObject['subtitle'] as string : null;
        obj.description = jsonObject['description'] as string;
        obj.invertXAxis = ('invert_x_axis' in jsonObject && jsonObject['invert_x_axis'] != null) ? jsonObject['invert_x_axis'] as boolean : false;
        obj.invertYAxis = ('invert_y_axis' in jsonObject && jsonObject['invert_y_axis'] != null) ? jsonObject['invert_y_axis'] as boolean : false;
        obj.zoomOutsideMap = ('zoom_outside_map' in jsonObject && jsonObject['zoom_outside_map'] != null) ? jsonObject['zoom_outside_map'] as boolean : false;
        return obj;
    }

    static toJson(value: VisualizationHeatmap): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['kind'] = VisualizationKind.toJson(value.kind);
        result['name'] = value.name;
        if (value.subtitle != null) result['subtitle'] = value.subtitle;
        result['description'] = value.description;
        result['invert_x_axis'] = value.invertXAxis;
        result['invert_y_axis'] = value.invertYAxis;
        result['zoom_outside_map'] = value.zoomOutsideMap;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return VisualizationHeatmap.toJson(this);
    }
}

export class VisualizationKillmap extends Visualization implements IInvertAxis, IZoomable {
    kind: VisualizationKind = VisualizationKind.Killmap;
    name!: string;
    subtitle?: string | null = null;
    description!: string;
    invertXAxis: boolean = false;
    invertYAxis: boolean = false;
    zoomOutsideMap: boolean = false;
    teamColors?: {[key: string]: string} | null = null;
    radius?: number | null = null;

    static fromJson(json: Igor.Json.JsonValue): VisualizationKillmap {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new VisualizationKillmap();
        obj.name = jsonObject['name'] as string;
        obj.subtitle = ('subtitle' in jsonObject && jsonObject['subtitle'] != null) ? jsonObject['subtitle'] as string : null;
        obj.description = jsonObject['description'] as string;
        obj.invertXAxis = ('invert_x_axis' in jsonObject && jsonObject['invert_x_axis'] != null) ? jsonObject['invert_x_axis'] as boolean : false;
        obj.invertYAxis = ('invert_y_axis' in jsonObject && jsonObject['invert_y_axis'] != null) ? jsonObject['invert_y_axis'] as boolean : false;
        obj.zoomOutsideMap = ('zoom_outside_map' in jsonObject && jsonObject['zoom_outside_map'] != null) ? jsonObject['zoom_outside_map'] as boolean : false;
        obj.teamColors = ('team_colors' in jsonObject && jsonObject['team_colors'] != null) ? Igor.Json.Dict(Igor.Json.String).fromJson(jsonObject['team_colors']) : null;
        obj.radius = ('radius' in jsonObject && jsonObject['radius'] != null) ? jsonObject['radius'] as number : null;
        return obj;
    }

    static toJson(value: VisualizationKillmap): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['kind'] = VisualizationKind.toJson(value.kind);
        result['name'] = value.name;
        if (value.subtitle != null) result['subtitle'] = value.subtitle;
        result['description'] = value.description;
        result['invert_x_axis'] = value.invertXAxis;
        result['invert_y_axis'] = value.invertYAxis;
        result['zoom_outside_map'] = value.zoomOutsideMap;
        if (value.teamColors != null) result['team_colors'] = Igor.Json.Dict(Igor.Json.String).toJson(value.teamColors);
        if (value.radius != null) result['radius'] = value.radius;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return VisualizationKillmap.toJson(this);
    }
}

export class VisualizationScatter extends Visualization implements ISeries<ScatterSerie>, IShowAxis, IShowGridlines, IAxisTypes, IAxisTitles, ILegend, IInvertAxis {
    kind: VisualizationKind = VisualizationKind.Scatter;
    name!: string;
    subtitle?: string | null = null;
    description!: string;
    series!: Array<ScatterSerie>;
    showXAxis: boolean = true;
    showYAxis: boolean = true;
    showXGridLine: boolean = true;
    showYGridLine: boolean = true;
    xAxisType!: AxisValueType;
    yAxisType!: AxisValueType;
    xAxisTitle: string = "";
    yAxisTitle: string = "";
    showLegend: boolean = true;
    invertXAxis: boolean = false;
    invertYAxis: boolean = false;

    static fromJson(json: Igor.Json.JsonValue): VisualizationScatter {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new VisualizationScatter();
        obj.name = jsonObject['name'] as string;
        obj.subtitle = ('subtitle' in jsonObject && jsonObject['subtitle'] != null) ? jsonObject['subtitle'] as string : null;
        obj.description = jsonObject['description'] as string;
        obj.series = Igor.Json.List(ScatterSerie).fromJson(jsonObject['series']);
        obj.showXAxis = ('show_x_axis' in jsonObject && jsonObject['show_x_axis'] != null) ? jsonObject['show_x_axis'] as boolean : true;
        obj.showYAxis = ('show_y_axis' in jsonObject && jsonObject['show_y_axis'] != null) ? jsonObject['show_y_axis'] as boolean : true;
        obj.showXGridLine = ('show_x_grid_line' in jsonObject && jsonObject['show_x_grid_line'] != null) ? jsonObject['show_x_grid_line'] as boolean : true;
        obj.showYGridLine = ('show_y_grid_line' in jsonObject && jsonObject['show_y_grid_line'] != null) ? jsonObject['show_y_grid_line'] as boolean : true;
        obj.xAxisType = AxisValueType.fromJson(jsonObject['x_axis_type']);
        obj.yAxisType = AxisValueType.fromJson(jsonObject['y_axis_type']);
        obj.xAxisTitle = ('x_axis_title' in jsonObject && jsonObject['x_axis_title'] != null) ? jsonObject['x_axis_title'] as string : "";
        obj.yAxisTitle = ('y_axis_title' in jsonObject && jsonObject['y_axis_title'] != null) ? jsonObject['y_axis_title'] as string : "";
        obj.showLegend = ('show_legend' in jsonObject && jsonObject['show_legend'] != null) ? jsonObject['show_legend'] as boolean : true;
        obj.invertXAxis = ('invert_x_axis' in jsonObject && jsonObject['invert_x_axis'] != null) ? jsonObject['invert_x_axis'] as boolean : false;
        obj.invertYAxis = ('invert_y_axis' in jsonObject && jsonObject['invert_y_axis'] != null) ? jsonObject['invert_y_axis'] as boolean : false;
        return obj;
    }

    static toJson(value: VisualizationScatter): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['kind'] = VisualizationKind.toJson(value.kind);
        result['name'] = value.name;
        if (value.subtitle != null) result['subtitle'] = value.subtitle;
        result['description'] = value.description;
        result['series'] = Igor.Json.List(ScatterSerie).toJson(value.series);
        result['show_x_axis'] = value.showXAxis;
        result['show_y_axis'] = value.showYAxis;
        result['show_x_grid_line'] = value.showXGridLine;
        result['show_y_grid_line'] = value.showYGridLine;
        result['x_axis_type'] = AxisValueType.toJson(value.xAxisType);
        result['y_axis_type'] = AxisValueType.toJson(value.yAxisType);
        result['x_axis_title'] = value.xAxisTitle;
        result['y_axis_title'] = value.yAxisTitle;
        result['show_legend'] = value.showLegend;
        result['invert_x_axis'] = value.invertXAxis;
        result['invert_y_axis'] = value.invertYAxis;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return VisualizationScatter.toJson(this);
    }
}

export class VisualizationBoxPlot extends Visualization implements IShowAxis, IShowGridlines, IAxisTypes, IAxisTitles, ILegend {
    kind: VisualizationKind = VisualizationKind.BoxPlot;
    name!: string;
    subtitle?: string | null = null;
    description!: string;
    showXAxis: boolean = true;
    showYAxis: boolean = true;
    showXGridLine: boolean = true;
    showYGridLine: boolean = true;
    xAxisType!: AxisValueType;
    yAxisType!: AxisValueType;
    xAxisTitle: string = "";
    yAxisTitle: string = "";
    showLegend: boolean = true;

    static fromJson(json: Igor.Json.JsonValue): VisualizationBoxPlot {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new VisualizationBoxPlot();
        obj.name = jsonObject['name'] as string;
        obj.subtitle = ('subtitle' in jsonObject && jsonObject['subtitle'] != null) ? jsonObject['subtitle'] as string : null;
        obj.description = jsonObject['description'] as string;
        obj.showXAxis = ('show_x_axis' in jsonObject && jsonObject['show_x_axis'] != null) ? jsonObject['show_x_axis'] as boolean : true;
        obj.showYAxis = ('show_y_axis' in jsonObject && jsonObject['show_y_axis'] != null) ? jsonObject['show_y_axis'] as boolean : true;
        obj.showXGridLine = ('show_x_grid_line' in jsonObject && jsonObject['show_x_grid_line'] != null) ? jsonObject['show_x_grid_line'] as boolean : true;
        obj.showYGridLine = ('show_y_grid_line' in jsonObject && jsonObject['show_y_grid_line'] != null) ? jsonObject['show_y_grid_line'] as boolean : true;
        obj.xAxisType = AxisValueType.fromJson(jsonObject['x_axis_type']);
        obj.yAxisType = AxisValueType.fromJson(jsonObject['y_axis_type']);
        obj.xAxisTitle = ('x_axis_title' in jsonObject && jsonObject['x_axis_title'] != null) ? jsonObject['x_axis_title'] as string : "";
        obj.yAxisTitle = ('y_axis_title' in jsonObject && jsonObject['y_axis_title'] != null) ? jsonObject['y_axis_title'] as string : "";
        obj.showLegend = ('show_legend' in jsonObject && jsonObject['show_legend'] != null) ? jsonObject['show_legend'] as boolean : true;
        return obj;
    }

    static toJson(value: VisualizationBoxPlot): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['kind'] = VisualizationKind.toJson(value.kind);
        result['name'] = value.name;
        if (value.subtitle != null) result['subtitle'] = value.subtitle;
        result['description'] = value.description;
        result['show_x_axis'] = value.showXAxis;
        result['show_y_axis'] = value.showYAxis;
        result['show_x_grid_line'] = value.showXGridLine;
        result['show_y_grid_line'] = value.showYGridLine;
        result['x_axis_type'] = AxisValueType.toJson(value.xAxisType);
        result['y_axis_type'] = AxisValueType.toJson(value.yAxisType);
        result['x_axis_title'] = value.xAxisTitle;
        result['y_axis_title'] = value.yAxisTitle;
        result['show_legend'] = value.showLegend;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return VisualizationBoxPlot.toJson(this);
    }
}

export class VisualizationRaw extends Visualization {
    kind: VisualizationKind = VisualizationKind.Raw;
    name!: string;
    subtitle?: string | null = null;
    description!: string;

    static fromJson(json: Igor.Json.JsonValue): VisualizationRaw {
        const jsonObject = json as Igor.Json.JsonObject;
        const obj = new VisualizationRaw();
        obj.name = jsonObject['name'] as string;
        obj.subtitle = ('subtitle' in jsonObject && jsonObject['subtitle'] != null) ? jsonObject['subtitle'] as string : null;
        obj.description = jsonObject['description'] as string;
        return obj;
    }

    static toJson(value: VisualizationRaw): Igor.Json.JsonValue {
        const result: Igor.Json.JsonObject = {};
        result['kind'] = VisualizationKind.toJson(value.kind);
        result['name'] = value.name;
        if (value.subtitle != null) result['subtitle'] = value.subtitle;
        result['description'] = value.description;
        return result;
    }

    toJson(): Igor.Json.JsonValue {
        return VisualizationRaw.toJson(this);
    }
}
