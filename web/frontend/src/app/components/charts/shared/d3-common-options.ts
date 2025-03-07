import { AxisValueType } from '../../../protocol/visualization-protocol';

export enum D3AxisValueType {
    Numeric = 1,
    Date = 2,
    Text = 3,
}

export enum D3AxisPosition {
    Primary,
    Secondary,
}

export enum D3ChartType {
    Bar,
    Area,
    AreaSpline,
    Line,
    Spline,
}

export interface D3NumericPoint {
    label: string | Date;
    value: number;
}

export interface D3ScatterPoint {
    x: string;
    y: string;
    value: number;
    seriesKey?: string;
    radius?: number;
}

export interface D3SeriesDataSet {
    data: Array<D3NumericPoint>;
    seriesType: D3ChartType;
    seriesTitle: string;
    seriesKey?: string;
    xAxisPosition?: D3AxisPosition;
    yAxisPosition?: D3AxisPosition;
    addDots?: boolean;
    showValues?: boolean;
    groupIndex?: number;
    seriesColor?: string;
}

export interface D3ScatterDataSet {
    data: Array<D3ScatterPoint>;
    seriesTitle: string;
    seriesKey?: string;
    showValues?: boolean;
    seriesColor?: string;
}

export interface D3Options {
    title?: string;
    subtitle?: string;
    description?: string;
    showTooltip?: boolean;
    showLegend?: boolean;
    showTimeline?: boolean;
    showVerticalGrid?: boolean;
    showHorizontalGrid?: boolean;

    showXPrimaryAxis?: boolean;
    showYPrimaryAxis?: boolean;
    xPrimaryValueType: AxisValueType | D3AxisValueType | null;
    yPrimaryValueType: AxisValueType | D3AxisValueType | null;
    xPrimaryAxisTitle: string | null;
    yPrimaryAxisTitle: string | null;

    showXSecondaryAxis?: boolean;
    showYSecondaryAxis?: boolean;
    xSecondaryValueType: AxisValueType | D3AxisValueType | null;
    ySecondaryValueType: AxisValueType | D3AxisValueType | null;
    xSecondaryAxisTitle: string | null;
    ySecondaryAxisTitle: string | null;
}

export interface D3BarPoint {
    key: string;
    value: number;
}

export interface D3BarSeries {
    label: string;
    values: D3BarPoint[];
}
