import * as DataProtocol from '../../protocol/data-protocol';

export enum AxisValueType {
    Numeric,
    Date,
    Text
}

export enum AxisPosition {
    Primary,
    Secondary,
}

export enum ChartType {
    Bar,
    Area,
    AreaSpline,
    Line,
    Spline,
    HorizontalBar,
    MultipleBar
}

export interface SeriesDataSet {
    data: Array<DataProtocol.NumericPoint>;
    seriesType: ChartType;
    seriesTitle: string;
    seriesKey?: string;
    xAxisPosition: AxisPosition;
    yAxisPosition: AxisPosition;
    addDots?: boolean;
}

export class ChartConfig {
    series: Array<SeriesDataSet>;
    key: string;
    showTooltip = false;
    showLegend = true;
    showValues = true;
    addVerticalGrid = true;
    addHorizontalGrid = true;
    xPrimaryValueType: AxisValueType | null = null;
    yPrimaryValueType: AxisValueType | null = null;
    xPrimaryAxisTitle: string | null  = null;
    xPrimaryAxisLabelsRotate: number | null  = null;
    yPrimaryAxisTitle: string | null  = null;
    xPrimaryAxisLabels: Array<string> | null = null;
    yPrimaryAxisLabels: Array<string> | null = null;
    xSecondaryValueType: AxisValueType | null = null;
    ySecondaryValueType: AxisValueType | null  = null;
    xSecondaryAxisTitle: string | null  = null;
    ySecondaryAxisTitle: string | null  = null;
    xSecondaryAxisLabels: Array<string> | null = null;
    ySecondaryAxisLabels: Array<string> | null = null;
}

