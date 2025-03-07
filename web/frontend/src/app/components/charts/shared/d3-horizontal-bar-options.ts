import * as D3Common from './d3-common-options';

export class D3HorizontalBarOptions {
    series: Array<D3Common.D3SeriesDataSet>;
    key: string;
    name: string;
    description: string;
    showTooltip = true;
    showLegend = true;
    showValues = true;
    showVerticalGrid = true;
    showHorizontalGrid = false;
    showXPrimaryAxis = true;
    showYPrimaryAxis = true;
    xPrimaryValueType: D3Common.D3AxisValueType | null = null;
    yPrimaryValueType: D3Common.D3AxisValueType | null = null;
    xPrimaryAxisTitle: string | null  = null;
    yPrimaryAxisTitle: string | null  = null;
}
