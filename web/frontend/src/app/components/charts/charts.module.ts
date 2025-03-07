import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { D3FunnelModule } from './d3-funnel/d3-funnel.module';
import { D3DashboardPlotModule } from './d3-dashboard-plot/d3-dashboard-plot.module';
import { D3HorizontalBarsModule } from './d3-horizontal-bars/d3-horizontal-bars.module';
import { D3KillmapModule } from './d3-killmap/d3-killmap.module';
import { D3HeatmapModule } from './d3-heatmap/d3-heatmap.module';
import { D3PlotModule } from './d3-plot/d3-plot.module';
import { D3SegmentedFunnelModule } from './d3-segmented-funnel/d3-segmented-funnel.module';
import { LoadingIndicatorModule } from '../general/loading-indicator/loading-indicator/loading-indicator.module';
import { D3SynchronizedModule } from './d3-synchronized/d3-synchronized.module';
import { D3ScatterplotModule } from './d3-scatterplot/d3-scatterplot.module';

@NgModule({
    imports: [
        CommonModule,
        LoadingIndicatorModule,
    ],
    declarations: [],
    exports: [
        D3DashboardPlotModule,
        D3FunnelModule,
        D3HorizontalBarsModule,
        D3KillmapModule,
        D3HeatmapModule,
        D3PlotModule,
        D3SegmentedFunnelModule,
        D3SynchronizedModule,
        D3ScatterplotModule,
        D3ScatterplotModule
    ]
})
export class ChartsModule { }
