import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatCardModule } from '@angular/material/card';
import { MatTooltipModule } from '@angular/material/tooltip';
import { D3DashboardPlotComponent } from './d3-dashboard-plot.component';
import { SharedModule } from '../../../shared/shared.module';
import { LoadingIndicatorModule } from '../../general/loading-indicator/loading-indicator/loading-indicator.module';

@NgModule({
    imports: [
        CommonModule,
        MatCardModule,
        MatTooltipModule,
        SharedModule,
        LoadingIndicatorModule,
    ],
    declarations: [
        D3DashboardPlotComponent,
    ],
    exports: [
        D3DashboardPlotComponent,
    ],
    providers: []
})
export class D3DashboardPlotModule {}
