import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { MatIconModule } from '@angular/material/icon';
import { MatTooltipModule } from '@angular/material/tooltip';
import { PerfectScrollbarModule } from 'ngx-perfect-scrollbar';
import { ProjectResolverGuard } from '../../../core/guards/project-resolver.guard';
import { SharedModule } from '../../../shared/shared.module';
import { PortletModule } from '../../../components/general/portlet/portlet.module';
import { ComponentsModule } from '../../../components/components.module';
import { DashboardOverviewComponent } from './dashboard-overview.component';
import { D3BoxPlotModule } from "../../../components/charts/d3-box-plot/d3-box-plot.module";

@NgModule({
    imports: [
        CommonModule,
        MatIconModule,
        MatTooltipModule,
        PerfectScrollbarModule,
        SharedModule,
        PortletModule,
        ComponentsModule,
        RouterModule.forChild([
            {
                path: '**',
                component: DashboardOverviewComponent,
                resolve: {
                    activeProject: ProjectResolverGuard,
                },
            },
        ]),
        D3BoxPlotModule
    ],
    exports: [],
    providers: [],
    declarations: [
        DashboardOverviewComponent
    ]
})
export class DashboardModule { }
