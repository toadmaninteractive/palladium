import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { RouterModule } from '@angular/router';
import { MatAutocompleteModule } from '@angular/material/autocomplete';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatMenuModule } from '@angular/material/menu';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatSelectModule } from '@angular/material/select';
import { MatSlideToggleModule } from '@angular/material/slide-toggle';
import { MatSortModule } from '@angular/material/sort';
import { MatTableModule } from '@angular/material/table';
import { MatTooltipModule } from '@angular/material/tooltip';
import { NgxMatSelectSearchModule } from 'ngx-mat-select-search';
import { PerfectScrollbarModule } from 'ngx-perfect-scrollbar';
import { SharedModule } from '../../../shared/shared.module';
import { PortletModule } from '../../../components/general/portlet/portlet.module';
import { ProjectResolverGuard } from '../../../core/guards/project-resolver.guard';
import { AnalyticsOverviewComponent } from './analytics-overview/analytics-overview.component';
import { ComponentsModule } from '../../../components/components.module';
import { D3BoxPlotModule } from "../../../components/charts/d3-box-plot/d3-box-plot.module";

@NgModule({
    imports: [
        CommonModule,
        PerfectScrollbarModule,
        SharedModule,
        PortletModule,
        ComponentsModule,
        FormsModule,
        ReactiveFormsModule,
        MatAutocompleteModule,
        MatFormFieldModule,
        MatIconModule,
        MatInputModule,
        MatMenuModule,
        MatPaginatorModule,
        MatTableModule,
        MatTooltipModule,
        MatSelectModule,
        MatSlideToggleModule,
        MatSortModule,
        NgxMatSelectSearchModule,
        RouterModule.forChild([
            {
                path: '**',
                component: AnalyticsOverviewComponent,
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
        AnalyticsOverviewComponent,
    ]
})
export class AnalyticsModule { }
