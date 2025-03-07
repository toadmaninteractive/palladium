import { RouterModule } from '@angular/router';
import { NgModule } from '@angular/core';
import { CommonModule, DatePipe } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatButtonModule } from '@angular/material/button';
import { MatCardModule } from '@angular/material/card';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatChipsModule } from '@angular/material/chips';
import { MatNativeDateModule, MatRippleModule } from '@angular/material/core';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatDialogModule } from '@angular/material/dialog';
import { MatGridListModule } from '@angular/material/grid-list';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatListModule } from '@angular/material/list';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatProgressBarModule } from '@angular/material/progress-bar';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';
import { MatSelectModule } from '@angular/material/select';
import { MatSlideToggleModule } from '@angular/material/slide-toggle';
import { MatSortModule } from '@angular/material/sort';
import { MatTableModule } from '@angular/material/table';
import { MatTabsModule } from '@angular/material/tabs';
import { MatTooltipModule } from '@angular/material/tooltip';
import { CsvModule } from '@ctrl/ngx-csv';
import { MarkdownModule } from 'ngx-markdown';
import { PerfectScrollbarModule } from 'ngx-perfect-scrollbar';
import { SatDatepickerModule, SatNativeDateModule } from 'saturn-datepicker';

import { ChartsModule } from './charts/charts.module';
import { AlertModule } from './general/alert/alert.module';
import { GravatarModule } from './general/gravatar/gravatar.module';
import { LoadingIndicatorModule } from './general/loading-indicator/loading-indicator/loading-indicator.module';
import { PortletModule } from './general/portlet/portlet.module';
import { ListTimelineModule } from './layout/quick-sidebar/list-timeline/list-timeline.module';
import { MessengerModule } from './layout/quick-sidebar/messenger/messenger.module';
import { SharedModule } from '../shared/shared.module';

import { AnalyticsErrorComponent } from './general/analytics-error/analytics-error.component';
import { NoticeComponent } from './general/notice/notice.component';
import { ChartBoxComponent } from './layout/chart-box/chart-box.component';
import { QuickSidebarComponent } from './layout/quick-sidebar/quick-sidebar.component';
import { ListSettingsComponent } from './layout/quick-sidebar/list-settings/list-settings.component';
import { ScrollTopComponent } from './layout/scroll-top/scroll-top.component';
import { TooltipsComponent } from './layout/tooltips/tooltips.component';
import { WidgetBoxComponent } from './layout/widget-box/widget-box.component';
import { ParamBoxComponent } from './widgets/analytics/param-box/param-box.component';
import { KpiWidgetComponent } from './widgets/dashboard/kpi-widget/kpi-widget.component';
import { MultiValueCardComponent } from './widgets/dashboard/multi-value-card/multi-value-card.component';
import { ValueCardComponent } from './widgets/dashboard/value-card/value-card.component';

@NgModule({
    providers: [
        DatePipe,
    ],
    declarations: [
        QuickSidebarComponent,
        ScrollTopComponent,
        TooltipsComponent,
        ListSettingsComponent,
        NoticeComponent,
        ValueCardComponent,
        MultiValueCardComponent,
        ParamBoxComponent,
        ChartBoxComponent,
        AnalyticsErrorComponent,
        KpiWidgetComponent,
        WidgetBoxComponent,
    ],
    exports: [
        CsvModule,
        MatTabsModule,
        MatTableModule,
        MatPaginatorModule,
        QuickSidebarComponent,
        ScrollTopComponent,
        TooltipsComponent,
        ChartBoxComponent,
        ListSettingsComponent,
        NoticeComponent,
        ValueCardComponent,
        PortletModule,
        AlertModule,
        GravatarModule,
        ChartsModule,
        LoadingIndicatorModule,
        MultiValueCardComponent,
        ParamBoxComponent,
        AnalyticsErrorComponent,
        KpiWidgetComponent,
        WidgetBoxComponent,
    ],
    imports: [
        CommonModule,
        RouterModule,
        PerfectScrollbarModule,
        MessengerModule,
        ListTimelineModule,
        SharedModule,
        PortletModule,
        AlertModule,
        GravatarModule,
        ChartsModule,
        LoadingIndicatorModule,
        FormsModule,
        ReactiveFormsModule,
        MatButtonModule,
        MatChipsModule,
        MatCheckboxModule,
        MatDatepickerModule,
        MatIconModule,
        MatInputModule,
        MatNativeDateModule,
        MatPaginatorModule,
        MatProgressBarModule,
        MatProgressSpinnerModule,
        MatSelectModule,
        MatSortModule,
        MatTableModule,
        MatTabsModule,
        MatTooltipModule,
        MatCardModule,
        MatListModule,
        MatRippleModule,
        MatSlideToggleModule,
        MatDialogModule,
        MatGridListModule,
        SatDatepickerModule,
        SatNativeDateModule,
        CsvModule,
        MarkdownModule,
    ],
})
export class ComponentsModule { }
