import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { MatChipsModule } from '@angular/material/chips';
import { MatDialogModule } from '@angular/material/dialog';
import { MatIconModule } from '@angular/material/icon';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatSortModule } from '@angular/material/sort';
import { MatTableModule } from '@angular/material/table';
import { MatTooltipModule } from '@angular/material/tooltip';
import { PerfectScrollbarModule } from 'ngx-perfect-scrollbar';
import { SharedModule } from '../../../shared/shared.module';
import { PortletModule } from '../../../components/general/portlet/portlet.module';
import { ProjectResolverGuard } from '../../../core/guards/project-resolver.guard';
import { EventsDialogComponent, EventsOverviewComponent } from './events-overview/events-overview.component';

@NgModule({
    imports: [
        CommonModule,
        MatChipsModule,
        MatDialogModule,
        MatIconModule,
        MatPaginatorModule,
        MatSortModule,
        MatTableModule,
        MatTooltipModule,
        PerfectScrollbarModule,
        SharedModule,
        PortletModule,
        RouterModule.forChild([
            {
                path: '**',
                component: EventsOverviewComponent,
                resolve: {
                    activeProject: ProjectResolverGuard,
                },
            },
        ])
    ],
    exports: [],
    providers: [],
    declarations: [
        EventsOverviewComponent,
        EventsDialogComponent,
    ],
    entryComponents: [
        EventsDialogComponent,
    ]
})
export class EventsModule { }
