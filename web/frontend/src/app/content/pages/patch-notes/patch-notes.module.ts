import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { PerfectScrollbarModule } from 'ngx-perfect-scrollbar';
import { SharedModule } from '../../../shared/shared.module';
import { PortletModule } from '../../../components/general/portlet/portlet.module';
import { ProjectResolverGuard } from '../../../core/guards/project-resolver.guard';
import { PatchNotesOverviewComponent } from './patch-notes-overview/patch-notes-overview.component';
import { MatIconModule } from '@angular/material/icon';
import { MarkdownModule } from 'node_modules/ngx-markdown';

@NgModule({
    imports: [
        CommonModule,
        PerfectScrollbarModule,
        SharedModule,
        PortletModule,
        MarkdownModule.forChild(),
        RouterModule.forChild([
            {
                path: '**',
                component: PatchNotesOverviewComponent,
                resolve: {
                    activeProject: ProjectResolverGuard,
                },
            },
        ]),
        MatIconModule
    ],
    exports: [],
    providers: [],
    declarations: [
        PatchNotesOverviewComponent,
    ],
})
export class PatchNotesModule { }
