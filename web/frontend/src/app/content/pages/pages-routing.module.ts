import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { PagesComponent } from './pages.component';
import { ErrorPageComponent } from './error-page/error-page.component';
import { AuthorizedGuard } from '../../core/guards/authorized.guard';
import { NotAuthorizedGuard } from '../../core/guards/not-authorized.guard';
import { SuperadminGuard } from '../../core/guards/superadmin.guard';

const routes: Routes = [
    {
        path: '',
        component: PagesComponent,
        canActivate: [AuthorizedGuard],
        children: [
            {
                path: '',
                redirectTo: 'dashboard',
                pathMatch: 'full'
            },
            {
                path: 'dashboard',
                loadChildren: () => import('./dashboard/dashboard.module').then(m => m.DashboardModule)
            },
            {
                path: 'analytics',
                loadChildren: () => import('./analytics/analytics.module').then(m => m.AnalyticsModule)
            },
            {
                path: 'events',
                loadChildren: () => import('./events/events.module').then(m => m.EventsModule)
            },
            {
                path: 'patch-notes',
                loadChildren: () => import('./patch-notes/patch-notes.module').then(m => m.PatchNotesModule)
            },
            {
                path: 'personnel',
                loadChildren: () => import('./personnel/personnel.module').then(m => m.PersonnelModule),
                canActivate: [SuperadminGuard],
            },
            {
                path: 'settings',
                loadChildren: () => import('./settings/settings.module').then(m => m.SettingsModule),
                canActivate: [SuperadminGuard],
            },
        ]
    },
    {
        path: 'login',
        canActivate: [NotAuthorizedGuard],
        loadChildren: () => import('./auth/auth.module').then(m => m.AuthModule)
    },
    {
        path: '404',
        component: ErrorPageComponent
    },
    {
        path: 'error/:type',
        component: ErrorPageComponent
    },
];

@NgModule({
    imports: [
        RouterModule.forChild(routes)
    ],
    exports: [
        RouterModule
    ]
})
export class PagesRoutingModule { }
