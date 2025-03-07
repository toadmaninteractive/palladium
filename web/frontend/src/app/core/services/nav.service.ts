import { Injectable } from '@angular/core';
import { NavigationEnd, Router, RouterEvent } from '@angular/router';
import { combineLatest } from 'rxjs';
import { filter, map, startWith } from 'rxjs/operators';
import { ProjectService } from './project.service';
import * as WebProtocol from '../../protocol/web-protocol';

@Injectable({
    providedIn: 'root',
})
export class NavService {
    constructor(
        private router: Router,
        private projectService: ProjectService,
    ) {
        combineLatest(
            this.router.events.pipe(
                startWith(this.router.routerState.snapshot.url),
                filter((eventOrUrl: string | RouterEvent) => typeof eventOrUrl === 'string' || eventOrUrl instanceof NavigationEnd),
                map((eventOrUrl: string | NavigationEnd) => typeof eventOrUrl === 'string' ? eventOrUrl : (eventOrUrl.urlAfterRedirects || eventOrUrl.url)),
                map(url => decodeURI(url).split(/[?#]/)[0]),
            ),
            this.projectService.activeProject$.asObservable().pipe(filter(project => project instanceof WebProtocol.ProjectConfig)),
            this.projectService.activeDatabase$.asObservable().pipe(filter(database => typeof database === 'string')),
        ).subscribe(([url, project, database]) => {
            if (this.projectService.isDynamicRoute(url)) {
                const path = url.split(/[?#]/)[0],
                    parts = path.split(/\//g);

                if (parts.length < 4) {
                    const db = project.databases.indexOf(database) !== -1 ? database : project.databases[0];
                    this.switchProject(project.id, db);
                }
            }
        });
    }

    switchProject(projectId: string = null, database: string = null, currentUrl: string = null): void {
        let url = currentUrl ? currentUrl : this.router.routerState.snapshot.url;
        const qs = url.split(/[?#]/)[1] || '';
        url = url.split(/[?#]/)[0];

        const isDynamicRoute = this.projectService.isDynamicRoute(url);

        if (!isDynamicRoute) {
            return;
        }

        const urlParts = url.split(/\//g);

        while (urlParts.length < 4) {
            urlParts.push('');
        }

        // Check if query string can be kept (only database is changed)
        const keepQs = urlParts[2] === projectId && urlParts[3] !== database;

        urlParts[2] = projectId;
        urlParts[3] = database;

        this.router.navigateByUrl(urlParts.join('/') + ((qs && keepQs) ? `?${qs}` : ''));
    }

    switchDatabase(database: string = null): void {
        this.switchProject(this.projectService.activeProject$.getValue().id, database, null);
    }
}
