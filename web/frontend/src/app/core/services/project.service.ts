import { Injectable } from '@angular/core';
import { NavigationEnd, Router, RouterEvent } from '@angular/router';
import { BehaviorSubject, combineLatest } from 'rxjs';
import { filter, map, switchMap, distinctUntilChanged, first, startWith } from 'rxjs/operators';
import * as objectPath from 'object-path';
import { MenuConfigService } from './metronic/menu-config.service';
import { AccountService } from './account.service';
import { StorageService } from './storage.service';
import { PalladiumQueryService } from '../../protocol/web-query-protocol.service';
import * as WebProtocol from '../../protocol/web-protocol';

@Injectable({
    providedIn: 'root',
})
export class ProjectService {
    projects$ = new BehaviorSubject<WebProtocol.ProjectConfig[] | null>(null);
    activeProject$ = new BehaviorSubject<WebProtocol.ProjectConfig | null>(null);
    activeDatabase$ = new BehaviorSubject<string | null>(null);

    constructor(
        private router: Router,
        private menuConfigService: MenuConfigService,
        private accountService: AccountService,
        private storageService: StorageService,
        private palladiumQueryService: PalladiumQueryService,
    ) {
        // Request projects if user is signed in
        this.accountService.isSignedIn()
            .pipe(
                filter(isSignedIn => isSignedIn === true),
                distinctUntilChanged(),
                switchMap(_ => this.palladiumQueryService.getProjects()),
                map(response => response.items),
            )
            .subscribe((items: WebProtocol.ProjectConfig[]) => this.projects$.next(items));

        this.projects$.asObservable()
            .pipe(
                filter(items => items instanceof Array),
                first(),
            )
            .subscribe(items => {
                const url = this.router.routerState.snapshot.url.split(/[?#]/)[0];
                this.activeProject$.next(this.getActiveProject(url));
                this.activeDatabase$.next(this.getActiveDatabase(url));
            });

        combineLatest(
            this.router.events.pipe(
                startWith(this.router.routerState.snapshot.url),
                filter((eventOrUrl: string | RouterEvent) => typeof eventOrUrl === 'string' || eventOrUrl instanceof NavigationEnd),
                map((eventOrUrl: string | NavigationEnd) => typeof eventOrUrl === 'string' ? eventOrUrl : (eventOrUrl.urlAfterRedirects || eventOrUrl.url)),
                map(url => decodeURI(url).split(/[?#]/)[0]),
            ),
            this.projects$.asObservable().pipe(
                filter(items => items instanceof Array),
            ),
        ).subscribe(([url, projects]) => {
            this.activeProject$.next(this.getActiveProject(url));
            this.activeDatabase$.next(this.getActiveDatabase(url));
        });

        // Monitor active project and database
        this.activeProject$
            .pipe(filter(project => project instanceof WebProtocol.ProjectConfig))
            .subscribe(project => this.storageService.setLastProjectId(project.id));

        this.activeDatabase$
            .pipe(filter(database => typeof database === 'string'))
            .subscribe(database => this.storageService.setLastDatabase(database));
    }

    getActiveProject(url: string): WebProtocol.ProjectConfig | null {
        const projects = this.projects$.getValue();
        let projectId = this.storageService.getLastProjectId();

        if (this.isDynamicRoute(url)) {
            const path = url.split(/[?#]/)[0],
                parts = path.split(/\//g);

            if (parts.length > 2) {
                projectId = parts[2];
            }
        }

        if (!projectId && projects && projects.length > 0) {
            projectId = projects[0].id;
        }

        return (!projectId || !projects)
            ? null
            : projects.filter(project => project.id === projectId)[0] || null;
    }

    getActiveDatabase(url: string): string | null {
        const projects = this.projects$.getValue();
        let database = this.storageService.getLastDatabase();

        if (this.isDynamicRoute(url)) {
            const path = url.split(/[?#]/)[0],
                parts = path.split(/\//g);

            if (parts.length > 3) {
                database = parts[3];
            }
        }

        if (!database && projects && projects.length > 0) {
            database = projects[0].databases[0];
        }

        return database;
    }

    isDynamicRoute(url: string): boolean {
        // Get menu items
        const menuItems = objectPath.get(this.menuConfigService.configModel, 'config.aside.items');

        if (!(menuItems instanceof Array)) {
            return false;
        }

        const qs = url.split(/[?#]/)[1] || '';
        url = url.split(/[?#]/)[0];

        return menuItems.filter(item => item.dynamic && url.startsWith(item.page.replace(/\/:(project|database)/g, ''))).length > 0;
    }
}
