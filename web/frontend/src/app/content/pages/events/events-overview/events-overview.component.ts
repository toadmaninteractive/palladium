import { ChangeDetectionStrategy, ChangeDetectorRef, Component, OnDestroy, ViewChild, AfterViewInit, Inject } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { MatDialog } from '@angular/material/dialog';
import { MatPaginator } from '@angular/material/paginator';
import { MatSort, Sort } from '@angular/material/sort';
import { MatTableDataSource } from '@angular/material/table';
import { Subject, Subscription } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { compare } from '../../../../shared/functions/compare';
import { PalladiumQueryService } from '../../../../protocol/web-query-protocol.service';
import * as CardProtocol from '../../../../protocol/card-protocol';
import * as WebProtocol from '../../../../protocol/web-protocol';

@Component({
    selector: 'm-events-overview',
    templateUrl: './events-overview.component.html',
    styleUrls: ['./events-overview.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class EventsOverviewComponent implements AfterViewInit, OnDestroy {
    @ViewChild(MatPaginator, {static: true}) paginator: MatPaginator;
    @ViewChild(MatSort, {static: true}) sort: MatSort;
    destroy$ = new Subject<any>();
    project: WebProtocol.ProjectConfig;
    dataSource: MatTableDataSource<CardProtocol.CardEvent>;
    eventData: WebProtocol.EventData;
    displayedColumns = ['name', 'group', 'status', 'addedAt', 'fields'];
    pageSize = 20;
    pageSizes = [10, 20, 50, 100];
    subEvents: Subscription;

    constructor (
        private route: ActivatedRoute,
        private cdr: ChangeDetectorRef,
        public dialog: MatDialog,
        private queryService: PalladiumQueryService,
    ) {
        this.route.data
            .pipe(takeUntil(this.destroy$))
            .subscribe((data) => this.initialize(data.activeProject.project));
    }

    ngAfterViewInit(): void {
        if (this.eventData) {
            this.updateEvents(this.eventData);
        }
    }

    ngOnDestroy(): void {
        if (this.subEvents instanceof Subscription) {
            this.subEvents.unsubscribe();
        }

        this.destroy$.next();
        this.destroy$.complete();
    }

    private initialize(project: WebProtocol.ProjectConfig): void {
        this.project = project;
        this.getEvents(this.project.id);
    }

    private updateEvents(eventData: WebProtocol.EventData): void {
        this.eventData = eventData;
        const eventGroupMap = new Map<string, Array<string>>();

        this.eventData.groups.forEach(group => {
            group.events.forEach(eventId => {
                const eventGroupTags = eventGroupMap.has(eventId) ? eventGroupMap.get(eventId) : [];
                eventGroupTags.push(group.name);
                eventGroupMap.set(eventId, eventGroupTags);
            });
        });

        this.eventData.events.forEach(event => {
            const groupTags = eventGroupMap.has(event.id) ? eventGroupMap.get(event.id) : [];
            event['group'] = groupTags.join(', ');
        });

        this.dataSource = new MatTableDataSource<CardProtocol.CardEvent>([]);
        this.dataSource.paginator = this.paginator;
        this.dataSource.sort = this.sort;
        this.sortTable(<Sort> { active: 'name', direction: 'asc' });
    }

    private getEvents(projectKey: string): void {
        if (this.subEvents instanceof Subscription) {
            this.subEvents.unsubscribe();
        }

        this.subEvents = this.queryService
            .getEvents(projectKey)
            .pipe(takeUntil(this.destroy$))
            .subscribe(eventData => this.updateEvents(eventData));
    }

    sortTable(sort: Sort): void {
        const data = this.eventData.events.slice();

        if (!sort.active || sort.direction === '') {
            this.dataSource.data = data;
            return;
        }

        this.dataSource.data = data.sort((a, b) => {
            const isAsc = sort.direction === 'asc';

            switch (sort.active) {
                case 'name': return compare(a.name, b.name, isAsc);
                case 'group': return compare(a['group'], b['group'], isAsc);
                case 'addedAt': return compare(a.addedAt.getTime(), b.addedAt.getTime(), isAsc);
                default: return 0;
            }
        });

        setTimeout(() => this.cdr.detectChanges());
    }

    openDialog(): void {
        const dialogRef = this.dialog.open(EventsDialogComponent);
    }
}

@Component({
    selector: 'm-events-dialog',
    templateUrl: 'events-dialog.html',
})
export class EventsDialogComponent { }
