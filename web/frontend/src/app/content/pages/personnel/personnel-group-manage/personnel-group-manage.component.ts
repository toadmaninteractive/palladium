import { ChangeDetectionStrategy, ChangeDetectorRef, Component, OnDestroy } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { BehaviorSubject, combineLatest, Observable, Subject, Subscription } from 'rxjs';
import { debounceTime, distinctUntilChanged, filter, finalize, map, switchMap, takeUntil, tap } from 'rxjs/operators';
import { PageEvent } from '@angular/material/paginator';
import { Sort } from '@angular/material/sort';
import { MatTableDataSource } from '@angular/material/table';
import { AlertMessage } from '../../../../shared/interfaces/alert-message';
import { Breadcrumb } from '../../../../shared/interfaces/breadcrumb';
import { SubheaderService } from '../../../../core/services/metronic/layout/subheader.service';
import { ClipboardService } from '../../../../core/services/clipboard.service';
import { NotificationService } from '../../../../core/services/notification.service';
import { PalladiumAdminService } from '../../../../protocol/web-admin-protocol.service';
import * as DataProtocol from '../../../../protocol/data-protocol';
import * as WebProtocol from '../../../../protocol/web-protocol';

interface QueryArguments {
    groupId: number;
    needle: string;
    orderBy: WebProtocol.PersonnelGroupRoleOrderBy;
    orderDir: DataProtocol.OrderDirection;
    limit: number;
    offset: number;
}

enum Column {
    ProjectId = 'project_id',
    ProjectTitle = 'project_title',
    GroupRole = 'group_role',
    ProjectDbs = 'project_dbs',
}

const DEFAULT_ORDER_BY = Column.ProjectId,
    DEFAULT_ORDER_DIR = 'asc',
    DEFAULT_PAGE_SIZE = 10,
    DEFAULT_COLUMNS = [Column.ProjectId, Column.ProjectTitle, Column.GroupRole, Column.ProjectDbs];

@Component({
    selector: 'm-personnel-group-manage',
    templateUrl: './personnel-group-manage.component.html',
    styleUrls: ['./personnel-group-manage.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PersonnelGroupManageComponent implements OnDestroy {
    public config: any;
    destroy$ = new Subject();
    loading$ = new BehaviorSubject<boolean>(false);
    group$ = new BehaviorSubject<WebProtocol.PersonnelGroup>(null);
    filteredMembers$ = new BehaviorSubject<string[]>([]);
    alert$ = new BehaviorSubject<AlertMessage | null>(null);
    reload$ = new BehaviorSubject<boolean>(true);
    needle$ = new BehaviorSubject<string>('');
    memberNeedle$ = new BehaviorSubject<string>('');
    sort$ = new BehaviorSubject<Sort>(<Sort> { active: DEFAULT_ORDER_BY, direction: DEFAULT_ORDER_DIR });
    page$ = new BehaviorSubject<PageEvent>(<PageEvent> { pageIndex: 0, pageSize: DEFAULT_PAGE_SIZE });
    total$ = new BehaviorSubject<number>(0);
    dataSource$ = new BehaviorSubject<MatTableDataSource<WebProtocol.PersonnelGroupRole>>(new MatTableDataSource<WebProtocol.PersonnelGroupRole>());
    subData: Subscription;
    displayedColumns = [...DEFAULT_COLUMNS];
    column = Column;
    accessRole = WebProtocol.AccessRole;
    pageSizes = [10, 25, 50, 100];
    args: QueryArguments;

    constructor(
        private activatedRoute: ActivatedRoute,
        private router: Router,
        private cdr: ChangeDetectorRef,
        private clipboardService: ClipboardService,
        private subheaderService: SubheaderService,
        private palladiumAdminService: PalladiumAdminService,
        private notificationService: NotificationService,
    ) {
        combineLatest(
            this.activatedRoute.paramMap.pipe(
                map(params => params.get('name')),
                distinctUntilChanged(),
                switchMap(name => this.palladiumAdminService.getPersonnelGroupByName(name)),
                tap(group => this.onGroupChange(group)),
            ),
            this.reload$.asObservable(),
            this.needle$.asObservable().pipe(
                distinctUntilChanged(),
                debounceTime(450),
            ),
            this.sort$.asObservable(),
            this.page$.asObservable(),
        )
            .pipe(
                takeUntil(this.destroy$),
                map(([group, reload, needle, sort, page]) => <QueryArguments> {
                    groupId: group.id,
                    needle: needle,
                    orderBy: WebProtocol.PersonnelGroupRoleOrderBy.fromJson(sort.active || DEFAULT_ORDER_BY),
                    orderDir: DataProtocol.OrderDirection.fromJson(sort.direction || DEFAULT_ORDER_DIR),
                    limit: page.pageSize,
                    offset: page.pageIndex * page.pageSize,
                }),
                filter(args => {
                    const shouldRestart = !(this.args
                        && this.args.groupId === args.groupId
                        && this.args.needle === args.needle
                        && this.args.orderBy === args.orderBy
                        && this.args.orderDir === args.orderDir
                        && this.args.limit === args.limit);

                    this.args = args;

                    if (shouldRestart) {
                        this.page$.next(<PageEvent> { pageSize: args.limit, pageIndex: 0 });
                    }

                    return !shouldRestart;
                }),
            )
            .subscribe(args => this.loadItems(args));

        combineLatest(
            this.group$.asObservable().pipe(
                filter(group => group instanceof WebProtocol.PersonnelGroup),
            ),
            this.memberNeedle$.asObservable().pipe(
                debounceTime(50)
            )
        )
        .pipe(takeUntil(this.destroy$))
        .subscribe(([group, needle]) => {
            const trimmedNeedle = (needle || '').trim().toLocaleLowerCase(),
                members = group.members
                    .filter(member => !trimmedNeedle || member.toLocaleLowerCase().indexOf(trimmedNeedle) !== -1)
                    .sort((a, b) => a < b ? -1 : 1);

            this.filteredMembers$.next(members);
        });
    }

    ngOnDestroy(): void {
        if (this.subData instanceof Subscription) {
            this.subData.unsubscribe();
            this.subData = null;
        }

        this.destroy$.next();
        this.destroy$.complete();
        this.loading$.complete();
    }

    private getData(args: QueryArguments): Observable<WebProtocol.PersonnelGroupRole[]> {
        return this.palladiumAdminService
            .getPersonnelGroupRoles(args.groupId, args.needle, args.orderBy, args.orderDir, args.offset, args.limit)
            .pipe(
                takeUntil(this.destroy$),
                tap((response: DataProtocol.CollectionSlice<WebProtocol.PersonnelGroupRole>) => this.total$.next(response.total)),
                map((response: DataProtocol.CollectionSlice<WebProtocol.PersonnelGroupRole>) => response.items),
            );
    }

    loadItems(args: QueryArguments): void {
        // Show loading indicator
        this.loading$.next(true);

        if (this.subData instanceof Subscription) {
            this.subData.unsubscribe();
            this.subData = null;
        }

        // Load items
        this.subData = this.getData(args)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe(response => {
                const dataSource = new MatTableDataSource<WebProtocol.PersonnelGroupRole>();
                dataSource.data = response;
                this.dataSource$.next(dataSource);
            });
    }

    roleName(role: string | WebProtocol.AccessRole | any): string {
        const actualRole = typeof role === 'string' ? WebProtocol.AccessRole.fromJson(role) : role;
        return WebProtocol.AccessRole.getDescription(actualRole);
    }

    updatePageStatus(group: WebProtocol.PersonnelGroup): void {
        this.subheaderService.setTitle(group.name);

        this.subheaderService.setBreadcrumbs([
            <Breadcrumb>{ title: 'Personnel Groups', page: '/personnel/groups' },
            <Breadcrumb>{ title: group.name, page: `/personnel/groups/${group.name}` },
        ]);

        this.alert$.next(null);
        this.cdr.detectChanges();
    }

    onNeedleChange(needle: string): void {
        this.needle$.next((needle || '').trim());
    }

    onMemberNeedleChange(needle: string): void {
        this.memberNeedle$.next((needle || '').trim());
    }

    onSortChange(sort: Sort): void {
        this.sort$.next(sort);
    }

    onPageChange(page: PageEvent): void {
        this.page$.next(page);
    }

    onReload(): void {
        this.reload$.next(true);
    }

    onGroupChange(group: WebProtocol.PersonnelGroup): void {
        this.group$.next(group);
        this.updatePageStatus(group);
    }

    onRoleChange(obj: WebProtocol.PersonnelGroupRole, role: WebProtocol.AccessRole): void {
        if (obj.groupRole === role) {
            return;
        }

        this.setPersonnelGroupRole(obj.groupId, obj.projectId, role)
            .subscribe(response => {
                this.reload$.next(true);
                this.notificationService.success('Role changed');
            });
    }

    onRoleReset(obj: WebProtocol.PersonnelGroupRole): void {
        if (!obj.groupRole) {
            return;
        }

        this.resetPersonnelGroupRole(obj.groupId, obj.projectId)
            .subscribe(response => {
                this.reload$.next(true);
                this.notificationService.success('Role changed');
            });
    }

    onGlobalDbsChange(obj: WebProtocol.PersonnelGroupRole, checked: boolean): void {
        this.setPersonnelGroupRole(obj.groupId, obj.projectId, obj.groupRole, checked, [])
            .subscribe(response => {
                this.reload$.next(true);
                this.notificationService.success('Database access updated');
            });
    }

    onDbChange(obj: WebProtocol.PersonnelGroupRole, db: string, checked: boolean): void {
        const dbIndex = obj.dbs.indexOf(db);
        let dbs: string[] = [];

        if (checked && dbIndex === -1) {
            dbs = [...obj.dbs, db];
        } else if (!checked && dbIndex !== -1) {
            dbs = obj.dbs.filter(id => id !== db);
        }

        this.setPersonnelGroupRole(obj.groupId, obj.projectId, obj.groupRole, false, dbs)
            .subscribe(response => {
                this.reload$.next(true);
                this.notificationService.success('Database access updated');
            });
    }

    private setPersonnelGroupRole(groupId: number, projectId: string, role: WebProtocol.AccessRole, isGlobal: boolean | null = null, dbs: string[] | null = null): Observable<WebProtocol.GenericResponse> {
        const request = new WebProtocol.AccessRoleUpdateRequest();
        if (typeof isGlobal === 'boolean') request.isGlobal = isGlobal;
        if (dbs instanceof Array) request.dbs = dbs;
        request.role = role;

        return this.palladiumAdminService
            .setPersonnelGroupRole(request, groupId, projectId)
            .pipe(takeUntil(this.destroy$));
    }

    private resetPersonnelGroupRole(groupId: number, projectId: string): Observable<WebProtocol.GenericResponse> {
        return this.palladiumAdminService
            .resetPersonnelGroupRole(groupId, projectId)
            .pipe(takeUntil(this.destroy$));
    }
}
