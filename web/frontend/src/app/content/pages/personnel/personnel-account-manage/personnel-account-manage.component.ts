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
    accountId: number;
    needle: string;
    orderBy: WebProtocol.PersonnelAccountRoleOrderBy;
    orderDir: DataProtocol.OrderDirection;
    limit: number;
    offset: number;
}

interface EffectiveRole {
    role: WebProtocol.AccessRole | null;
    groupName: string | null;
}

enum Column {
    ProjectId = 'project_id',
    ProjectTitle = 'project_title',
    GroupRoles = 'group_roles',
    UserRole = 'user_role',
    ProjectDbs = 'project_dbs',
    EffectiveRole = 'effective_role',
}

const DEFAULT_ORDER_BY = Column.ProjectId,
    DEFAULT_ORDER_DIR = 'asc',
    DEFAULT_PAGE_SIZE = 10,
    DEFAULT_COLUMNS = [Column.ProjectId, Column.ProjectTitle, Column.GroupRoles, Column.UserRole, Column.ProjectDbs, Column.EffectiveRole];

@Component({
    selector: 'm-personnel-account-manage',
    templateUrl: './personnel-account-manage.component.html',
    styleUrls: ['./personnel-account-manage.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PersonnelAccountManageComponent implements OnDestroy {
    public config: any;
    destroy$ = new Subject();
    loading$ = new BehaviorSubject<boolean>(false);
    account$ = new BehaviorSubject<WebProtocol.PersonnelAccount>(null);
    alert$ = new BehaviorSubject<AlertMessage | null>(null);
    reload$ = new BehaviorSubject<boolean>(true);
    needle$ = new BehaviorSubject<string>('');
    sort$ = new BehaviorSubject<Sort>(<Sort> { active: DEFAULT_ORDER_BY, direction: DEFAULT_ORDER_DIR });
    page$ = new BehaviorSubject<PageEvent>(<PageEvent> { pageIndex: 0, pageSize: DEFAULT_PAGE_SIZE });
    total$ = new BehaviorSubject<number>(0);
    dataSource$ = new BehaviorSubject<MatTableDataSource<WebProtocol.PersonnelAccountRole>>(new MatTableDataSource<WebProtocol.PersonnelAccountRole>());
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
                map(params => params.get('username')),
                distinctUntilChanged(),
                switchMap(username => this.palladiumAdminService.getPersonnelAccountByUsername(username)),
                tap(account => this.onAccountChange(account)),
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
            map(([account, reload, needle, sort, page]) => <QueryArguments> {
                accountId: account.id,
                needle: needle,
                orderBy: WebProtocol.PersonnelAccountRoleOrderBy.fromJson(sort.active || DEFAULT_ORDER_BY),
                orderDir: DataProtocol.OrderDirection.fromJson(sort.direction || DEFAULT_ORDER_DIR),
                limit: page.pageSize,
                offset: page.pageIndex * page.pageSize,
            }),
            filter(args => {
                const shouldRestart = !(this.args
                    && this.args.accountId === args.accountId
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

    private getData(args: QueryArguments): Observable<WebProtocol.PersonnelAccountRole[]> {
        return this.palladiumAdminService
            .getPersonnelAccountRoles(args.accountId, args.needle, args.orderBy, args.orderDir, args.offset, args.limit)
            .pipe(
                takeUntil(this.destroy$),
                tap((response: DataProtocol.CollectionSlice<WebProtocol.PersonnelAccountRole>) => this.total$.next(response.total)),
                map((response: DataProtocol.CollectionSlice<WebProtocol.PersonnelAccountRole>) => response.items),
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
                map(response => {
                    response = response.map(item => {
                        const effective = this.effectiveRole(item.userRole, item.groupRoles),
                            isGlobalConsumer = item.userRole === WebProtocol.AccessRole.Consumer && item.isGlobal,
                            isGlobalGroupConsumer = Object.values(item.groupRoles)
                                .filter(obj => WebProtocol.AccessRole.fromJson(obj.role) === WebProtocol.AccessRole.Consumer && obj.is_global).length > 0;

                        item['effectiveRole'] = effective.role;
                        item['effectiveGroup'] = effective.groupName;
                        item['effectiveAllDbs'] = effective.role >= WebProtocol.AccessRole.Maintainer || isGlobalGroupConsumer || isGlobalConsumer;
                        item['effectiveDbs'] = this.effectiveDbs(item);

                        return item;
                    });

                    return response;
                }),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe(response => {
                const dataSource = new MatTableDataSource<WebProtocol.PersonnelAccountRole>();
                dataSource.data = response;
                this.dataSource$.next(dataSource);
            });
    }

    roleName(role: string | WebProtocol.AccessRole | any): string {
        const actualRole = typeof role === 'string' ? WebProtocol.AccessRole.fromJson(role) : role;
        return WebProtocol.AccessRole.getDescription(actualRole);
    }

    private mostPrivilegedGroup(groupRoles: any): string | null {
        const roles = groupRoles || {},
            groups = Object.keys(roles);

        return groups.length > 0
            ? groups.sort((a, b) => WebProtocol.AccessRole.fromJson(roles[a].role) > WebProtocol.AccessRole.fromJson(roles[b].role) ? -1 : 1)[0]
            : null;
    }

    private effectiveRole(userRole: WebProtocol.AccessRole, groupRoles: any): EffectiveRole {
        const privilegedGroup = this.mostPrivilegedGroup(groupRoles),
            groupRole = privilegedGroup ? WebProtocol.AccessRole.fromJson(groupRoles[privilegedGroup].role) : null,
            groupOverridesUser = (groupRole || 0) > (userRole || 0),
            result = <EffectiveRole> { role: null, groupName: null };

        if (!userRole && !privilegedGroup) {
            return result;
        }

        result.role = groupOverridesUser ? groupRole : userRole;
        result.groupName = groupOverridesUser ? privilegedGroup : null;

        return result;
    }

    private effectiveDbs(item: WebProtocol.PersonnelAccountRole): string[] {
        const dbSet = new Set<string>();

        Object.values(item.groupRoles)
            .filter(obj => WebProtocol.AccessRole.fromJson(obj.role) === WebProtocol.AccessRole.Consumer)
            .forEach(obj => obj.dbs.forEach(db => dbSet.add(db)));

        if (item.userRole === WebProtocol.AccessRole.Consumer) {
            item.dbs.forEach(db => dbSet.add(db));
        }

        return [...dbSet].sort((a, b) => a > b ? 1 : -1);
    }

    updatePageStatus(account: WebProtocol.PersonnelAccount): void {
        this.subheaderService.setTitle(account.name);

        this.subheaderService.setBreadcrumbs([
            <Breadcrumb>{ title: 'Personnel Accounts', page: '/personnel/accounts' },
            <Breadcrumb>{ title: account.name, page: `/personnel/accounts/${account.username}` },
        ]);

        this.alert$.next(null);
        this.cdr.detectChanges();
    }

    onNeedleChange(needle: string): void {
        this.needle$.next((needle || '').trim());
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

    onAccountChange(account: WebProtocol.PersonnelAccount): void {
        this.account$.next(account);
        this.updatePageStatus(account);
    }

    onRoleChange(obj: WebProtocol.PersonnelAccountRole, role: WebProtocol.AccessRole): void {
        if (obj.userRole === role) {
            return;
        }

        this.setPersonnelAccountRole(obj.personnelId, obj.projectId, role)
            .subscribe(response => {
                this.reload$.next(true);
                this.notificationService.success('Role changed');
            });
    }

    onRoleReset(obj: WebProtocol.PersonnelAccountRole): void {
        if (!obj.userRole) {
            return;
        }

        this.resetPersonnelAccountRole(obj.personnelId, obj.projectId)
            .subscribe(response => {
                this.reload$.next(true);
                this.notificationService.success('Role changed');
            });
    }

    onGlobalDbsChange(obj: WebProtocol.PersonnelAccountRole, checked: boolean): void {
        this.setPersonnelAccountRole(obj.personnelId, obj.projectId, obj.userRole, checked, [])
            .subscribe(response => {
                this.reload$.next(true);
                this.notificationService.success('Database access updated');
            });
    }

    onDbChange(obj: WebProtocol.PersonnelAccountRole, db: string, checked: boolean): void {
        const dbIndex = obj.dbs.indexOf(db);
        let dbs: string[] = [];

        if (checked && dbIndex === -1) {
            dbs = [...obj.dbs, db];
        } else if (!checked && dbIndex !== -1) {
            dbs = obj.dbs.filter(id => id !== db);
        }

        this.setPersonnelAccountRole(obj.personnelId, obj.projectId, obj.userRole, false, dbs)
            .subscribe(response => {
                this.reload$.next(true);
                this.notificationService.success('Database access updated');
            });
    }

    private setPersonnelAccountRole(personnelId: number, gameId: string, role: WebProtocol.AccessRole, isGlobal: boolean | null = null, dbs: string[] | null = null): Observable<WebProtocol.GenericResponse> {
        const request = new WebProtocol.AccessRoleUpdateRequest();
        if (typeof isGlobal === 'boolean') request.isGlobal = isGlobal;
        if (dbs instanceof Array) request.dbs = dbs;
        request.role = role;

        return this.palladiumAdminService
            .setPersonnelAccountRole(request, personnelId, gameId)
            .pipe(takeUntil(this.destroy$));
    }

    private resetPersonnelAccountRole(personnelId: number, gameId: string): Observable<WebProtocol.GenericResponse> {
        return this.palladiumAdminService
            .resetPersonnelAccountRole(personnelId, gameId)
            .pipe(takeUntil(this.destroy$));
    }
}
