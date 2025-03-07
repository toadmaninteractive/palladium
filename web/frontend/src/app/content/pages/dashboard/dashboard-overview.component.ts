import { ChangeDetectionStrategy, Component, OnDestroy } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { BehaviorSubject, Subject } from 'rxjs';
import { takeUntil, tap } from 'rxjs/operators';
import { PalladiumQueryService } from '../../../protocol/web-query-protocol.service';
import * as CardProtocol from '../../../protocol/card-protocol';
import * as WebProtocol from '../../../protocol/web-protocol';
import { AxisValueType, VisualizationBoxPlot, VisualizationKind } from "../../../protocol/visualization-protocol";
import { BoxPlotPoint, BoxPlotSeries, SeriesKind } from "../../../protocol/data-protocol";

@Component({
    selector: 'm-dashboard-overview',
    templateUrl: './dashboard-overview.component.html',
    styleUrls: ['./dashboard-overview.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DashboardOverviewComponent implements OnDestroy {
    destroy$ = new Subject<any>();
    dashboardData$ = new BehaviorSubject<WebProtocol.DashboardData>(null);
    widgets$ = new BehaviorSubject<CardProtocol.CardWidget[]>(null);
    loading$ = new BehaviorSubject<boolean>(null);

    paramsMap = new Map<string, CardProtocol.CardQueryParam[]>();

    project: WebProtocol.ProjectConfig;
    database: string;
    option;
    data;

    constructor(
        private route: ActivatedRoute,
        private queryService: PalladiumQueryService,
    ) {
        this.route.data
            .pipe(
                takeUntil(this.destroy$),
                tap((d) => this.loading$.next(true))
            )
            .subscribe((routeData) => {
                this.initialize(routeData.activeProject.project, routeData.activeProject.database);
            });

        this.dashboardData$
            .pipe(takeUntil(this.destroy$))
            .subscribe(dashboardData => {
                if (dashboardData) {
                    dashboardData.widgets.forEach(widget => {
                        const cardQueryParams = widget.params.map(key =>
                            dashboardData.params.find(parameter => parameter.id === key));
                        this.paramsMap.set(widget.key, cardQueryParams);
                    });
                    this.widgets$.next(dashboardData.widgets);
                }
            });
        this.option = {
            kind: VisualizationKind.BoxPlot,
            name: 'Box plot',
            subtitle: '',
            description: '',
            showXAxis: true,
            showYAxis: true,
            showXGridLine: true,
            showYGridLine: true,
            xAxisType: AxisValueType.Text,
            yAxisType: AxisValueType.Numeric,
            xAxisTitle: '1',
            yAxisTitle: '2',
            showLegend: true,
        } as VisualizationBoxPlot;
        this.data = {
            kind: SeriesKind.BoxPlot,
            key: 'value',
            data: [{
                label: 'intro',
                value: [105, 200, 94, 16, 83, 144, 95, 368, 134, 88, 86, 82, 110, 94, 102, 107, 157, 167, 200, 202, 146, 66, 230, 396, 109],
            }, {
                label: 'ty_spy',
                value: [1025, 591, 377, 434, 374, 444, 490, 505, 571, 485, 475, 368, 591, 368, 374, 282, 474, 479, 291, 411, 320, 302, 198, 2, 446],
            }, {
                label: 'sh_smr',
                value: [10, 229, 147, 210, 157, 240, 224, 275, 313, 140, 209, 195, 222, 215, 282, 354, 310, 206, 202, 270, 281, 318, 151, 126, 900],
            }

            ] as BoxPlotPoint[],
        } as BoxPlotSeries;

    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
    }

    private initialize(project: WebProtocol.ProjectConfig, database: string): void {
        this.project = project;
        this.database = database;
        this.queryService.getDashboardData(this.project.id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(res => {
                if (res) this.dashboardData$.next(res);
            });
    }
}
