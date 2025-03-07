import { Component, Input, ChangeDetectionStrategy, ViewChild, ElementRef, ContentChild, TemplateRef } from '@angular/core';
import { BehaviorSubject } from 'rxjs';
import { D3Options, D3AxisValueType, D3SeriesDataSet } from '../../charts/shared/d3-common-options';
import { VisualizationCommonPlot, VisualizationSynchronizedPlot } from 'src/app/protocol/visualization-protocol';
import { WindowRefService } from '../../../core/services/window-ref.service';
import { ChartBoxExpansionService } from 'src/app/core/services/chart-box-expansion.service';

@Component({
    selector: 'm-chart-box',
    templateUrl: './chart-box.component.html',
    styleUrls: ['./chart-box.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ChartBoxComponent {
    // @ts-ignore
    @ContentChild(TemplateRef) contentTemplate: TemplateRef;
    @ViewChild('chart', {static: true}) chart: ElementRef;
    isExpanded: boolean;

    // ANIMATION TIME NEEDS TO BE LONGER THAN .d3-container-fixed WIDTH TRANSITION
    animationTime = 510;
    @Input() onlyFixed = false;
    @Input() uniqueId = '0';

    options$ = new BehaviorSubject<D3Options | VisualizationCommonPlot | VisualizationSynchronizedPlot>(null);
    @Input('options') set _options(value: | D3Options | VisualizationCommonPlot | VisualizationSynchronizedPlot) {
        this.options$.next(value);
    }

    data = new BehaviorSubject<D3SeriesDataSet[] | null>([]);
    @Input('data') set _series(value: D3SeriesDataSet[]) {
        this.data.next(value);
    }

    userOS: string;

    constructor(
        private windowRefService: WindowRefService,
        private chartBoxExpansionService: ChartBoxExpansionService) {
        this.isExpanded = this.chartBoxExpansionService.isExpanded;
        this.userOS = this.windowRefService.detectOperatingSystem();
    }

    onExpandChart(): void {
        this.isExpanded = !this.isExpanded;
        this.chartBoxExpansionService.isExpanded = this.isExpanded;
        this.chartBoxExpansionService.expand.next({ delay: this.animationTime, containerKey: this.uniqueId });
    }
}
