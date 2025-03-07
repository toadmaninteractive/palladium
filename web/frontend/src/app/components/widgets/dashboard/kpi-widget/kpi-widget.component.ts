import {Component, Input, OnInit} from '@angular/core';
import { WidgetQueryResult } from '../../../../protocol/web-protocol';
import {NumericSeries, QueryParamType} from '../../../../protocol/data-protocol';
import {CardQueryParam} from "../../../../protocol/card-protocol";

@Component({
    selector: 'm-kpi-widget',
    templateUrl: './kpi-widget.component.html',
    styleUrls: ['./kpi-widget.component.scss']
})
export class KpiWidgetComponent implements OnInit {

    value: number;
    changeInPercentage: number;
    previous: number;
    showPrevValue: boolean;

    @Input('value')
    set _value(value: WidgetQueryResult) {
        const numericSeries = value.result.result[0] as NumericSeries;
        this.value = numericSeries.data[0].value;
        this.changeInPercentage = Math.round((this.value - this.previous) / this.previous * 100);
    }

    @Input('previous')
    set _previous(previous: WidgetQueryResult) {
        const numericSeries = previous.result.result[0] as NumericSeries;
        this.previous = numericSeries.data[0].value;
        this.changeInPercentage = Math.round((this.value - this.previous) / this.previous * 100);
    }

    @Input() unit: string;
    @Input() params: CardQueryParam[];

    ngOnInit() {
        this.showPrevValue = this.determineShowPrevVals();
    }

    getPercentageAsAbsoluteNumber(value: number): number {
        return Math.abs(value);
    }

    determineShowPrevVals(): boolean {
        // If widget's parameter is of any of the following types, then show previous value
        const types = [
            QueryParamType.Month,
            QueryParamType.Date,
            QueryParamType.DateRange,
            QueryParamType.TimePartition,
            QueryParamType.Select
        ];

        return this.params.some(param => {
            return types.some(type => type === param.type);
        })
    }
}
