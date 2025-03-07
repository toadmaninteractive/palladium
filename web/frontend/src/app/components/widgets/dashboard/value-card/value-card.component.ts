import { ChangeDetectionStrategy, ChangeDetectorRef, Component, EventEmitter, Input, OnInit, OnChanges, Output } from '@angular/core';
import { Period, PeriodKind } from '../../../../shared/interfaces/period';
import { MatDatepicker } from '@angular/material/datepicker';

@Component({
    selector: 'm-value-card',
    templateUrl: './value-card.component.html',
    styleUrls: ['./value-card.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ValueCardComponent implements OnInit, OnChanges {
    @Input() caption: string;
    @Input() captionForButton: string;
    @Input() selectedDate = new Date();
    @Input() datepickerDepth: string;
    @Input() tooltipCaption: string;
    @Input() value: number;
    @Input() previous: number;
    @Input() styleClass: string;
    @Input() description: string;
    @Input() activePeriod: PeriodKind;
    @Input() periods: Period[];
    @Input() error?: string;
    @Output() periodChanged = new EventEmitter<PeriodKind>();
    @Output() dateChanged = new EventEmitter<Date>();
    activePeriodLabel = '';
    increase: number;

    ngOnInit(): void {
        if (this.periods) {
            const activePeriod = this.periods.filter(p => p.value === this.activePeriod)[0];
            this.activePeriodLabel = activePeriod ? activePeriod.label : null;
        }
    }

    ngOnChanges() {
        this.increase = null;
        if (this.value !== null && this.previous !== null) {
            this.increase = Math.round((this.value - this.previous) / this.previous * 1000) / 10;
            if (this.previous === 0 || undefined) { this.increase = 0; }
        }
    }

    onPeriodChange(selectedPeriod: PeriodKind) {
        this.activePeriod = selectedPeriod;
        this.periodChanged.emit(selectedPeriod);
        const activePeriod = this.periods.filter(p => p.value === this.activePeriod)[0];
        this.activePeriodLabel = activePeriod ? activePeriod.label : null;
    }

    yearHandler(selectedYear: Date) {
        this.selectedDate.setFullYear(selectedYear.getFullYear());
    }

    monthHandler(selectedMonth: Date, datepicker: MatDatepicker<Date>) {
        this.selectedDate.setMonth(selectedMonth.getMonth());
        this.selectedDate = new Date(this.selectedDate);
        datepicker.close();
        this.dateChanged.emit(this.selectedDate);
    }

    setSelectedDate(event) {
        this.selectedDate = new Date(event);
        this.dateChanged.emit(this.selectedDate);
    }
}
