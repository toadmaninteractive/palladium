import { Component, Input, Output, EventEmitter } from '@angular/core';
import * as DataProtocol from '../../../../protocol/data-protocol';
import { Subject } from 'rxjs';
import { debounceTime, filter } from 'rxjs/operators';

@Component({
    selector: 'm-param-box',
    templateUrl: './param-box.component.html',
    styleUrls: ['./param-box.component.scss'],
})
export class ParamBoxComponent {
    @Input() name: string;
    @Input() type: DataProtocol.QueryParamType;
    @Input() isRequired: boolean;
    @Input() selectOptions: Array<DataProtocol.JsonPoint>;
    @Input() value: any;
    @Output() onChange = new EventEmitter<any>();
    @Output() onRemove = new EventEmitter<boolean>();
    @Input() key: string;                                   // DEPRECATED
    @Output() valueChange = new EventEmitter<any>();        // DEPRECATED
    @Output() removeFilter = new EventEmitter<string>();    // DEPRECATED
    enumType = DataProtocol.QueryParamType;
    enumPartition = DataProtocol.TimePartition;
    debouncer: Subject<any> = new Subject<any>();

    constructor() {
        this.debouncer
            .pipe(
                debounceTime(150),
                filter(value => value !== null))
            .subscribe(value => this.onChange.emit(value));
    }

    isNumber(value): boolean {
        return typeof value === 'number' && !isNaN(value);
    }

    selectOption(value: any): DataProtocol.JsonPoint | null {
        const opts = (this.selectOptions || []).filter(opt => opt.value === value);
        return opts.length > 0 ? opts[0] : null;
    }

    change(value: any): void {
        this.valueChange.emit({ value: value, key: this.key }); // DEPRECATED
        this.debouncer.next(value);

    }

    remove(): void {
        this.removeFilter.emit(this.key); // DEPRECATED
        this.onRemove.emit(true);
    }
}
