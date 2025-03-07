import { Component, EventEmitter, Input, Output } from '@angular/core';
import { SelectItem } from '../../../../shared/interfaces/select-item';

@Component({
    selector: 'm-multi-value-card',
    templateUrl: './multi-value-card.component.html',
    styleUrls: ['./multi-value-card.component.scss']
})
export class MultiValueCardComponent {
    @Input() caption: string;
    @Input() tooltipCaption: string;
    @Input() captionForButton: string;
    @Input() items: SelectItem[];
    @Output() reload = new EventEmitter<void>();

    colorStyleClass = [
        'm--font-success',
        'm--font-accent',
        'm--font-brand',
        'm--font-danger',
    ];

    onReload() {
        this.reload.emit();
    }
}
