import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatCardModule } from '@angular/material/card';
import { MatCheckbox, MatCheckboxModule } from '@angular/material/checkbox';
import { MatRadioModule, MatRadioGroup, MatRadioButton } from '@angular/material/radio';
import { MatTooltipModule } from '@angular/material/tooltip';
import { SharedModule } from '../../../shared/shared.module';
import { D3FunnelComponent } from './d3-funnel.component';

@NgModule({
    imports: [
        CommonModule,
        MatCardModule,
        MatTooltipModule,
        SharedModule,
        MatCheckboxModule
    ],
    declarations: [D3FunnelComponent],
    exports: [D3FunnelComponent],
    providers: []
})
export class D3FunnelModule { }
