import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatCardModule } from '@angular/material/card';
import { MatCheckbox, MatCheckboxModule } from '@angular/material/checkbox';
import { MatRadioModule, MatRadioGroup, MatRadioButton } from '@angular/material/radio';
import { MatTooltipModule } from '@angular/material/tooltip';
import { SharedModule } from '../../../shared/shared.module';
import { D3SynchronizedComponent } from './d3-synchronized.component';
import { FormsModule } from '@angular/forms';

@NgModule({
    imports: [
        CommonModule,
        MatCardModule,
        MatTooltipModule,
        SharedModule,
        MatCheckboxModule,
        FormsModule
    ],
    declarations: [D3SynchronizedComponent],
    exports: [D3SynchronizedComponent],
    providers: []
})
export class D3SynchronizedModule { }
