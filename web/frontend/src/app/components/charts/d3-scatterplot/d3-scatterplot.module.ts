import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatCardModule } from '@angular/material/card';
import { MatTooltipModule } from '@angular/material/tooltip';
import { SharedModule } from '../../../shared/shared.module';
import { D3ScatterplotComponent } from './d3-scatterplot';

@NgModule({
    imports: [
        CommonModule,
        MatCardModule,
        MatTooltipModule,
        SharedModule,
    ],
    declarations: [D3ScatterplotComponent],
    exports: [D3ScatterplotComponent],
    providers: []
})
export class D3ScatterplotModule { }
