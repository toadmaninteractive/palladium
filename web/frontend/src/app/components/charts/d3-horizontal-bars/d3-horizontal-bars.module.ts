import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatCardModule } from '@angular/material/card';
import { MatTooltipModule } from '@angular/material/tooltip';
import { SharedModule } from '../../../shared/shared.module';
import { D3HorizontalBarsComponent } from './d3-horizontal-bars.component';

@NgModule({
    imports: [
        CommonModule,
        MatCardModule,
        MatTooltipModule,
        SharedModule,
    ],
    declarations: [D3HorizontalBarsComponent],
    exports: [D3HorizontalBarsComponent],
    providers: []
})
export class D3HorizontalBarsModule { }
