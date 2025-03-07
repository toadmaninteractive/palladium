import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatCardModule } from '@angular/material/card';
import { MatTooltipModule } from '@angular/material/tooltip';
import { D3PlotComponent } from './d3-plot.component';
import { SharedModule } from '../../../shared/shared.module';

@NgModule({
    imports: [
        CommonModule,
        MatCardModule,
        MatTooltipModule,
        SharedModule,
    ],
    declarations: [
        D3PlotComponent,
    ],
    exports: [
        D3PlotComponent,
    ],
    providers: []
})
export class D3PlotModule {}
