import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatCardModule } from '@angular/material/card';
import { MatTooltipModule } from '@angular/material/tooltip';
import { SharedModule } from '../../../shared/shared.module';
import { D3BoxPlotComponent } from "./d3-box-plot.component";

@NgModule({
    imports: [
        CommonModule,
        MatCardModule,
        MatTooltipModule,
        SharedModule,
    ],
    declarations: [
        D3BoxPlotComponent,
    ],
    exports: [
        D3BoxPlotComponent,
    ],
    providers: []
})
export class D3BoxPlotModule {}
