import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatCardModule } from '@angular/material/card';
import { MatRadioModule } from '@angular/material/radio';
import { MatTooltipModule } from '@angular/material/tooltip';
import { LoadingIndicatorModule } from '../../general/loading-indicator/loading-indicator/loading-indicator.module';
import { D3KillmapComponent } from './d3-killmap.component';

@NgModule({
    imports: [
        // Angular modules
        CommonModule,
        FormsModule,
        ReactiveFormsModule,

        // Angular Material modules
        MatCardModule,
        MatRadioModule,
        MatTooltipModule,

        // Project modules
        LoadingIndicatorModule,
    ],
    declarations: [D3KillmapComponent],
    exports: [D3KillmapComponent],
    providers: []
})
export class D3KillmapModule { }
