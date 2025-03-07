import { CommonModule } from '@angular/common';
import { LoadingIndicatorComponent } from './loading-indicator.component';
import { NgModule } from '@angular/core';


@NgModule({
    imports: [
        CommonModule,
    ],
    declarations: [
        LoadingIndicatorComponent
    ],
    exports: [
        LoadingIndicatorComponent,
    ],
    providers: [
    ]
})
export class LoadingIndicatorModule {}
