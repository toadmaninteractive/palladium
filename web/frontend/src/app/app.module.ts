import { NgModule } from '@angular/core';
import { HttpClientModule } from '@angular/common/http';
import { BrowserModule, HAMMER_GESTURE_CONFIG } from '@angular/platform-browser';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { OverlayModule } from '@angular/cdk/overlay';
import { GestureConfig } from '@angular/material/core';
import { PerfectScrollbarConfigInterface, PERFECT_SCROLLBAR_CONFIG } from 'ngx-perfect-scrollbar';
import { ToastrModule } from 'ngx-toastr';
import 'hammerjs';


import { AppComponent } from './app.component';
import { AppRoutingModule } from './app-routing.module';

import { ComponentsModule } from './components/components.module';
import { CoreModule } from './core/core.module';
import { LayoutModule } from './content/layout/layout.module';
import { ProtocolModule } from './protocol/protocol.module';
import { SharedModule } from './shared/shared.module';
import { MarkdownModule } from 'ngx-markdown';

@NgModule({
    declarations: [AppComponent],
    imports: [
        // Angular modules
        BrowserModule,
        BrowserAnimationsModule,
        OverlayModule,
        HttpClientModule,

        // Third party modules
        ToastrModule.forRoot({ maxOpened: 10 }),
        MarkdownModule.forRoot(),

        // Project modules
        CoreModule,
        ProtocolModule,
        SharedModule,
        LayoutModule,
        ComponentsModule,

        // Application routing module
        AppRoutingModule,
    ],
    providers: [
        { provide: HAMMER_GESTURE_CONFIG, useClass: GestureConfig },
        { provide: PERFECT_SCROLLBAR_CONFIG, useValue: <PerfectScrollbarConfigInterface>{ } },
    ],
    bootstrap: [AppComponent]
})
export class AppModule { }
