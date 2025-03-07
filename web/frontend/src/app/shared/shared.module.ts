import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { ClipboardDirective } from './directives/clipboard.directive';
import { HeaderDirective } from './directives/header.directive';
import { MenuAsideDirective } from './directives/menu-aside.directive';
import { MenuAsideOffcanvasDirective } from './directives/menu-aside-offcanvas.directive';
import { MenuAsideToggleDirective } from './directives/menu-aside-toggle.directive';
import { MenuHorizontalDirective } from './directives/menu-horizontal.directive';
import { MenuHorizontalOffcanvasDirective } from './directives/menu-horizontal-offcanvas.directive';
import { PortletDirective } from './directives/portlet.directive';
import { ScrollTopDirective } from './directives/scroll-top.directive';
import { QuickSearchDirective } from './directives/quick-search.directive';
import { QuickSidebarOffcanvasDirective } from './directives/quick-sidebar-offcanvas.directive';

import { ConsoleLogPipe } from './pipes/console-log.pipe';
import { FirstLetterPipe } from './pipes/first-letter.pipe';
import { GetObjectPipe } from './pipes/get-object.pipe';
import { JoinPipe } from './pipes/join.pipe';
import { MapToPipe } from './pipes/map-to.pipe';
import { NumericPipe } from './pipes/numeric.pipe';
import { PrettyDurationPipe } from './pipes/pretty-duration.pipe';
import { PrettySizePipe } from './pipes/pretty-size.pipe';
import { ReversePipe } from './pipes/reverse.pipe';
import { SafePipe } from './pipes/safe.pipe';
import { TimeElapsedPipe } from './pipes/time-elapsed.pipe';
import { VisualizationCheckTypePipe } from './pipes/visualization-check-type';
import { DateRangePipe } from './pipes/date-range.pipe';

@NgModule({
    imports: [
        CommonModule
    ],
    declarations: [
        // Directives
        ClipboardDirective,
        HeaderDirective,
        MenuAsideDirective,
        MenuAsideOffcanvasDirective,
        MenuAsideToggleDirective,
        MenuHorizontalDirective,
        MenuHorizontalOffcanvasDirective,
        PortletDirective,
        ScrollTopDirective,
        QuickSearchDirective,
        QuickSidebarOffcanvasDirective,

        // Pipes
        ConsoleLogPipe,
        FirstLetterPipe,
        GetObjectPipe,
        JoinPipe,
        PrettyDurationPipe,
        PrettySizePipe,
        ReversePipe,
        SafePipe,
        TimeElapsedPipe,
        MapToPipe,
        NumericPipe,
        VisualizationCheckTypePipe,
        DateRangePipe,
    ],
    exports: [
        // Directives
        ClipboardDirective,
        HeaderDirective,
        MenuAsideDirective,
        MenuAsideOffcanvasDirective,
        MenuAsideToggleDirective,
        MenuHorizontalDirective,
        MenuHorizontalOffcanvasDirective,
        PortletDirective,
        ScrollTopDirective,
        QuickSearchDirective,
        QuickSidebarOffcanvasDirective,

        // Pipes
        ConsoleLogPipe,
        FirstLetterPipe,
        GetObjectPipe,
        JoinPipe,
        PrettyDurationPipe,
        PrettySizePipe,
        ReversePipe,
        SafePipe,
        TimeElapsedPipe,
        MapToPipe,
        NumericPipe,
        VisualizationCheckTypePipe,
        DateRangePipe,
    ],
    providers: []
})
export class SharedModule { }
