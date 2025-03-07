import { NgModule, Optional, SkipSelf  } from '@angular/core';
import { CommonModule } from '@angular/common';

// Metronic services
import { ClassInitService } from './services/metronic/class-init.service';
import { LayoutConfigService } from './services/metronic/layout-config.service';
import { LayoutConfigStorageService } from './services/metronic/layout-config-storage.service';
import { MenuConfigService } from './services/metronic/menu-config.service';
import { PageConfigService } from './services/metronic/page-config.service';
import { SplashScreenService } from './services/metronic/splash-screen.service';

// Metronic layout services
import { HeaderService } from './services/metronic/layout/header.service';
import { LayoutRefService } from './services/metronic/layout/layout-ref.service';
import { MenuAsideService } from './services/metronic/layout/menu-aside.service';
import { MenuHorizontalService } from './services/metronic/layout/menu-horizontal.service';
import { SubheaderService } from './services/metronic/layout/subheader.service';

// Palladium services
import { AccountService } from './services/account.service';
import { CanvasService } from './services/canvas.service';
import { ClipboardService } from './services/clipboard.service';
import { FilterService } from './services/filter.service';
import { NavService } from './services/nav.service';
import { NotificationService } from './services/notification.service';
import { ProjectService } from './services/project.service';
import { StorageService } from './services/storage.service';
import { WindowRefService } from './services/window-ref.service';

// Palladium fake services
import { FakeDataService } from './services/fake/fake-data.service';

// Palladium guards
import { AuthorizedGuard } from './guards/authorized.guard';
import { NotAuthorizedGuard } from './guards/not-authorized.guard';
import { ProjectResolverGuard } from './guards/project-resolver.guard';
import { SuperadminGuard } from './guards/superadmin.guard';

@NgModule({
    imports: [
        CommonModule
    ],
    declarations: [],
    exports: [],
    providers: [
        // Metronic services
        ClassInitService,
        LayoutConfigService,
        LayoutConfigStorageService,
        MenuConfigService,
        PageConfigService,
        SplashScreenService,

        // Metronic layout services
        HeaderService,
        LayoutRefService,
        MenuAsideService,
        MenuHorizontalService,
        SubheaderService,

        // Palladium services
        AccountService,
        CanvasService,
        ClipboardService,
        FilterService,
        NavService,
        NotificationService,
        ProjectService,
        StorageService,
        WindowRefService,

        // Palladium fake services
        FakeDataService,

        // Palladium guards
        AuthorizedGuard,
        NotAuthorizedGuard,
        ProjectResolverGuard,
        SuperadminGuard,
    ]
})
export class CoreModule {
    constructor (@Optional() @SkipSelf() parentModule: CoreModule) {
        if (parentModule) {
            throw new Error('CoreModule is already loaded. Import it in the AppModule only');
        }
    }
}
