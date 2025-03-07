import { NgModule, Optional, SkipSelf  } from '@angular/core';
import { CommonModule } from '@angular/common';

import { PalladiumAdminService } from './web-admin-protocol.service';
import { PalladiumAuthService } from './web-auth-protocol.service';
import { PalladiumQueryService } from './web-query-protocol.service';

@NgModule({
    imports: [
        CommonModule
    ],
    declarations: [],
    exports: [],
    providers: [
        PalladiumAdminService,
        PalladiumAuthService,
        PalladiumQueryService,
    ]
})
export class ProtocolModule {
    constructor (@Optional() @SkipSelf() parentModule: ProtocolModule) {
        if (parentModule) {
            throw new Error('ProtocolModule is already loaded. Import it in the AppModule only');
        }
    }
}
