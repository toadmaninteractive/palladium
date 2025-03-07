import { Component, ChangeDetectionStrategy } from '@angular/core';
import { NavService } from '../../../../../core/services/nav.service';
import { ProjectService } from '../../../../../core/services/project.service';

@Component({
    selector: 'm-database-switch',
    templateUrl: './database-switch.component.html',
    styleUrls: ['./database-switch.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DatabaseSwitchComponent {
    constructor(
        public navService: NavService,
        public projectService: ProjectService,
    ) { }
}
