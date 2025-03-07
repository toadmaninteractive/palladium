import { Component, ChangeDetectionStrategy } from '@angular/core';
import { NavService } from '../../../../../core/services/nav.service';
import { ProjectService } from '../../../../../core/services/project.service';

@Component({
    selector: 'm-project-switch',
    templateUrl: './project-switch.component.html',
    styleUrls: ['./project-switch.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ProjectSwitchComponent {
    constructor (
        public navService: NavService,
        public projectService: ProjectService,
    ) { }
}
