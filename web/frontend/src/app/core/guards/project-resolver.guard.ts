import { Injectable } from '@angular/core';
import { Resolve, ActivatedRouteSnapshot, RouterStateSnapshot, Router } from '@angular/router';
import { Observable } from 'rxjs';
import { filter, first, map } from 'rxjs/operators';
import { ActivatedProject } from '../../shared/interfaces/activated-project';
import { NavService } from '../services/nav.service';
import { ProjectService } from '../services/project.service';

@Injectable({
    providedIn: 'root',
})
export class ProjectResolverGuard implements Resolve<ActivatedProject> {
    constructor(
        private router: Router,
        private navService: NavService,
        private projectService: ProjectService,
    ) { }

    resolve(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): Observable<ActivatedProject> {
        return this.projectService.projects$
            .pipe(
                filter(projects => projects instanceof Array),
                first(),
                map(projects => {
                    const activeProject = this.projectService.getActiveProject(state.url),
                        fallbackProject = projects[0],
                        activeDatabase = this.projectService.getActiveDatabase(state.url),
                        isActiveDatabaseValid = activeProject ? activeProject.databases.filter(db => db === activeDatabase).length > 0 : false;

                    if (activeProject) {
                        if (isActiveDatabaseValid) {
                            return <ActivatedProject> { project: activeProject, database: activeDatabase };
                        } else {
                            this.navService.switchProject(activeProject.id, activeProject.databases[0], state.url);
                            return null;
                        }
                    } else if (fallbackProject) {
                        this.navService.switchProject(fallbackProject.id, fallbackProject.databases[0], state.url);
                        return null;
                    } else {
                        this.router.navigate(['/404']);
                        return null;
                    }
                }),
            );
    }
}
