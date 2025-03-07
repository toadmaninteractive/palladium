import { ChangeDetectionStrategy, ChangeDetectorRef, Component, OnDestroy } from '@angular/core';
import { Subject, Subscription } from 'rxjs';
import { ActivatedRoute } from '@angular/router';
import { takeUntil } from 'rxjs/operators';
import { PalladiumQueryService } from '../../../../protocol/web-query-protocol.service';
import * as CardProtocol from '../../../../protocol/card-protocol';
import * as WebProtocol from '../../../../protocol/web-protocol';

@Component({
    selector: 'm-patch-notes-overview',
    templateUrl: './patch-notes-overview.component.html',
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PatchNotesOverviewComponent implements OnDestroy {
    destroy$ = new Subject<any>();
    project: WebProtocol.ProjectConfig;
    fullDescriptionMap = new Map();
    shortDescriptionMap = new Map();
    isFullVisibleMap = new Map();
    patchNotes: Array<CardProtocol.CardPatchNote>;
    subPatchNotes: Subscription;
    substringLength = 220;

    constructor (
        private route: ActivatedRoute,
        private cdr: ChangeDetectorRef,
        private queryService: PalladiumQueryService,
    ) {
        this.route.data
            .pipe(takeUntil(this.destroy$))
            .subscribe((data) => {
                this.initialize(data.activeProject.project);
            });
    }

    private initialize(project: WebProtocol.ProjectConfig): void {
        if (this.project !== project) {
            this.project = project;
            this.getPatchNotes(this.project.id);
        }
    }

    ngOnDestroy(): void {
        if (this.subPatchNotes instanceof Subscription) {
            this.subPatchNotes.unsubscribe();
        }

        this.destroy$.next();
        this.destroy$.complete();
    }

    private updatePatchNotes(patchNotes: Array<CardProtocol.CardPatchNote>): void {
        this.patchNotes = patchNotes;

        this.patchNotes.sort((a, b) => {
            const dateA = new Date(a.addedAt).getTime(),
                dateB = new Date(b.addedAt).getTime();

            return dateA > dateB ? -1 : dateA < dateB ? 1 : 0;
        });

        this.patchNotes.forEach((item, index) => {
            this.fullDescriptionMap.set(index, item.description);
            this.shortDescriptionMap.set(index, item.description.substring(0, this.substringLength));
            this.isFullVisibleMap.set(index, false);
        });

        setTimeout(() => this.cdr.detectChanges());
    }

    private getPatchNotes(projectKey: string): void {
        if (this.subPatchNotes instanceof Subscription) {
            this.subPatchNotes.unsubscribe();
        }

        this.subPatchNotes = this.queryService
            .getPatchNotes(projectKey)
            .pipe(takeUntil(this.destroy$))
            .subscribe(response => this.updatePatchNotes(response.items));
    }
}
