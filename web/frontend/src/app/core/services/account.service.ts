import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { BehaviorSubject, Observable, of } from 'rxjs';
import { filter, finalize, switchMap, tap } from 'rxjs/operators';
import { Constants } from '../../shared/config/constants';
import { PalladiumAuthService } from '../../protocol/web-auth-protocol.service';
import * as CommonProtocol from '../../protocol/common-protocol';
import * as WebProtocol from '../../protocol/web-protocol';

@Injectable({
    providedIn: 'root',
})
export class AccountService {
    private isInitializing$ = new BehaviorSubject(true);
    private isSignedIn$ = new BehaviorSubject<boolean | null>(null);
    isSigningIn$ = new BehaviorSubject(false);
    profile$ = new BehaviorSubject<WebProtocol.PersonnelAccountProfile | null>(null);

    constructor(
        private router: Router,
        private palladiumAuthService: PalladiumAuthService,
    ) {
        this.initialize();
    }

    private initialize(): void {
        if (!this.isInitializing$.getValue()) {
            this.isInitializing$.next(true);
        }

        this.palladiumAuthService
            .getPersonnelStatus()
            .pipe(
                switchMap(response => response.loggedIn ? this.palladiumAuthService.getMyPersonnelProfile() : of(null)),
                finalize(() => this.isInitializing$.next(false)),
            )
            .subscribe((profile: WebProtocol.PersonnelAccountProfile | null) => {
                this.profile$.next(profile);
                this.isSignedIn$.next(profile instanceof WebProtocol.PersonnelAccountProfile);
            });
    }

    private reset(): void {
        this.isSignedIn$.next(false);
        this.profile$.next(null);
        this.router.navigate([Constants.loginUrl]);
    }

    isSignedIn(): Observable<boolean> {
        return this.isSignedIn$.pipe(filter(value => value !== null));
    }

    signIn(username: string, password: string): Observable<WebProtocol.PersonnelAccountProfile | WebProtocol.PersonnelLoginResponse> {
        if (this.isSigningIn$.getValue()) {
            return;
        }

        this.isSigningIn$.next(true);

        const request = new WebProtocol.PersonnelLoginRequest();
        request.username = username;
        request.password = password;

        return this.palladiumAuthService
            .loginPersonnel(request)
            .pipe(
                switchMap(response => response.result ? this.palladiumAuthService.getMyPersonnelProfile() : of(response)),
                tap(profile => {
                    if (profile instanceof WebProtocol.PersonnelAccountProfile) {
                        // Success
                        this.profile$.next(profile);
                        this.isSignedIn$.next(true);
                    } else {
                        // Failure
                        this.profile$.next(null);
                    }
                }),
                finalize(() => this.isSigningIn$.next(false))
            );
    }

    signOut(): Observable<WebProtocol.GenericResponse> {
        return this.palladiumAuthService
            .logoutPersonnel(new CommonProtocol.Empty())
            .pipe(
                tap((response: WebProtocol.GenericResponse) => {
                    console.log(`Sign out ${response.result ? 'successful' : 'failure'}`);
                    this.reset();
                })
            );
    }
}
