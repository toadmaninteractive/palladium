import { Injectable } from '@angular/core';
import { Constants } from '../../shared/config/constants';

@Injectable({
    providedIn: 'root',
})
export class StorageService {
    getStoredRoute(): string | null {
        return localStorage.getItem(Constants.storedRouteKey) || null;
    }

    setStoredRoute(url: string): void {
        localStorage.setItem(Constants.storedRouteKey, url);
    }

    resetStoredRoute(): void {
        localStorage.removeItem(Constants.storedRouteKey);
    }

    getLastProjectId(): string | null {
        return localStorage.getItem(Constants.lastProjectId) || null;
    }

    setLastProjectId(projectId: string): void {
        localStorage.setItem(Constants.lastProjectId, projectId);
    }

    resetLastProjectId(): void {
        localStorage.removeItem(Constants.lastProjectId);
    }

    getLastDatabase(): string | null {
        return localStorage.getItem(Constants.lastDatabase) || null;
    }

    setLastDatabase(db: string): void {
        localStorage.setItem(Constants.lastDatabase, db);
    }

    resetLastDatabase(): void {
        localStorage.removeItem(Constants.lastDatabase);
    }
}
