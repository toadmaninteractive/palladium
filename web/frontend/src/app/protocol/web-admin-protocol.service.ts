// Author: Igor compiler
// Compiler version: igorc 2.1.4
// DO NOT EDIT THIS FILE - it is machine generated

import { Injectable } from '@angular/core';
import { HttpClient, HttpErrorResponse, HttpHeaders } from '@angular/common/http';
import { Observable, throwError } from 'rxjs';
import { map, catchError } from 'rxjs/operators';
import * as Igor from './igor';
import * as WebProtocol from './web-protocol';
import * as DataProtocol from './data-protocol';

@Injectable({
    providedIn: 'root',
})
export class PalladiumAdminService {
    public baseUrl = '';

    constructor(private http: HttpClient) { }

    public getPersonnelAccount(id: number): Observable<WebProtocol.PersonnelAccount> {
        return this.http
            .get(`${this.baseUrl}/api/admin/personnel/${id}`)
            .pipe(
                map(response => WebProtocol.PersonnelAccount.fromJson(response as Igor.Json.JsonValue))
            );
    }

    public getPersonnelAccountByUsername(username: string): Observable<WebProtocol.PersonnelAccount> {
        return this.http
            .get(`${this.baseUrl}/api/admin/personnel/username/${username}`)
            .pipe(
                map(response => WebProtocol.PersonnelAccount.fromJson(response as Igor.Json.JsonValue))
            );
    }

    public getPersonnelAccounts(needle: string | null = null, orderBy: WebProtocol.PersonnelAccountOrderBy, orderDir: DataProtocol.OrderDirection, offset: number, limit: number): Observable<DataProtocol.CollectionSlice<WebProtocol.PersonnelAccount>> {
        const queryParams: Array<string> = [];
        if (needle != null)
            queryParams.push(`needle=${needle}`);
        queryParams.push(`order_by=${WebProtocol.PersonnelAccountOrderBy.toJson(orderBy)}`);
        queryParams.push(`order_dir=${DataProtocol.OrderDirection.toJson(orderDir)}`);
        queryParams.push(`offset=${offset}`);
        queryParams.push(`limit=${limit}`);
        const queryString = queryParams.length > 0 ? `?${queryParams.join('&')}` : '';

        return this.http
            .get(`${this.baseUrl}/api/admin/personnels${queryString}`)
            .pipe(
                map(response => DataProtocol.CollectionSlice.instanceJsonSerializer<WebProtocol.PersonnelAccount>(WebProtocol.PersonnelAccount).fromJson(response as Igor.Json.JsonValue))
            );
    }

    public getPersonnelAccountRoles(id: number, needle: string | null = null, orderBy: WebProtocol.PersonnelAccountRoleOrderBy, orderDir: DataProtocol.OrderDirection, offset: number, limit: number): Observable<DataProtocol.CollectionSlice<WebProtocol.PersonnelAccountRole>> {
        const queryParams: Array<string> = [];
        if (needle != null)
            queryParams.push(`needle=${needle}`);
        queryParams.push(`order_by=${WebProtocol.PersonnelAccountRoleOrderBy.toJson(orderBy)}`);
        queryParams.push(`order_dir=${DataProtocol.OrderDirection.toJson(orderDir)}`);
        queryParams.push(`offset=${offset}`);
        queryParams.push(`limit=${limit}`);
        const queryString = queryParams.length > 0 ? `?${queryParams.join('&')}` : '';

        return this.http
            .get(`${this.baseUrl}/api/admin/personnel/${id}/roles${queryString}`)
            .pipe(
                map(response => DataProtocol.CollectionSlice.instanceJsonSerializer<WebProtocol.PersonnelAccountRole>(WebProtocol.PersonnelAccountRole).fromJson(response as Igor.Json.JsonValue))
            );
    }

    public getPersonnelAccountRolesForProject(project: string, needle: string | null = null, orderBy: WebProtocol.PersonnelAccountRoleOrderBy, orderDir: DataProtocol.OrderDirection, offset: number, limit: number): Observable<DataProtocol.CollectionSlice<WebProtocol.PersonnelAccountRole>> {
        const queryParams: Array<string> = [];
        if (needle != null)
            queryParams.push(`needle=${needle}`);
        queryParams.push(`order_by=${WebProtocol.PersonnelAccountRoleOrderBy.toJson(orderBy)}`);
        queryParams.push(`order_dir=${DataProtocol.OrderDirection.toJson(orderDir)}`);
        queryParams.push(`offset=${offset}`);
        queryParams.push(`limit=${limit}`);
        const queryString = queryParams.length > 0 ? `?${queryParams.join('&')}` : '';

        return this.http
            .get(`${this.baseUrl}/api/projects/${project}/roles/account${queryString}`)
            .pipe(
                map(response => DataProtocol.CollectionSlice.instanceJsonSerializer<WebProtocol.PersonnelAccountRole>(WebProtocol.PersonnelAccountRole).fromJson(response as Igor.Json.JsonValue))
            );
    }

    public setPersonnelAccountRole(request: WebProtocol.AccessRoleUpdateRequest, id: number, project: string): Observable<WebProtocol.GenericResponse> {
        const options = { headers: new HttpHeaders({'Content-Type': 'application/json' }) };

        return this.http
            .put(`${this.baseUrl}/api/admin/personnel/${id}/roles/${project}`, request.toJson(), options)
            .pipe(
                map(response => WebProtocol.GenericResponse.fromJson(response as Igor.Json.JsonValue))
            );
    }

    public resetPersonnelAccountRole(id: number, project: string): Observable<WebProtocol.GenericResponse> {
        return this.http
            .delete(`${this.baseUrl}/api/admin/personnel/${id}/roles/${project}`)
            .pipe(
                map(response => WebProtocol.GenericResponse.fromJson(response as Igor.Json.JsonValue))
            );
    }

    public getPersonnelGroup(id: number): Observable<WebProtocol.PersonnelGroup> {
        return this.http
            .get(`${this.baseUrl}/api/admin/personnel-groups/${id}`)
            .pipe(
                map(response => WebProtocol.PersonnelGroup.fromJson(response as Igor.Json.JsonValue))
            );
    }

    public getPersonnelGroupByName(name: string): Observable<WebProtocol.PersonnelGroup> {
        return this.http
            .get(`${this.baseUrl}/api/admin/personnel-groups/name/${name}`)
            .pipe(
                map(response => WebProtocol.PersonnelGroup.fromJson(response as Igor.Json.JsonValue))
            );
    }

    public getPersonnelGroups(needle: string | null = null, orderBy: WebProtocol.PersonnelGroupOrderBy, orderDir: DataProtocol.OrderDirection, offset: number, limit: number): Observable<DataProtocol.CollectionSlice<WebProtocol.PersonnelGroup>> {
        const queryParams: Array<string> = [];
        if (needle != null)
            queryParams.push(`needle=${needle}`);
        queryParams.push(`order_by=${WebProtocol.PersonnelGroupOrderBy.toJson(orderBy)}`);
        queryParams.push(`order_dir=${DataProtocol.OrderDirection.toJson(orderDir)}`);
        queryParams.push(`offset=${offset}`);
        queryParams.push(`limit=${limit}`);
        const queryString = queryParams.length > 0 ? `?${queryParams.join('&')}` : '';

        return this.http
            .get(`${this.baseUrl}/api/admin/personnel-groups${queryString}`)
            .pipe(
                map(response => DataProtocol.CollectionSlice.instanceJsonSerializer<WebProtocol.PersonnelGroup>(WebProtocol.PersonnelGroup).fromJson(response as Igor.Json.JsonValue))
            );
    }

    public getPersonnelGroupRoles(id: number, needle: string | null = null, orderBy: WebProtocol.PersonnelGroupRoleOrderBy, orderDir: DataProtocol.OrderDirection, offset: number, limit: number): Observable<DataProtocol.CollectionSlice<WebProtocol.PersonnelGroupRole>> {
        const queryParams: Array<string> = [];
        if (needle != null)
            queryParams.push(`needle=${needle}`);
        queryParams.push(`order_by=${WebProtocol.PersonnelGroupRoleOrderBy.toJson(orderBy)}`);
        queryParams.push(`order_dir=${DataProtocol.OrderDirection.toJson(orderDir)}`);
        queryParams.push(`offset=${offset}`);
        queryParams.push(`limit=${limit}`);
        const queryString = queryParams.length > 0 ? `?${queryParams.join('&')}` : '';

        return this.http
            .get(`${this.baseUrl}/api/admin/personnel-groups/${id}/roles${queryString}`)
            .pipe(
                map(response => DataProtocol.CollectionSlice.instanceJsonSerializer<WebProtocol.PersonnelGroupRole>(WebProtocol.PersonnelGroupRole).fromJson(response as Igor.Json.JsonValue))
            );
    }

    public getPersonnelGroupRolesForProject(project: string, needle: string | null = null, orderBy: WebProtocol.PersonnelGroupRoleOrderBy, orderDir: DataProtocol.OrderDirection, offset: number, limit: number): Observable<DataProtocol.CollectionSlice<WebProtocol.PersonnelGroupRole>> {
        const queryParams: Array<string> = [];
        if (needle != null)
            queryParams.push(`needle=${needle}`);
        queryParams.push(`order_by=${WebProtocol.PersonnelGroupRoleOrderBy.toJson(orderBy)}`);
        queryParams.push(`order_dir=${DataProtocol.OrderDirection.toJson(orderDir)}`);
        queryParams.push(`offset=${offset}`);
        queryParams.push(`limit=${limit}`);
        const queryString = queryParams.length > 0 ? `?${queryParams.join('&')}` : '';

        return this.http
            .get(`${this.baseUrl}/api/projects/${project}/roles/group${queryString}`)
            .pipe(
                map(response => DataProtocol.CollectionSlice.instanceJsonSerializer<WebProtocol.PersonnelGroupRole>(WebProtocol.PersonnelGroupRole).fromJson(response as Igor.Json.JsonValue))
            );
    }

    public setPersonnelGroupRole(request: WebProtocol.AccessRoleUpdateRequest, id: number, project: string): Observable<WebProtocol.GenericResponse> {
        const options = { headers: new HttpHeaders({'Content-Type': 'application/json' }) };

        return this.http
            .put(`${this.baseUrl}/api/admin/personnel-groups/${id}/roles/${project}`, request.toJson(), options)
            .pipe(
                map(response => WebProtocol.GenericResponse.fromJson(response as Igor.Json.JsonValue))
            );
    }

    public resetPersonnelGroupRole(id: number, project: string): Observable<WebProtocol.GenericResponse> {
        return this.http
            .delete(`${this.baseUrl}/api/admin/personnel-groups/${id}/roles/${project}`)
            .pipe(
                map(response => WebProtocol.GenericResponse.fromJson(response as Igor.Json.JsonValue))
            );
    }

    public getSettings(): Observable<WebProtocol.Settings> {
        return this.http
            .get(`${this.baseUrl}/api/admin/settings`)
            .pipe(
                map(response => WebProtocol.Settings.fromJson(response as Igor.Json.JsonValue))
            );
    }

    public updateSettings(request: WebProtocol.SettingsUpdateRequest): Observable<WebProtocol.GenericResponse> {
        const options = { headers: new HttpHeaders({'Content-Type': 'application/json' }) };

        return this.http
            .put(`${this.baseUrl}/api/admin/settings`, request.toJson(), options)
            .pipe(
                map(response => WebProtocol.GenericResponse.fromJson(response as Igor.Json.JsonValue))
            );
    }
}
