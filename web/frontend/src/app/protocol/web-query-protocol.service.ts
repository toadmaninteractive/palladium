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
import * as CardProtocol from './card-protocol';

@Injectable({
    providedIn: 'root',
})
export class PalladiumQueryService {
    public baseUrl = '';

    constructor(private http: HttpClient) { }

    public getProject(project: string): Observable<WebProtocol.ProjectConfig> {
        return this.http
            .get(`${this.baseUrl}/api/projects/${project}`)
            .pipe(
                map(response => WebProtocol.ProjectConfig.fromJson(response as Igor.Json.JsonValue))
            );
    }

    public getProjects(): Observable<DataProtocol.Collection<WebProtocol.ProjectConfig>> {
        return this.http
            .get(`${this.baseUrl}/api/projects`)
            .pipe(
                map(response => DataProtocol.Collection.instanceJsonSerializer<WebProtocol.ProjectConfig>(WebProtocol.ProjectConfig).fromJson(response as Igor.Json.JsonValue))
            );
    }

    public getMyRolesForProject(project: string): Observable<WebProtocol.PersonnelAccountRole> {
        return this.http
            .get(`${this.baseUrl}/api/projects/${project}/roles/me`)
            .pipe(
                map(response => WebProtocol.PersonnelAccountRole.fromJson(response as Igor.Json.JsonValue))
            );
    }

    public getDashboardData(project: string): Observable<WebProtocol.DashboardData> {
        return this.http
            .get(`${this.baseUrl}/api/dashboard/data/${project}`)
            .pipe(
                map(response => WebProtocol.DashboardData.fromJson(response as Igor.Json.JsonValue))
            );
    }

    public sendWidgetRequest(request: Igor.Json.JsonValue, project: string, database: string, query: string): Observable<WebProtocol.WidgetQueryResult> {
        const options = { headers: new HttpHeaders({'Content-Type': 'application/json' }) };

        return this.http
            .put(`${this.baseUrl}/api/widgets/query/${project}/${database}/${query}`, request, options)
            .pipe(
                map(response => WebProtocol.WidgetQueryResult.fromJson(response as Igor.Json.JsonValue))
            );
    }

    public getAnalyticsData(project: string): Observable<WebProtocol.AnalyticsData> {
        return this.http
            .get(`${this.baseUrl}/api/analytics/data/${project}`)
            .pipe(
                map(response => WebProtocol.AnalyticsData.fromJson(response as Igor.Json.JsonValue))
            );
    }

    public sendAnalyticsRequest(request: Igor.Json.JsonValue, project: string, database: string, query: string): Observable<WebProtocol.AnalyticsQueryResult> {
        const options = { headers: new HttpHeaders({'Content-Type': 'application/json' }) };

        return this.http
            .put(`${this.baseUrl}/api/analytics/query/${project}/${database}/${query}`, request, options)
            .pipe(
                map(response => WebProtocol.AnalyticsQueryResult.fromJson(response as Igor.Json.JsonValue))
            );
    }

    public retrieveParameterValues(request: Igor.Json.JsonValue, project: string, database: string, param: string): Observable<DataProtocol.Collection<DataProtocol.JsonPoint>> {
        const options = { headers: new HttpHeaders({'Content-Type': 'application/json' }) };

        return this.http
            .put(`${this.baseUrl}/api/parameters/values/${project}/${database}/${param}`, request, options)
            .pipe(
                map(response => DataProtocol.Collection.instanceJsonSerializer<DataProtocol.JsonPoint>(DataProtocol.JsonPoint).fromJson(response as Igor.Json.JsonValue))
            );
    }

    public getEvents(project: string): Observable<WebProtocol.EventData> {
        return this.http
            .get(`${this.baseUrl}/api/events/${project}`)
            .pipe(
                map(response => WebProtocol.EventData.fromJson(response as Igor.Json.JsonValue))
            );
    }

    public getPatchNotes(project: string): Observable<DataProtocol.Collection<CardProtocol.CardPatchNote>> {
        return this.http
            .get(`${this.baseUrl}/api/patch-notes/${project}`)
            .pipe(
                map(response => DataProtocol.Collection.instanceJsonSerializer<CardProtocol.CardPatchNote>(CardProtocol.CardPatchNote).fromJson(response as Igor.Json.JsonValue))
            );
    }
}
