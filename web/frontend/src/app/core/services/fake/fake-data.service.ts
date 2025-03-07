import { Injectable } from '@angular/core';
import { Observable, of } from 'rxjs';
import { SelectItem } from '../../../shared/interfaces/select-item';
import { PlayerEvent } from '../../../shared/interfaces/player-event';
import { D3ScatterPoint } from '../../../components/charts/shared/d3-common-options';
import { QueryNode, ScatterSeries } from 'src/app/protocol/data-protocol';
import * as CardProtocol from 'src/app/protocol/card-protocol';
import { CardCategory } from 'src/app/protocol/card-protocol';
import * as WebQueryProtocol from 'src/app/protocol/web-query-protocol.service';
import { AxisValueType, ChartSerie, ChartType, VisualizationCommonPlot, VisualizationIndicator, VisualizationKind } from '../../../protocol/visualization-protocol';

@Injectable({
    providedIn: 'root'
})
export class FakeDataService {

    colorPalette =  [
        '#6bcaff', '#ff7d85', '#ffd363', '#9edb89',
        '#b1a6fe', '#fca77d', '#86ead6', '#218fcc',
        '#ef5b64', '#dda820', '#6ac04c', '#cb7212',
        '#2fc3a5', '#0970aa', '#d83a43', '#ac7c00',
        '#54921f', '#501f76', '#a15402'];

    constructor(
        private queryService: WebQueryProtocol.PalladiumQueryService,
    ) {
    }

    getData(key: string): number {
        switch (key) {
            case 'tuu':
                return Math.round(Math.random() * 10000);
            case 'asl':
                return Math.round(Math.random() * 1000) / 100;
            case 'nut':
                return Math.round(Math.random() * 100);
            case 'dau':
                return Math.round(Math.random() * 100);
            case 'mau':
                return Math.round(Math.random() * 1000);
            default:
                return 0;
        }
    }

    getUniqueUsersByPlatform(): Observable<SelectItem[]> {
        return of([
            {label: 'PC', value: Math.round(Math.random() * 1000000)},
            {label: 'Playstation 4', value: Math.round(Math.random() * 1000000)},
            {label: 'XBOX', value: Math.round(Math.random() * 1000000)},
            {label: 'Android', value: Math.round(Math.random() * 1000000)}
        ]);
    }

    getEvents(offset, limit): Observable<PlayerEvent[]> {
        const result = [];
        for (let index = 0; index < limit; index++) {
            result.push(this.eventFabric(offset, limit));
        }
        return of(result);
    }

    eventFabric(offset: number, limit: number): PlayerEvent {
        const eventsTitle = ['player_received_damage', 'player_deal_damage', 'character_created', 'character_deleted', 'session_start', 'session_finish'];
        const eventsStatusTitle = ['a', 'i', 'd', 't'];
        const statusArr = [];
        for (let index = 0; index < 3; index++) {
            statusArr.push({label: eventsStatusTitle[Math.floor(Math.random() * 4)], value: Math.random() >= 0.5});
        }
        const event: PlayerEvent = {
                id: Math.floor(Math.random() * limit) + offset,
                eventName: eventsTitle[Math.floor(Math.random() * eventsTitle.length )],
                status: statusArr,
                dateAdded: new Date(new Date(2017, 0, 1).getTime() + Math.random() * (new Date().getTime() - new Date(2017, 0, 1).getTime())).toLocaleDateString() ,
                parameterList: [{label: 'event_name', value: true}, {label: 'game_id', value: false}]
        };
        return event;
    }


    getScatterPoint(): Observable<ScatterSeries[]> {
        const result = Array<ScatterSeries>();
        const yNumeric = false;
        const xNumeric = true;

        let yAxisValues = [];
        let xAxisValues = [];

        if (yNumeric) {
            const max = this.getRandomInt(60);
            for (let i = 0; i < max; i++) {
                yAxisValues.push(this.getRandomArbitraryFloat(0, 100).toString());
            }
        } else {
            yAxisValues = [
                'unit_hero_ninja',
                'unit_hero_hunter',
                'unit_hero_sarge_stone',
                'unit_hero_astro',
                'unit_hero_engineer',
                'unit_hero_doc_eliza',
                'unit_hero_boxer',
                'unit_hero_trondson',
                'unit_hero_magnus',
                'unit_hero_abe',
                'unit_hero_cogwheel',
                'unit_hero_kreepy',
                'unit_hero_kira',
                'unit_hero_djinn',
                'unit_hero_roly'];

        }
        if (xNumeric) {
            const max = this.getRandomInt(60);
            for (let i = 0; i < max; i++) {
                xAxisValues.push(this.getRandomArbitraryFloat(0, 100).toString());
            }
        } else {
            xAxisValues = [ 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Sunday'];
        }

        const nrOfSeries = 3;
        for (let i = 0; i < nrOfSeries; i++) {
            const oneDataSet = Object();
            oneDataSet.seriesKey = this.randomStringGenerator();
            oneDataSet.seriesTitle = oneDataSet.seriesKey;
            oneDataSet.seriesColor = this.colorPalette[this.getRandomInt(this.colorPalette.length)];
            oneDataSet.data = [];
            yAxisValues.forEach(yValue => {
                const point = {} as D3ScatterPoint;
                    point.x = xAxisValues[this.getRandomInt(xAxisValues.length)];
                    point.y = yValue;
                    point.value = this.getRandomArbitraryFloat(0, 1000) + 1;
                    oneDataSet.data.push(point);
            });
            oneDataSet.data[0] = {
                    x: '0',
                    y: yAxisValues[0],
                    value: this.getRandomArbitraryFloat(0, 1000) + 1,
                };

            result.push(oneDataSet);
        }
        return of(result);
    }

    randomStringGenerator(): string {
        const alphabet = 'qwertyuiopasdfghjklzxcvbnm';
        let word = '';
        for (let i = 0; i < 6; i++) {
            word += alphabet[Math.round(Math.random() * (alphabet.length - 1))];
        }
        return word;
    }

    getRandomInt(max) {
        return Math.floor(Math.random() * Math.floor(max));
    }

    getRandomArbitraryFloat(min, max) {
        return Math.random() * (max - min) + min;
    }

    getDashboardCardWidgets(): Observable<Array<CardProtocol.CardWidget>> {
        const widget = new CardProtocol.CardWidget();
        widget.id = '1';
        widget.category = CardCategory.Widget;
        widget.key = 'first';
        widget.params = null;
        widget.widthCols = 4;

        const query = new QueryNode(),
            output = new VisualizationIndicator();

        output.description = 'widget description';
        output.kind = VisualizationKind.Indicator;
        output.label = 'widget label';
        output.name = 'widget name';
        output.subtitle = 'widget subtitle';

        query.output = output;
        query.inputSql = 'sql query';

        widget.query = query;

        const widget2 = new CardProtocol.CardWidget();
        widget2.id = '2';
        widget2.category = CardCategory.Widget;
        widget2.key = 'second';
        widget2.params = null;
        widget2.widthCols = 6;

        const query2 = new QueryNode(),
            output2 = new VisualizationIndicator();

        output2.description = 'second widget description';
        output2.kind = VisualizationKind.Indicator;
        output2.label = 'second widget label';
        output2.name = 'second widget name';
        output2.subtitle = 'second widget subtitle';

        query2.output = output2;
        query2.inputSql = 'second sql query2';

        widget2.query = query2;

        const widget3 = new CardProtocol.CardWidget();
        widget3.id = 'query_unique_sessions';
        widget3.category = CardCategory.Widget;
        widget3.key = 'unique_sessions';
        widget3.params = ['query_param_time_period', 'query_param_time_scale'];
        widget3.widthCols = 8;

        const query3 = new QueryNode(),
            output3 = new VisualizationCommonPlot();

        output3.description = 'The number of unique sessions for each historical period. AUS region has been excluded because AUS region servers have been shut down.';
        output3.kind = VisualizationKind.CommonPlot;
        output3.name = 'Unique sessions';
        output3.secondaryYAxisTitle = null;
        output3.secondaryYAxisType = null;

        const serie1 = new ChartSerie();
        serie1.chartType = ChartType.Line;
        serie1.showValues = true;
        serie1.name =  'Total';
        serie1.key =  'value';
        serie1.groupIndex =  0;
        serie1.color =  null;
        serie1.yAxis =  1;

        const serie2 = new ChartSerie();
        serie2.chartType = ChartType.Line;
        serie2.showValues = true;
        serie2.name =  'EU';
        serie2.key =  'value_eu';
        serie2.groupIndex =  0;
        serie2.color =  null;
        serie2.yAxis =  1;

        const serie3 = new ChartSerie();
        serie3.chartType = ChartType.Line;
        serie3.showValues = true;
        serie3.name =  'US';
        serie3.key =  'value_us';
        serie3.groupIndex =  0;
        serie3.color =  null;
        serie3.yAxis =  1;

        output3.series = [serie1, serie2, serie3];
        output3.showLegend =  true;
        output3.showTimeline =  true;
        output3.showXAxis =  true;
        output3.showXGridLine =  false;
        output3.showYAxis =  true;
        output3.showYGridLine =  true;
        output3.subtitle =  'Shows the number of unique sessions across all regions';
        output3.xAxisTitle =  '';
        output3.xAxisType = AxisValueType.Date;
        output3.yAxisTitle =  'Number of sessions';
        output3.yAxisType =  AxisValueType.Numeric;
        query3.output = output3;
        query3.inputSql = 'second sql query3';

        widget3.query = query3;

        return of([widget, widget2, widget3]);
    }

    getWidgetQueryResult(widgetKey: string): Observable<any> {
        switch (widgetKey) {
            case 'first': return of(this.getRandomInt(50));
            case 'second': return of(this.getRandomInt(10));
            case 'chart': return this.queryService.sendAnalyticsRequest({time: {from: '2020-05-03T03:00:00.000Z', to: '2020-06-03T03:00:00.000Z'}, partition: 'day'}, 'bnl', 'ta_blocknload_prod', 'unique_sessions');

        }
    }

    getWidgetDateRange(): Observable<any> {
        const date = new Date();
        const defaultTo = new Date(date);
        let defaultFrom = new Date(date);
        defaultFrom = new Date(defaultFrom.setDate(defaultFrom.getDate() - 30));
        console.log(date, 'defaultFrom')
        console.log(defaultTo)
        return of({
            begin: defaultFrom,
            end: defaultTo
        });
    }
}
