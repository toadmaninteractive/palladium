import { Pipe, PipeTransform } from '@angular/core';
import * as Visualization from '../../protocol/visualization-protocol'

@Pipe({
    name: 'VisualizationCheckType'
})
export class VisualizationCheckTypePipe implements PipeTransform {

    transform(value: any): any {
        if (value instanceof Visualization.VisualizationIndicator) return 'indicator';
        else if (value instanceof Visualization.VisualizationCommonPlot) return 'commonPlot';
        else if (value instanceof Visualization.VisualizationHorizontalBars) return 'horizontalBars';
        else if (value instanceof Visualization.VisualizationFunnel) return 'funnel';
        else if (value instanceof Visualization.VisualizationSynchronizedPlot) return 'synchronized';
        else if (value instanceof Visualization.VisualizationScatter) return 'scatter';
        return undefined;
    }
}
