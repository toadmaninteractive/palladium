export class Widget {
    widgetType: string;
    title: string;
    tooltipCaption: string;
    previous$?: any;
    datepickerDepth: string;
    captionForButton?: string;
    updateWidget: () => void;
    value$: any;
}