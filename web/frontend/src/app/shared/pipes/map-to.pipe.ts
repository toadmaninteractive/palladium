import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
    name: 'mMapTo'
})
export class MapToPipe implements PipeTransform {
    transform(value: Array<any>, mapFun: (any) => any): any {
        return value.map((v) => mapFun(v));
    }
}
