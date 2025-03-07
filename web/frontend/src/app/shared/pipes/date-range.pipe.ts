import { Pipe, PipeTransform } from '@angular/core';


@Pipe({
  name: 'dateRange'
})
export class DateRangePipe implements PipeTransform {

  transform(value: any, conversion?: string, ...args: any[]): any {
      let converted;
      switch (conversion) {
          case 'from-request':
              if (value.from && value.to) converted = {begin: new Date(value.from), end: new Date(value.to)}
              break;
          case 'to-string':
              if (value.begin && value.end) {
                  converted = `${this.formatDate(value.begin)} &ndash; ${this.formatDate(value.end)}`
              }
              if (value.from && value.to) {
                  converted = `${value.from.split('T')[0]} &ndash; ${(value.to).split('T')[0]}`
              }
              break;
      }
      return converted;
      // this.dateRange.begin = new Date(this.dateRange.begin.getTime() - this.dateRange.begin.getTimezoneOffset() * 60 * 1000);
      // this.dateRange.end = new Date(this.dateRange.end.getTime() - this.dateRange.end.getTimezoneOffset() * 60 * 1000);
      // this.dateRangeString = `${this.formatDate(this.dateRange.begin)} &ndash; ${this.formatDate(this.dateRange.end)}`;
  }

    private formatDate(date: Date): string {
        const queryDate = new Date(date.getTime() - date.getTimezoneOffset() * 60 * 1000);
        const months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
        return `${months[queryDate.getUTCMonth()]} ${queryDate.getUTCDate()}, ${queryDate.getFullYear()}`;
    }

}
