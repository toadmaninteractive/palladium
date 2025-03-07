import { Injectable } from '@angular/core';
import { Subject } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class ChartBoxExpansionService {

  public expand = new Subject<{ delay: number, containerKey: string }>();
  isExpanded = false;

}
