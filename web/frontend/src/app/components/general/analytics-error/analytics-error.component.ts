import { Component, OnInit, Input } from '@angular/core';
import { AnalyticsErrorTypes } from '../../../shared/error-types/analytics-error-types';
import { ChartBoxExpansionService } from '../../../core/services/chart-box-expansion.service';

@Component({
  selector: 'm-analytics-error',
  templateUrl: './analytics-error.component.html',
  styleUrls: ['./analytics-error.component.scss']
})
export class AnalyticsErrorComponent implements OnInit {

  constructor(public chartBoxExpansionService: ChartBoxExpansionService) { }


  ErrorTypes = AnalyticsErrorTypes;
  @Input() currentErrorType: string;
  @Input() errorMessage: string;

  ngOnInit(): void {
  }

}
