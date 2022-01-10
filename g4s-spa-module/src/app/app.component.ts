import { Component, OnInit } from '@angular/core';
import { SignalrService } from './modules/layout/services/signalr.service';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent implements OnInit{
  
  constructor() {
  }

  ngOnInit(): void {
  }
  title = 'Graphs4Social';
}
