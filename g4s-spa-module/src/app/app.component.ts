import { Component, OnInit } from '@angular/core';
import { SignalrService } from './modules/layout/services/signalr.service';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent implements OnInit{
  
  constructor(public sService: SignalrService) {
  }

  ngOnInit(): void {
    this.sService.startConnection();
  }
  title = 'Graphs4Social';
}
