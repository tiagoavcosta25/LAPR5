import { Component, OnInit } from '@angular/core';
import { NgxSpinner, NgxSpinnerService } from 'ngx-spinner';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { SignalrService } from '../../services/signalr.service';

@Component({
  selector: 'app-header',
  templateUrl: './header.component.html',
  styleUrls: ['./header.component.css']
})
export class HeaderComponent implements OnInit {

  constructor(public sService: SignalrService,
    public pService: PlayerService,
    private spinner: NgxSpinnerService) { }

  ngOnInit(): void {
    this.sService.addPlayerNumberListener();
    this.getPlayer();
  }

  logout(){
    localStorage.removeItem('currentPlayer');
  }

  getPlayer(): void {
    this.spinner.show();
    this.pService.getPlayerNumber().subscribe({ next: data => {
      this.sService.playerNumber = data;
      this.spinner.hide();
    },
      error: _error => {
        console.log(_error);
        this.spinner.hide();
      }
    });
  }
}
