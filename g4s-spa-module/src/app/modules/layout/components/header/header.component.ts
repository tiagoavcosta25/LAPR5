import { Component, OnInit } from '@angular/core';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { SignalrService } from '../../services/signalr.service';

@Component({
  selector: 'app-header',
  templateUrl: './header.component.html',
  styleUrls: ['./header.component.css']
})
export class HeaderComponent implements OnInit {

  constructor(public sService: SignalrService,
    public pService: PlayerService) { }

  ngOnInit(): void {
    this.sService.addPlayerNumberListener();
    this.getPlayer();
  }

  logout(){
    localStorage.removeItem('currentPlayer');
  }

  getPlayer(): void {
    this.pService.getPlayerNumber().subscribe({ next: data => {
      this.sService.playerNumber = data;
    },
      error: _error => {
        console.log(_error);
      }
    });
  }
}
