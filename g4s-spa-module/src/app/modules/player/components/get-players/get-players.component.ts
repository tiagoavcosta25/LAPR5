import { Component, OnInit } from '@angular/core';
import { Player } from 'src/shared/models/player/player.model';
import { PlayerService } from '../../services/player.service';
import { NgxSpinnerService } from 'ngx-spinner';

@Component({
  selector: 'app-get-players',
  templateUrl: './get-players.component.html',
  styleUrls: ['./get-players.component.css']
})
export class GetPlayersComponent implements OnInit {

  players: Player[] = [];

  constructor(private playersService: PlayerService,
    private spinner: NgxSpinnerService) {}

  ngOnInit() {
    this.getPlayers();
  }

  getPlayers(): void {
    this.spinner.show();
    this.playersService.getPlayers()
      .subscribe({ next: data => {
        if(data) {
          this.players = data;
        }
        this.spinner.hide();
      },
        error: _error => {
          this.spinner.hide();
        }
      });
  }

}
