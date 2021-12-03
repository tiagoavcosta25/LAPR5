import { Component, OnInit } from '@angular/core';
import { Player } from 'src/shared/models/player/player.model';
import { PlayerService } from '../../services/player.service';

@Component({
  selector: 'app-get-players',
  templateUrl: './get-players.component.html',
  styleUrls: ['./get-players.component.css']
})
export class GetPlayersComponent implements OnInit {

  players: Player[] = [];

  constructor(private playersService: PlayerService) {}

  ngOnInit() {
    this.getPlayers();
  }

  getPlayers(): void {
    this.playersService.getPlayers()
      .subscribe(players => (this.players = players));
  }

}
