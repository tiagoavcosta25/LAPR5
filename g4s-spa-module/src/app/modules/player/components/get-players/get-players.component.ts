import { Component, OnInit } from '@angular/core';
import { Player } from 'src/shared/models/player.model';
import { PlayerServiceService } from '../../services/player-service.service';

@Component({
  selector: 'app-get-players',
  templateUrl: './get-players.component.html',
  styleUrls: ['./get-players.component.css']
})
export class GetPlayersComponent implements OnInit {

  players: Player[] = [];

  constructor(private playersService: PlayerServiceService) {}

  ngOnInit() {
    this.getPlayers();
  }

  getPlayers(): void {
    this.playersService.getPlayers()
      .subscribe(players => (this.players = players));
  }

}
