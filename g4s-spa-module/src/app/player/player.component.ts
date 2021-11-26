import { Component, OnInit } from '@angular/core';
import { Player } from '../player';
import { PLAYERS } from '../mock-players';

@Component({
  selector: 'app-player',
  templateUrl: './player.component.html',
  styleUrls: ['./player.component.css']
})
export class PlayerComponent implements OnInit {

  constructor() { }

  ngOnInit(): void {
  }

  players = PLAYERS;

  selectedPlayer?: Player;
  onSelect(player: Player): void {
  this.selectedPlayer = player;
}

}
