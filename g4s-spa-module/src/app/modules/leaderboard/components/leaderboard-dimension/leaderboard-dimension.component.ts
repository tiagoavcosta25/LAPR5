import { Component, OnInit } from '@angular/core';
import { NgxSpinnerService } from 'ngx-spinner';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { Player } from 'src/shared/models/player/player.model';

@Component({
  selector: 'app-leaderboard-dimension',
  templateUrl: './leaderboard-dimension.component.html',
  styleUrls: ['./leaderboard-dimension.component.css']
})
export class LeaderboardDimensionComponent implements OnInit {

  unorderedPlayers: Player[];
  orderedMap: Map<Player, number>;
  leaderboardMap: Map<Player, number>;

  constructor(private pService: PlayerService,
    private cService: ConnectionService,
    private spinner: NgxSpinnerService) {}

  ngOnInit() {
    this.spinner.show();
    this.unorderedPlayers = [];
    this.leaderboardMap = new Map<Player, number>();
    this.getPlayers();
  }

  getPlayers() {
    this.pService.getPlayers()
      .subscribe({ next: data => {
        for(let player of data) {
          this.unorderedPlayers.push(player);
        }

        this.getNetworkDimension();
      },
        error: _error => {
          this.spinner.hide();
        }
      });
  }

  getNetworkDimension() {
    if(this.unorderedPlayers.length === 0){
      let tempMap = new Map([...this.leaderboardMap.entries()].sort((a, b) => b[1] - a[1]));
      this.orderedMap = tempMap;
      this.spinner.hide();
    }

    let player = this.unorderedPlayers.pop()!;

    if(player === undefined){
      this.spinner.hide();
    }

      this.cService.getNetwork(player.email, 1).subscribe({ next: data => {
          this.leaderboardMap.set(player, data.length);
          this.getNetworkDimension();
      },
        error: _error => {
          this.spinner.hide();
        }
      });
  }

}
