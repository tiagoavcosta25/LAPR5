import { Component, OnInit } from '@angular/core';
import { NgxSpinnerService } from 'ngx-spinner';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { Player } from 'src/shared/models/player/player.model';
import { AiService } from '../../services/ai.service';
import { Location } from '@angular/common';


@Component({
  selector: 'app-safest-route',
  templateUrl: './safest-route.component.html',
  styleUrls: ['./safest-route.component.css']
})
export class SafestRouteComponent implements OnInit {

  step: number;

  safestRoute: string[];

  error: boolean;

  players: Player[];

  tempPlayers: Player[];


  constructor(private aiService: AiService,
    private pService: PlayerService,
    private location: Location,
    private spinner: NgxSpinnerService) { }

  ngOnInit(): void {
    let email = localStorage.getItem('currentPlayer');
    if(email != null){
      this.getSafestRoute(email.trim(), "email3@gmail.com", 0);
    }
  }

  setStep(index: number) {
    this.step = index;
  }

  getSafestRoute(emailPlayer: string, emailTarget: string, threshold: number): void {
    this.spinner.show();
    this.aiService.getSafestRoute(emailPlayer, emailTarget, threshold).subscribe({ next: data => {
      this.safestRoute = data;
      this.getPlayers();
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
        this.error = true;
      }
    });
  } 

  getPlayers(): void {
    this.pService.getPlayers().subscribe({ next: data => {
      this.tempPlayers = data;
      var lst = [];
      for(let playerId of this.safestRoute) {
        for(let player of this.tempPlayers) {
          if(player.id == playerId) {
            lst.push(player);
            break;
          }
        }
      }
      this.players = lst;
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
        this.error = true;
      }
    });
  } 

  getLastPlayerName(): string {
    return this.players[this.players.length - 1].name;
  }

  goBack(): void {
    this.location.back();
  }

  refresh(): void {
    window.location.reload();
  }

}
