import { Component, OnInit } from '@angular/core';
import { NgxSpinnerService } from 'ngx-spinner';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { Player } from 'src/shared/models/player/player.model';
import { AiService } from '../../services/ai.service';
import { Location } from '@angular/common';


@Component({
  selector: 'app-a-star',
  templateUrl: './a-star.component.html',
  styleUrls: ['./a-star.component.css']
})
export class AStarComponent implements OnInit {

  step: number;

  AStar: string[];

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
      this.getAStar(email.trim(), "email3@gmail.com", 0);
      //this.getAStar('email1@gmail.com', "email3@gmail.com", 0);
    } else{
      this.error = true;
    }
  }

  setStep(index: number) {
    this.step = index;
  }

  getAStar(emailPlayer: string, emailTarget: string, threshold: number): void {
    this.spinner.show();
    this.aiService.getSafestRoute(emailPlayer, emailTarget, threshold).subscribe({ next: data => {
      this.AStar = data;
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
      for(let playerId of this.AStar) {
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
