import { Component, OnInit } from '@angular/core';
import { NgxSpinnerService } from 'ngx-spinner';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { Player } from 'src/shared/models/player/player.model';
import { AiService } from '../../services/ai.service';
import { Location } from '@angular/common';


@Component({
  selector: 'app-shortest-route',
  templateUrl: './shortest-route.component.html',
  styleUrls: ['./shortest-route.component.css']
})
export class ShortestRouteComponent implements OnInit {

  step: number;

  shortestRoute: string[];

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
      this.getshortestRoute(email.trim(), "email3@gmail.com");
    } else{
      this.error = true;
    }
  }

  setStep(index: number) {
    this.step = index;
  }

  getshortestRoute(emailPlayer: string, emailTarget: string): void {
    this.spinner.show();
    this.aiService.getshortestRoute(emailPlayer, emailTarget).subscribe({ next: data => {
      this.shortestRoute = data;
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
      for(let playerId of this.shortestRoute) {
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
