import { Component, OnInit } from '@angular/core';
import { NgxSpinnerService } from 'ngx-spinner';
import { firstValueFrom } from 'rxjs';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { Connection } from 'src/shared/models/connection/connection.model';
import { Player } from 'src/shared/models/player/player.model';
import { AiService } from '../../services/ai.service';
import { Location } from '@angular/common';
import { DobPlayer } from 'src/app/modules/player/models/dob-player.model copy';

@Component({
  selector: 'app-aipaths',
  templateUrl: './aipaths.component.html',
  styleUrls: ['./aipaths.component.css']
})
export class AipathsComponent implements OnInit {

  tempPlayers: Player[];

  algoPlayers: Player[] | undefined;
 
  algo: string | undefined;

  multicriteria: boolean | undefined;

  emotions: boolean | undefined;

  n: number | undefined;

  tempN: number | undefined = 1;

  currentUserEmail: string;

  currentUser: DobPlayer;

  players: Player[] = [];

  networkPlayers: Player[] | undefined;

  tempTarget: string | undefined;

  targetEmail: string | undefined;

  path: string[] | undefined;

  cost: number | undefined;

  step: number;

  joy: number = 0.25;
  
  anguish: number = 0.25;
  
  hope: number = 0.25;
  
  deception: number = 0.25;
  
  fear: number = 0.25;
  
  relief: number = 0.25;

  constructor(private cService: ConnectionService,
    private pService: PlayerService,
    private aService: AiService,
    private spinner: NgxSpinnerService,
    private location: Location) { }

  ngOnInit(): void {
    this.currentUserEmail = localStorage.getItem("currentPlayer")!;
    this.getPlayer();
    this.getPlayers();
  }

  setAlgo(algo: string) {
    if(this.algo == algo) {
      return;
    }
    this.algo = algo;
    this.multicriteria = undefined;
    this.emotions = undefined;
    this.n = undefined;
    this.networkPlayers = undefined;
    this.targetEmail = undefined;
    this.path = undefined;
    this.algoPlayers = undefined;
    this.cost = 0;
    this.resetEmotions();
  }

  setMulticriteria(multi: boolean) {
    if(this.multicriteria == multi) {
      return;
    }
    this.multicriteria = multi;
    this.n = undefined;
    this.emotions = undefined;
    this.networkPlayers = undefined;
    this.targetEmail = undefined;
    this.path = undefined;
    this.algoPlayers = undefined;
    this.cost = 0;
    this.resetEmotions();
  }

  setEmotions(emot: boolean) {
    if(this.emotions == emot) {
      return;
    }
    this.n = undefined;
    this.emotions = emot;
    this.networkPlayers = undefined;
    this.targetEmail = undefined;
    this.path = undefined;
    this.algoPlayers = undefined;
    this.cost = 0;
  }

  getMulticriteria(): string {
    if(this.multicriteria == undefined) {
      return "";
    } else if(this.multicriteria) {
      return "yes";
    }
    return "no";
  }

  getEmotions(): string {
    if(this.emotions == undefined) {
      return "";
    } else if(this.emotions) {
      return "yes";
    }
    return "no";
  }

  async setN() {
    if(this.n == this.tempN) {
      return;
    }
    this.n = this.tempN;
    this.networkPlayers = undefined;
    this.targetEmail = undefined;
    this.path = undefined;
    this.algoPlayers = undefined;
    this.cost = 0;
    let cons = await this.getNt();
    let np:Player[] = [];
    for(let c of cons) {
      if(!np.some(x => x.id == c.friend)) {
        for(let p of this.players) {
          if(p.id == c.friend) {
            np.push(p);
            break;
          }
        }
      }
    }
    this.networkPlayers = np;
  }

  getPlayers(): void {
    this.spinner.show();
    this.pService.getPlayers().subscribe({ next: data => {
      this.players = data;
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
      }
    });
  }

  getPlayer(): void {
    this.spinner.show();
    this.pService.getOnlyPlayerByEmail(this.currentUserEmail).subscribe({ next: data => {
      this.currentUser = data;
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
      }
    });
  }

  getAlgo(): void {
    this.spinner.show();
    let mode: number;
    if(this.multicriteria) {
      mode = 1;
    } else {
      mode = 0;
    }
    if(this.algo == undefined || this.targetEmail == undefined || this.multicriteria == undefined || this.emotions == undefined || this.n == undefined) {
      return;
    }
    if(this.emotions == true) {
      this.aService.getAiPathEmotions(this.algo, this.currentUserEmail, this.targetEmail, mode, this.n,
        this.joy, this.anguish, this.hope, this.deception, this.fear, this.relief).subscribe({ next: data => {
        this.path = data[0];
        this.cost =+ data[1];
        this.getPlayersAlgo();
        console.log(this.path, this.cost);
      },
        error: _error => {
          this.spinner.hide();
        }
      });
    } else {
      this.aService.getAiPath(this.algo, this.currentUserEmail, this.targetEmail, mode, this.n).subscribe({ next: data => {
        this.path = data[0];
        this.cost =+ data[1];
        this.getPlayersAlgo();
        console.log(this.path, this.cost);
      },
        error: _error => {
          this.spinner.hide();
        }
      });
    }
  }

  getPlayersAlgo(): void {
    this.pService.getPlayers().subscribe({ next: data => {
      this.tempPlayers = data;
      var lst = [];
      if(this.path == undefined) {
        return;
      }
      for(let playerId of this.path) {
        for(let player of this.tempPlayers) {
          if(player.id == playerId) {
            lst.push(player);
            break;
          }
        }
      }
      this.algoPlayers = lst;
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
      }
    });
  } 

  async getNt(): Promise<Connection[]>{
    return await firstValueFrom(this.cService.getNetwork(this.currentUserEmail, this.n!));
  }

  calculate(): void {
    this.targetEmail = this.tempTarget;
    this.getAlgo();
    console.log(this.algo, this.multicriteria, this.n, this.currentUserEmail, this.targetEmail);
  }

  getLastPlayerName(): string {
    if(this.algoPlayers == undefined) {
      return "null";
    }
    return this.algoPlayers[this.algoPlayers.length - 1].name;
  }

  setStep(index: number) {
    this.step = index;
  }

  goBack(): void {
    this.location.back();
  }

  reset() {
    this.algo = undefined;
    this.multicriteria = undefined;
    this.emotions = undefined;
    this.n = undefined;
    this.networkPlayers = undefined;
    this.targetEmail = undefined;
    this.path = undefined;
    this.algoPlayers = undefined;
    this.cost = 0;
  }

  resetEmotions() {
    this.joy = 0.25;
    this.anguish = 0.25;
    this.hope = 0.25;
    this.deception = 0.25;
    this.fear = 0.25;
    this.relief = 0.25;
  }

  setEmotion() {
    let currentEmotion = this.currentUser.emotionalStatus;
    switch(currentEmotion) {
      case "joyful":
        this.joy = 0.75;
        break;
      case "distressed":
        this.anguish = 0.75;
        break;
      case "hopeful":
        this.hope = 0.75;
        break;
      case "disappointed":  
        this.deception = 0.75;
        break;
      case "fearful":
        this.fear = 0.75;
        break;
      case "relieve":
        this.relief = 0.75;
        break;
    }
  }
}
