import { Component, OnInit } from '@angular/core';
import { NgxSpinnerService } from 'ngx-spinner';
import { firstValueFrom } from 'rxjs';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { Connection } from 'src/shared/models/connection/connection.model';
import { Player } from 'src/shared/models/player/player.model';
import { AiService } from '../../services/ai.service';
import { Location } from '@angular/common';

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

  n: number | undefined;

  tempN: number | undefined = 1;

  currentUserEmail: string;

  players: Player[] = [];

  networkPlayers: Player[] | undefined;

  tempTarget: string | undefined;

  targetEmail: string | undefined;

  path: string[] | undefined;

  cost: number | undefined;

  step: number;

  constructor(private cService: ConnectionService,
    private pService: PlayerService,
    private aService: AiService,
    private spinner: NgxSpinnerService,
    private location: Location) { }

  ngOnInit(): void {
    this.getPlayers();
    this.currentUserEmail = localStorage.getItem("currentPlayer")!;
  }

  setAlgo(algo: string) {
    if(this.algo == algo) {
      return;
    }
    this.algo = algo;
    this.multicriteria = undefined;
    this.n = undefined;
    this.networkPlayers = undefined;
    this.targetEmail = undefined;
    this.path = undefined;
    this.algoPlayers = undefined;
    this.cost = 0;
  }

  setMulticriteria(multi: boolean) {
    if(this.multicriteria == multi) {
      return;
    }
    this.multicriteria = multi;
    this.n = undefined;
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

  getAlgo(): void {
    this.spinner.show();
    let mode: number;
    if(this.multicriteria) {
      mode = 1;
    } else {
      mode = 0;
    }
    if(this.algo == undefined || this.targetEmail == undefined || this.multicriteria == undefined || this.n == undefined) {
      return;
    }
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
    this.n = undefined;
    this.networkPlayers = undefined;
    this.targetEmail = undefined;
    this.path = undefined;
    this.algoPlayers = undefined;
    this.cost = 0;
  }
}
