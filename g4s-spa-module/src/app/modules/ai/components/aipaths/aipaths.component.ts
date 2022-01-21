import { Component, OnInit } from '@angular/core';
import { NgxSpinnerService } from 'ngx-spinner';
import { firstValueFrom } from 'rxjs';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { Connection } from 'src/shared/models/connection/connection.model';
import { Player } from 'src/shared/models/player/player.model';
import { AiService } from '../../services/ai.service';

@Component({
  selector: 'app-aipaths',
  templateUrl: './aipaths.component.html',
  styleUrls: ['./aipaths.component.css']
})
export class AipathsComponent implements OnInit {

  algo: string;

  multicriteria: boolean | undefined;

  n: number | undefined;

  tempN: number | undefined = 1;

  currentUserEmail: string;

  players: Player[] = [];

  networkPlayers: Player[] | undefined;

  tempTarget: string | undefined;

  targetEmail: string | undefined;

  constructor(private cService: ConnectionService,
    private pService: PlayerService,
    private aService: AiService,
    private spinner: NgxSpinnerService) { }

  ngOnInit(): void {
    this.getPlayers();
    this.currentUserEmail = localStorage.getItem("currentPlayer")!;
  }

  setAlgo(algo: string) {
    this.algo = algo;
    this.multicriteria = undefined;
    this.n = undefined;
    this.networkPlayers = undefined;
    this.targetEmail = undefined;
  }

  setMulticriteria(multi: boolean) {
    this.multicriteria = multi;
    this.n = undefined;
    this.networkPlayers = undefined;
    this.targetEmail = undefined;
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
    this.n = this.tempN;
    this.networkPlayers = undefined;
    this.targetEmail = undefined;
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

  async getNt(): Promise<Connection[]>{
    return await firstValueFrom(this.cService.getNetwork(this.currentUserEmail, this.n!));
  }

  calculate(): void {
    this.targetEmail = this.tempTarget;
    console.log(this.algo, this.multicriteria, this.n, this.currentUserEmail, this.targetEmail);
  }
}
