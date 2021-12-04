import { Component, OnInit } from '@angular/core';
import { Validators, FormBuilder, FormControl } from '@angular/forms';
import { NgxSpinnerService } from 'ngx-spinner';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { Connection } from 'src/shared/models/connection/connection.model';
import { Player } from 'src/shared/models/player/player.model';

@Component({
  selector: 'app-get-network',
  templateUrl: './get-network.component.html',
  styleUrls: ['./get-network.component.css']
})
export class GetNetworkComponent implements OnInit {

  getNetworkForm = this.fb.group({
    scope: ['', [Validators.required, Validators.min(1), Validators.max(10)]]
  });

  email: string;
  success: any;
  playersIds: string[];
  players: Player[];
  network: Connection[];
  showForm: boolean = true;
  showGraph: boolean = false;

  constructor(
    private spinner: NgxSpinnerService,
    private cService: ConnectionService,
    private pService: PlayerService,
    private fb: FormBuilder) { 

      this.showForm = true;
      this.showGraph = false
    }

  ngOnInit(): void {
    this.network = [];
    this.playersIds = [];
    this.players = [];
    this.email = "jane@email.com";
  }

  getPlayersByScope(){
      this.spinner.show();

      /*this.cService.getNetwork(this.email, this.getNetworkForm.value.scope).subscribe({ next: data => {
        this.network = data;
      },
        error: _error => {
        }
      });*/

      this.mockGetNetwork();

      this.getPlayers();

      this.initializeGraph();

      this.spinner.hide();
  }

  mockGetNetwork(){
    let con1 = new Connection();
    con1.player = '115ac456-8a98-4e60-9966-02c38dd0ee64';
    con1.friend = 'a0164888-d6af-4fd5-ba01-ffce1d1cd0a0';
    con1.connectionStrength = 4;
    con1.tags = ['gaming', 'coding'];
    let con2 = new Connection();
    con2.player = 'a0164888-d6af-4fd5-ba01-ffce1d1cd0a0';
    con2.friend = '873277b7-6e1d-482f-9a71-e899ceaebfbd';
    con2.connectionStrength = 4;
    this.network = [con1, con2];
    con2.tags = ['music', 'coding'];
    console.log(this.network);
  }

  getPlayers(){
    for(let c of this.network){
      if(!this.playersIds.includes(c.player)){
        this.playersIds.push(c.player);
        this.pService.getPlayerById(c.player).subscribe({ next: data => {
          this.players.push(data);
        },
          error: _error => {
          }
        });
      }
      if(!this.playersIds.includes(c.friend)){
        this.playersIds.push(c.friend);
        this.pService.getPlayerById(c.friend).subscribe({ next: data => {
          this.players.push(data);
        },
          error: _error => {
          }
        });
      }
    }
  }

  getErrorMessageScopeRequired() {
    return this.getNetworkForm.controls['scope'].hasError('required') ? 'Scope is required' : '';
  }

  getErrorMessageScopeInvalid() {
    return (this.getNetworkForm.controls['scope'].hasError('min') || this.getNetworkForm.controls['scope'].hasError('max'))
      ? 'Scope should be between 1 and 10' : '';
  }

  clearSucess() {
    delete this.success;
  }

  get f() { return this.getNetworkForm.controls; }

  initializeGraph(){
    this.showForm = false;
    this.showGraph = true;

    console.log(this.players);

  }

}
