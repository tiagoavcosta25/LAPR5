import { Component, OnInit} from '@angular/core';
import { FormArray, FormBuilder, Validators, } from '@angular/forms';
import { NgxSpinnerService } from 'ngx-spinner';
import { Player } from 'src/shared/models/player/player.model';
import { UserSearchFilterEnum } from 'src/shared/models/player/user-search-filter.enum';
import { PlayerService } from '../../services/player.service';
import { Location } from '@angular/common';
import { DirectRequest } from 'src/shared/models/requests/direct-request.model';
import { RequestService } from 'src/app/modules/request/services/request.service';
import { CreatingDirectRequest } from 'src/shared/models/requests/creating-direct-request.model';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { DobPlayer } from '../../models/dob-player.model copy';

@Component({
  selector: 'app-search-player',
  templateUrl: './search-player.component.html',
  styleUrls: ['./search-player.component.css']
})
export class SearchPlayerComponent implements OnInit {
  // TODO: Remover isto
  currentPlayer: DobPlayer;

  error: boolean;

  success?: boolean;

  successMessage: string;
  
  successMessageAccept: string = "Request accepted sucessfully! Refreshing in 2 seconds.";

  successMessageDeny: string = "Request denied sucessfully! Refreshing in 2 seconds.";
  
  errorMessage: string = "There was an error with the request!";

  step: number;

  players?: Player[];

  friends: Player[];

  chosenPlayer: Player;

  playerIdSelected: string;

  r: CreatingDirectRequest;

  requestForm = this.fb.group({
    filter: ['', Validators.required],
    value: ['', Validators.required],
    playerToTargetMessage: ['', Validators.required],
    connectionStrength: ['', [Validators.required, Validators.min(1), Validators.max(100)]],
    tags: this.fb.array([])
  })

  constructor(private pService: PlayerService,
    private rService: RequestService,
    private cService: ConnectionService,
    private spinner: NgxSpinnerService,
    private location: Location,
    private fb: FormBuilder) { }

  ngOnInit(): void {
    this.r = new DirectRequest;
    this.friends = [];
    this.currentPlayerUpdate();
  }

  // TODO: Remover
  currentPlayerUpdate(): void {
    this.spinner.show();
    this.pService.getOnlyPlayerByEmail(localStorage.getItem("currentPlayer")!).subscribe({ next: data => {
      this.currentPlayer = data;
      this.getFriends();
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
        this.error = true;
      }
    });
  }

  getFriends(): void {
    this.spinner.show();
    var connections = [];
    this.cService.getConnections(this.currentPlayer.email).subscribe({ next: data => {
      connections = data;
      for(let con of connections) {
        this.friends.push(con.friend);
      }
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
        this.error = true;
      }
    });
  }

  checkIfFriend(player : Player): boolean {
    if(this.friends == undefined) return false;
    for(let friend of this.friends) {
      if(player.id == friend.id) {
        return true;
      }
    }
    return false;
  }

  setStep(index: number) {
    this.step = index;
  }

  get f() { return this.requestForm.controls }

  get tagsFormGroups() {
    return this.requestForm.get('tags') as FormArray;
  }

  get filters() : string[] {
    var flts = [];
    for(const filter in UserSearchFilterEnum) {
      if (isNaN(Number(filter))){
        flts.push(filter.toString());
      }
    }
    return flts;
  }

  addTag(){
    let tags = this.requestForm.get('tags') as FormArray;
    tags.push(this.fb.group({
      tag: ['', Validators.required]
    }));
  }

  removeTag(value: string) {
    let tags = this.requestForm.get('tags') as FormArray;
    tags.removeAt(tags.value.findIndex((tag: { tag: string; }) => tag.tag === value))
  }

  getPlayerById(id: string): void {
    this.playerIdSelected = id;
    if(this.players != undefined) {
      for(let player of this.players) {
        if (player.id == id) {
          this.chosenPlayer = player;
        }
      }
    }
  }

  createDirectRequest() {
    this.r.player = this.currentPlayer.id;
    this.r.target = this.chosenPlayer.id;
    this.r.playerToTargetMessage = this.requestForm.value.playerToTargetMessage;
    this.r.strength = this.requestForm.value.connectionStrength;
    this.r.tags = [];
    for(let tag of this.requestForm.value.tags)
    {
      if(!this.r.tags.includes(tag.tag))
        this.r.tags.push(tag.tag);
    }
  }

  search(): void {
    this.spinner.show();
    this.pService.searchPlayers(this.requestForm.value.filter, this.requestForm.value.value).subscribe({ next: data => {
      this.players = data;
      this.players.filter(obj => obj.id !== this.currentPlayer.id);
      let index = this.players.findIndex( e => e.id == this.currentPlayer.id);
      if (index != -1) {
        this.players.splice(index, 1);
      }
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
        this.error = true;
      }
    });
  }

  sendRequest(): void {
    this.createDirectRequest();
    this.spinner.show();
    this.rService.sendDirectRequest(this.r)
    .subscribe({ next: data => {
      if(data) {
        this.success = true;
        this.successMessage = this.successMessageAccept;
        this.r = new CreatingDirectRequest;
      }
      this.spinner.hide();
      this.refreshOnTime();
    },
      error: _error => {
        this.success = false;
        this.r = new CreatingDirectRequest;
        this.spinner.hide();
      }
    });
  }

  timeLeft: number = 2;
  interval: any;

  refreshOnTime() {
    this.interval = setInterval(() => {
      if(this.timeLeft > 0) {
        this.timeLeft--;
      } else {
        this.refresh();
      }
    },1000)
  }

  getErrorMessageFilterRequired() {
    return this.requestForm.controls['filter'].hasError('required') ? 'Filter is required' : '';
  }

  getErrorMessageValueRequired() {
    return this.requestForm.controls['value'].hasError('required') ? 'Value is required' : '';
  }

  getErrorMessagePlayerToTargetMessageRequired() {
    return this.requestForm.controls['playerToTargetMessage'].hasError('required') ? 'Message is required' : '';
  }

  getErrorMessageConnectionStrengthRequired() {
    return this.requestForm.controls['connectionStrength'].hasError('required') ? 'ConnectionStrength is required' : '';
  }

  getErrorMessageConnectionStrengthInvalid() {
    return (this.requestForm.controls['connectionStrength'].hasError('min') || this.requestForm.controls['connectionStrength'].hasError('max'))
      ? 'Connection Strength should be between 1 and 100' : '';
  }

  getErrorMessageTagRequired() {
    return 'Tag name is required';
  }

  clearResults() {
    delete this.players;
  }

  clearSuccess() {
    delete this.success;
  }

  goBack(): void {
    this.location.back();
  }

  refresh(): void {
    window.location.reload();
  }

  resetForm(): void {
    this.requestForm.reset();
  }
}
