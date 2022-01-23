import { Location } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { NgxSpinnerService } from 'ngx-spinner';
import { DobPlayer } from 'src/app/modules/player/models/dob-player.model copy';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { Player } from 'src/shared/models/player/player.model';
import { AiService } from '../../services/ai.service';

@Component({
  selector: 'app-group-search',
  templateUrl: './group-search.component.html',
  styleUrls: ['./group-search.component.css']
})
export class GroupSearchComponent implements OnInit {

  algoTags: string[] | undefined;

  algoPlayers: Player[] | undefined;

  players: Player[];

  ntags: number = 1;

  nusers: number= 1;

  tags: string[] = [];

  taglist: string = "";

  currentUserEmail: string;

  currentUser: DobPlayer;

  step: number;

  constructor(private aService: AiService,
    private pService: PlayerService,
    private spinner: NgxSpinnerService,
    private location: Location) { }

  ngOnInit(): void {
    this.getPlayers();
    this.currentUserEmail = localStorage.getItem("currentPlayer")!;
    this.getCurrentUser();
  }

  getGroups(): void {
    this.spinner.show();
    if(this.tags.length == 0) {
      this.taglist = "_";
    } else {
      for(let i = 0; i < this.tags.length; i++) {
        this.taglist += this.tags[i];
        if(i != this.tags.length - 1) {
          this.taglist += "/";
        }
      }
    }
    this.aService.getGroups(this.currentUser.id, this.ntags, this.nusers, this.taglist).subscribe({ next: data => {
      this.algoTags = data[0];
      let algoT = [];
      for(let pId of data[1]) {
        for(let p of this.players) {
          if(pId == p.id) {
            algoT.push(p);
            break;
          }
        }
      }
      this.algoPlayers = algoT; 
      this.spinner.hide()
    },
      error: _error => {
        this.spinner.hide();
      }
    });
  }

  getCurrentUser(): void {
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

  addTag(tag: string) {
    if(this.tags.length == this.ntags) { return; }
    if(!this.tags.some(x => x == tag)) {
      this.tags.push(tag);
    }
  }

  removeTag(tag: string) {
    let index = this.tags.findIndex( t => t == tag);
    if (index != -1) {
      this.tags.splice(index, 1);
    }
  }

  setStep(index: number) {
    this.step = index;
  }

  reset() {
    this.algoPlayers = undefined;
    this.algoTags = undefined;
    this.tags = [];
    this.taglist = "";
  }
}
