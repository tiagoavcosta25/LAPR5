import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { NgxSpinnerService } from 'ngx-spinner';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { RequestService } from 'src/app/modules/request/services/request.service';
import { CreatingDirectRequest } from 'src/shared/models/requests/creating-direct-request.model';
import { DirectRequest } from 'src/shared/models/requests/direct-request.model';
import { DobPlayer } from '../../../models/dob-player.model copy';
import { PlayerService } from '../../../services/player.service';

declare var $:any;

@Component({
  selector: 'app-profile-header',
  templateUrl: './profile-header.component.html',
  styleUrls: ['./profile-header.component.css']
})
export class ProfileHeaderComponent implements OnInit{

  requestSent: boolean;

  requestPending: boolean;

  currentPlayer: string;

  currentPlayerObject: DobPlayer;

  userEmail: string;

  player: DobPlayer;

  ownProfile: boolean;

  isFriend: boolean;

  currentTab: string;

  sendFriendRequest: boolean = false;

  tags: string[] = [];

  r: CreatingDirectRequest = new CreatingDirectRequest;

  constructor(private activatedRoute: ActivatedRoute,
    private pService: PlayerService,
    private cService: ConnectionService,
    private rService: RequestService,
    private spinner: NgxSpinnerService,
    private router: Router) { }

  async ngOnInit(): Promise<void> {
    this.spinner.show();
    $('[data-bs-toggle="tooltip"]').tooltip();
    this.checkCurrentTab();
    this.currentPlayer = localStorage.getItem("currentPlayer")!;
    this.getCurrentPlayer();
    this.activatedRoute.params.subscribe(params => {
      this.userEmail = params['email'];
      if(this.userEmail != this.currentPlayer) {
        this.CheckIfRequestsPendingBetweenUsers();
        this.CheckIfRequestSent();
      } else {
        this.requestPending = false;
        this.requestSent = false;
      }
    })
    this.getPlayer();
    this.spinner.hide();
  }

  getPlayer(): void {
    this.spinner.show();
    this.pService.getOnlyPlayerByEmail(this.userEmail).subscribe({ next: data => {
      if(this.currentPlayer != data.email) {
        this.ownProfile = false;
      } else {
        this.ownProfile = true;
      }
      this.player = data;
      if(!this.ownProfile) {
        this.getFriends();
      }
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
      }
    });
  }

  getCurrentPlayer(): void {
    this.spinner.show();
    this.pService.getOnlyPlayerByEmail(this.currentPlayer).subscribe({ next: data => {
      this.currentPlayerObject = data;
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
      }
    });
  }

  checkCurrentTab() {
    let tab = this.router.url.substring(this.router.url.lastIndexOf('/') + 1);
    if(tab == "profile-timeline") {
      this.currentTab = "navPosts";
    } else if (tab == "profile-about") {
      this.currentTab = "navAbout";
    } else if (tab == "profile-tag-cloud") {
      this.currentTab = "navTagCloud";
    } else if (tab == "profile-friends") {
      this.currentTab = "navFriends";
    } else if (tab == "profile-common-friends") {
      this.currentTab = "navCommonFriends";
    } else {
      this.currentTab = "navPosts";
    }
  }

  changeActive(event: any) {
    var navs = document.getElementsByClassName("nav-link");
    for(let i = 0; i < navs.length; i++) {
      if(navs[i].id != event.target.id) {
        navs[i].classList.remove('active');
      }
    }
    event.target.classList.add('active');
  }

  getFriends(): void {
    var connections = [];
    this.cService.getConnections(this.currentPlayer).subscribe({ next: data => {
      connections = data;
      for(let con of connections) {
        if(con.friend.email == this.userEmail) {
          this.isFriend = true;
          break;
        }
      }
      if(this.isFriend == undefined) {
        this.isFriend = false;
      }
    },
      error: _error => {
      }
    });
  }

  CheckIfRequestsPendingBetweenUsers(): void {
    this.spinner.show();
    this.rService.CheckIfRequestsPendingBetweenUsers(this.userEmail, this.currentPlayer).subscribe({ next: data => {
      this.requestPending = data;
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
      }
    });
  }

  CheckIfRequestSent(): void {
    this.spinner.show();
    this.rService.CheckIfRequestsPendingBetweenUsers(this.currentPlayer, this.userEmail).subscribe({ next: data => {
      this.requestSent = data;
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
      }
    });
  }

  getStatus(): string {
    switch(this.player.emotionalStatus) {
      case "joyful": {
        return "😄";
      }
      case "distressed": {
        return "😖";
      }
      case "hopeful": {
        return "😇";
      }
      case "fearful": {
        return "😧";
      }
      case "relieve": {
        return "😪";
      }
      case "disappointed": {
        return "😔";
      }
      case "proud": {
        return "🥰";
      }
      case "remorseful": {
        return "🤥";
      }
      case "grateful": {
        return "🤗";
      }
      case "angry": {
        return "😡";
      }
      default: {
        return "👾";
      }
    }
  }

  addTag() {
    let el = document.getElementById('tagvalue');
    let val = "";
    if(el != undefined) {
      val = ((<HTMLInputElement>el).value);
      (<HTMLInputElement>el).value = "";
    }
    if(this.tags.indexOf(val) < 0) {
      this.tags.push(val);
    }
  }

  removeTag(tag: string) {
    let index = this.tags.findIndex( t => t == tag);
    if (index != -1) {
      this.tags.splice(index, 1);
    }
  }

  createDirectRequest() {
    let el = document.getElementById("messageToPlayer");
    let messageToPlayer = "";
    let strength: number = 0;
    if(el != undefined) {
      messageToPlayer = ((<HTMLInputElement>el).value);
      (<HTMLInputElement>el).value = "";
    }
    let el2 = document.getElementById("connectionStrength");
    if(el2 != undefined) {
      strength =+ ((<HTMLInputElement>el2).value);
      (<HTMLInputElement>el2).value = "";
    }

    this.r.player = this.currentPlayerObject.id;
    this.r.target = this.player.id;
    this.r.playerToTargetMessage = messageToPlayer;
    this.r.strength = strength;
    this.r.tags = this.tags;
  }

  sendRequest(): void {
    this.createDirectRequest();
    this.spinner.show();
    this.rService.sendDirectRequest(this.r)
    .subscribe({ next: data => {
      if(data) {
        this.r = new CreatingDirectRequest;
      }
      this.sendFriendRequest = false;
      this.spinner.hide();
      this.ngOnInit();
    },
      error: _error => {
        this.sendFriendRequest = false;
        this.r = new CreatingDirectRequest;
        this.spinner.hide();
      }
    });
  }
}
