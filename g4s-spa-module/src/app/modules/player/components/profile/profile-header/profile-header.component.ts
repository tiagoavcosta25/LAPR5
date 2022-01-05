import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { NgxSpinnerService } from 'ngx-spinner';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { DobPlayer } from '../../../models/dob-player.model copy';
import { PlayerService } from '../../../services/player.service';

@Component({
  selector: 'app-profile-header',
  templateUrl: './profile-header.component.html',
  styleUrls: ['./profile-header.component.css']
})
export class ProfileHeaderComponent implements OnInit {

  currentPlayer: string;

  userEmail: string;

  player: DobPlayer;

  ownProfile: boolean;

  isFriend: boolean;

  currentTab: string;

  constructor(private activatedRoute: ActivatedRoute,
    private pService: PlayerService,
    private cService: ConnectionService,
    private spinner: NgxSpinnerService,
    private router: Router) { }

  async ngOnInit(): Promise<void> {
    this.checkCurrentTab();
    this.currentPlayer = localStorage.getItem("currentPlayer")!;
    this.activatedRoute.params.subscribe(params => {
      this.userEmail = params['email'];
    })
    this.getPlayer();
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
}
