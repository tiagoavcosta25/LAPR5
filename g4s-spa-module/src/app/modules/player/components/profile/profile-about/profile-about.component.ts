import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { NgxSpinnerService } from 'ngx-spinner';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { FeedService } from 'src/app/modules/feed/services/feed.service';
import { DobPlayer } from '../../../models/dob-player.model copy';
import { PlayerService } from '../../../services/player.service';

@Component({
  selector: 'app-profile-about',
  templateUrl: './profile-about.component.html',
  styleUrls: ['./profile-about.component.css']
})
export class ProfileAboutComponent implements OnInit {

  userEmail: string;

  player: DobPlayer;

  hasLinkedin: boolean;

  hasFacebook: boolean;

  networkStrength: number;

  currentPlayerEmail: string;

  currentPlayer: DobPlayer;

  dcalcValue: number;

  networkSize: number;

  constructor(private activatedRoute: ActivatedRoute,
    private pService: PlayerService,
    private cService: ConnectionService,
    private fService: FeedService,
    private spinner: NgxSpinnerService) { }

  ngOnInit(): void {
    this.currentPlayerEmail = localStorage.getItem("currentPlayer")!;
    this.activatedRoute.params.subscribe(params => {
      this.userEmail = params['email'];
      this.getCurrentPlayer();
    })
  }

  getNetworkSize(): void {
    this.spinner.show();
    this.cService.getNetwork(this.currentPlayerEmail, 3).subscribe({ next: data => {
      this.networkSize = data.length;
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
      }
    });
  }

  getDCalc(): void {
    this.spinner.show();
    this.fService.dCalc(this.currentPlayer.id, this.player.id).subscribe({ next: data => {
      this.dcalcValue = data.dCalc;
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
      }
    });
  }

  getPlayer(): void {
    this.spinner.show();
    this.pService.getOnlyPlayerByEmail(this.userEmail).subscribe({ next: data => {
      this.player = data;
      let birthday = this.player.dateOfBirth.split('T')[0];
      this.player.dateOfBirth = birthday;
      if(this.player.facebook == null || this.player.facebook == "" || this.player.facebook == undefined) {
        this.hasFacebook = false;
      } else {
        this.hasFacebook = true;
      }

      if(this.player.linkedIn == null || this.player.linkedIn == "" || this.player.linkedIn == undefined) {
        this.hasLinkedin = false;
      } else {
        this.hasLinkedin = true;
      }
      this.getNetworkFirstLevel();
      this.getDCalc();
      this.getNetworkSize();
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
      }
    });
  }

  getCurrentPlayer(): void {
    this.spinner.show();
    this.pService.getOnlyPlayerByEmail(this.currentPlayerEmail).subscribe({ next: data => {
      this.currentPlayer = data;
      this.getPlayer();
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
      }
    });
  }

  getNetworkFirstLevel() {
    this.spinner.show();
    this.cService.getNetwork(this.userEmail, 1).subscribe({ next: async data => {
      let strength = 0;
      for(let c of data) {
        strength += c.connectionStrength;
      }
      this.networkStrength = strength;
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
      }
    });
}

}
