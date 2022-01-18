import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { NgxSpinnerService } from 'ngx-spinner';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
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

  constructor(private activatedRoute: ActivatedRoute,
    private pService: PlayerService,
    private cService: ConnectionService,
    private spinner: NgxSpinnerService) { }

  ngOnInit(): void {
    this.activatedRoute.params.subscribe(params => {
      this.userEmail = params['email'];
      this.getPlayer();
    })
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

      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
      }
    });
  }

  /*getNetwork() {
    this.spinner.show();
    this.cService.getNetwork(this.email, this.getNetworkForm.value).subscribe({ next: async data => {
      this.id = await this.getCurrentPlayerId();
      for(let con of data) {
        let netCon = new NetworkConnection(con.id, con.player, con.friend, con.connectionStrength);
        let tempPlayer = await this.getPlayer(netCon.player.id);
        netCon.player.setEmailAndName(tempPlayer.email, tempPlayer.name);
        tempPlayer = await this.getPlayer(netCon.friend.id);
        netCon.friend.setEmailAndName(tempPlayer.email, tempPlayer.name);
        this.connections.push(netCon);
      }
      this.getScopes();
      this.initializeGraph();
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
      }
    });
}*/

}
