import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { NgxSpinnerService } from 'ngx-spinner';
import { lastValueFrom } from 'rxjs';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { GettingConnection } from 'src/shared/models/connection/getting-connection.model';
import { Player } from 'src/shared/models/player/player.model';

@Component({
  selector: 'app-profile-common-friends',
  templateUrl: './profile-common-friends.component.html',
  styleUrls: ['./profile-common-friends.component.css']
})
export class ProfileCommonFriendsComponent implements OnInit {

  currentPlayerEmail: string;

  currentPlayerFriends: Player[];

  userEmail: string;

  userFriends: Player[];

  friendsInCommon: Player[];

  constructor(private activatedRoute: ActivatedRoute,
    private cService: ConnectionService,
    private spinner: NgxSpinnerService) { }

  async ngOnInit(): Promise<void> {
    this.spinner.show();
    this.currentPlayerEmail = localStorage.getItem("currentPlayer")!;
    this.currentPlayerFriends = await this.getFriends(this.currentPlayerEmail);
    this.activatedRoute.params.subscribe(async params => {
      this.userEmail = params['email'];
      this.userFriends = await this.getFriends(this.userEmail);
      this.getFriendsInCommon();
    })
    this.spinner.hide();
  }

  async getConnections(userEmail: string): Promise<GettingConnection[]> {
    let cons = [];
    const connections = this.cService.getConnections(userEmail);
    cons = await lastValueFrom(connections);
    return cons;
  }

  async getFriends(userEmail: string): Promise<Player[]> {
    let cons = await this.getConnections(userEmail);
    let friends = [];
    for(let con of cons) {
      friends.push(con.friend);
    }
    return friends;
  }

  getFriendsInCommon() {
    let friends = [];
    for(let cpf of this.currentPlayerFriends) {
      for(let uf of this.userFriends) {
        if(cpf.email == uf.email) {
          friends.push(uf);
        }
      }
    }
    this.friendsInCommon = friends;
  }

  checkIfFriend(friend: Player): boolean {
    for(let f of this.currentPlayerFriends) {
      if(f.email == friend.email) {
        return true;
      }
    }
    return false;
  }

  checkIfOwner(friend: Player): boolean {
    return friend.email == this.currentPlayerEmail;
  }

}
