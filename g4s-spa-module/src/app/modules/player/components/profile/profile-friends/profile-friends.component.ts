import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { NgxSpinnerService } from 'ngx-spinner';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { Player } from 'src/shared/models/player/player.model';
import { GettingConnection } from 'src/shared/models/connection/getting-connection.model';
import { lastValueFrom } from 'rxjs';

@Component({
  selector: 'app-profile-friends',
  templateUrl: './profile-friends.component.html',
  styleUrls: ['./profile-friends.component.css']
})
export class ProfileFriendsComponent implements OnInit {

  currentPlayerEmail: string;

  currentPlayerFriends: Player[];

  userEmail: string;

  userFriends: Player[];

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
      console.log(this.userFriends);
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
