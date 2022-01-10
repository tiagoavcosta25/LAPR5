import { Injectable } from '@angular/core';
import * as signalR from "@aspnet/signalr";
import { environment } from 'src/environments/environment';

@Injectable({
  providedIn: 'root'
})
export class SignalrService {

  playerNumber: Number;

  hubConnection: signalR.HubConnection;

  startConnection = () => {
    this.hubConnection = new signalR.HubConnectionBuilder()
      .withUrl(environment.apiUrl + "/signalr")
      .build();

    this.hubConnection
      .start()
      .catch(err => console.log('Error while starting connection ' + err));
  }

  addPlayerNumberListener = () => {
    this.hubConnection.on('playerPost', (data) => {
      this.playerNumber = data;
    })
  }

  constructor() { }
}
