import { Injectable } from '@angular/core';
import * as signalR from "@aspnet/signalr";

@Injectable({
  providedIn: 'root'
})
export class SignalrService {

  playerNumber: Number;

  hubConnection: signalR.HubConnection;

  startConnection = () => {
    this.hubConnection = new signalR.HubConnectionBuilder()
      .withUrl("https://localhost:5001/signalr")
      .build();
  
    this.hubConnection
      .start()
      .then(() => console.log('Connection started'))
      .catch(err => console.log('Error while starting connection ' + err));
  }

  addPlayerNumberListener = () => {
    this.hubConnection.on('playerPost', (data) => {
      this.playerNumber = data;
      console.log(data, "yo");
    })
  }

  constructor() { }
}
