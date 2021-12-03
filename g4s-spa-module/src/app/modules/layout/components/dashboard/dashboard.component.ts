import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'app-dashboard',
  templateUrl: './dashboard.component.html',
  styleUrls: ['./dashboard.component.css']
})
export class DashboardComponent implements OnInit {

  showGetPlayers: boolean = true;
  showRegisterPlayer: boolean = false;
  showLogin: boolean = false;

  constructor() { }

  ngOnInit(): void {
    this.showGetPlayers = true;
    this.showRegisterPlayer = false;
    this.showLogin = false;
  }

  openGetPlayers(){
    this.showGetPlayers = true;
    this.showLogin = false;
    this.showRegisterPlayer = false;
  }

  openRegisterPlayer(){
    this.showGetPlayers = false;
    this.showLogin = false;
    this.showRegisterPlayer = true;
  }

  openLogin(){
    this.showGetPlayers = false;
    this.showRegisterPlayer = false;
    this.showLogin = true;
  }

}
