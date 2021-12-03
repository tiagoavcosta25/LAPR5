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
  showRequestIntroduction: boolean = false;

  constructor() { }

  ngOnInit(): void {
    this.showGetPlayers = true;
    this.showRequestIntroduction = false;
    this.showRegisterPlayer = false;
    this.showLogin = false;
  }

  openGetPlayers(){
    this.showGetPlayers = true;
    this.showRequestIntroduction = false;
    this.showLogin = false;
    this.showRegisterPlayer = false;
  }

  openRegisterPlayer(){
    this.showGetPlayers = false;
    this.showRequestIntroduction = false;
    this.showLogin = false;
    this.showRegisterPlayer = true;
  }

  openLogin(){
    this.showGetPlayers = false;
    this.showRequestIntroduction = false;
    this.showRegisterPlayer = false;
    this.showLogin = true;
  }

  openRequestIntroduction(){
    this.showGetPlayers = false;
    this.showRequestIntroduction = true;
    this.showRegisterPlayer = false;
    this.showLogin = false;
  }

}
