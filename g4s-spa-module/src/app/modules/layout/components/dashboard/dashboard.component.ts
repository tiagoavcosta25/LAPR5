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
  showUpdatePlayer: boolean = false;
  showAcceptRequest: boolean = false;
  showApproveRequest: boolean = false;
  showGetNetwork: boolean = false;
  showSearchPlayer: boolean = false;

  constructor() { }

  ngOnInit(): void {
    this.showGetPlayers = true;
    this.showRequestIntroduction = false;
    this.showRegisterPlayer = false;
    this.showLogin = false;
    this.showUpdatePlayer = false;
    this.showAcceptRequest = false;
    this.showApproveRequest = false;
    this.showGetNetwork = false;
    this.showSearchPlayer = false;
  }

  openGetPlayers(){
    this.showGetPlayers = true;
    this.showRequestIntroduction = false;
    this.showLogin = false;
    this.showRegisterPlayer = false;
    this.showUpdatePlayer = false;
    this.showAcceptRequest = false;
    this.showApproveRequest = false;
    this.showGetNetwork = false;
    this.showSearchPlayer = false;
  }

  openRegisterPlayer(){
    this.showGetPlayers = false;
    this.showRequestIntroduction = false;
    this.showLogin = false;
    this.showRegisterPlayer = true;
    this.showUpdatePlayer = false;
    this.showAcceptRequest = false;
    this.showApproveRequest = false;
    this.showGetNetwork = false;
    this.showSearchPlayer = false;
  }

  openLogin(){
    this.showGetPlayers = false;
    this.showRequestIntroduction = false;
    this.showRegisterPlayer = false;
    this.showLogin = true;
    this.showUpdatePlayer = false;
    this.showAcceptRequest = false;
    this.showApproveRequest = false;
    this.showGetNetwork = false;
    this.showSearchPlayer = false;
  }

  openRequestIntroduction(){
    this.showGetPlayers = false;
    this.showRequestIntroduction = true;
    this.showRegisterPlayer = false;
    this.showLogin = false;
    this.showUpdatePlayer = false;
    this.showAcceptRequest = false;
    this.showApproveRequest = false;
    this.showGetNetwork = false;
    this.showSearchPlayer = false;
  }

  openUpdatePlayer(){
    this.showGetPlayers = false;
    this.showRequestIntroduction = false;
    this.showRegisterPlayer = false;
    this.showLogin = false;
    this.showUpdatePlayer = true;
    this.showAcceptRequest = false;
    this.showApproveRequest = false;
    this.showGetNetwork = false;
    this.showSearchPlayer = false;
  }

  openAcceptRequest(){
    this.showGetPlayers = false;
    this.showRequestIntroduction = false;
    this.showRegisterPlayer = false;
    this.showLogin = false;
    this.showUpdatePlayer = false;
    this.showAcceptRequest = true;
    this.showApproveRequest = false;
    this.showGetNetwork = false;
    this.showSearchPlayer = false;
  }

  openApproveRequest(){
    this.showGetPlayers = false;
    this.showRequestIntroduction = false;
    this.showRegisterPlayer = false;
    this.showLogin = false;
    this.showUpdatePlayer = false;
    this.showAcceptRequest = false;
    this.showApproveRequest = true;
    this.showGetNetwork = false;
    this.showSearchPlayer = false;
  }

  openGetNetwork(){
    this.showGetPlayers = false;
    this.showRequestIntroduction = false;
    this.showRegisterPlayer = false;
    this.showLogin = false;
    this.showUpdatePlayer = false;
    this.showAcceptRequest = false;
    this.showApproveRequest = false;
    this.showGetNetwork = true;
    this.showSearchPlayer = false;
  }

  openSearchPlayer(){
    this.showGetPlayers = false;
    this.showRequestIntroduction = false;
    this.showRegisterPlayer = false;
    this.showLogin = false;
    this.showUpdatePlayer = false;
    this.showAcceptRequest = false;
    this.showApproveRequest = false;
    this.showGetNetwork = false;
    this.showSearchPlayer = true;
  }

}
