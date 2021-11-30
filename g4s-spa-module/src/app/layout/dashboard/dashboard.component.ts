import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'app-dashboard',
  templateUrl: './dashboard.component.html',
  styleUrls: ['./dashboard.component.css']
})
export class DashboardComponent implements OnInit {

  showRegisterPlayer: boolean = true;
  showLogin: boolean = false;

  constructor() { }

  ngOnInit(): void {
    this.showRegisterPlayer = true;
    this.showLogin = false;
  }

  openRegisterPlayer(){
    this.showLogin = false;
    this.showRegisterPlayer = true;
  }

  openLogin(){
    this.showRegisterPlayer = false;
    this.showLogin = true;
  }

}
