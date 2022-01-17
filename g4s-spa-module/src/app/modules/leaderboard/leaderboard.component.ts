import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';

@Component({
  selector: 'app-leaderboard',
  templateUrl: './leaderboard.component.html',
  styleUrls: ['./leaderboard.component.css']
})
export class LeaderboardComponent implements OnInit {

  currentTab: string;

  constructor(private router: Router) { }

  ngOnInit(): void {
    this.checkCurrentTab();
  }

  checkCurrentTab() {
    let tab = this.router.url.substring(this.router.url.lastIndexOf('/') + 1);
    if(tab == "leaderboard-dimension") {
      this.currentTab = "navDimension";
    } else if (tab == "leaderboard-strength") {
      this.currentTab = "navStrength";
    }
  }

  changeActive(event: any) {
    var navs = document.getElementsByClassName("nav-link");
    for(let i = 0; i < navs.length; i++) {
      if(navs[i].id != event.target.id) {
        navs[i].classList.remove('active');
      }
    }
    event.target.classList.add('active');
  }

}
