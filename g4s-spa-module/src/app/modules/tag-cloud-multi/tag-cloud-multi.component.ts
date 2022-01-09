import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';

@Component({
  selector: 'app-tag-cloud-multi',
  templateUrl: './tag-cloud-multi.component.html',
  styleUrls: ['./tag-cloud-multi.component.css']
})
export class TagCloudMultiComponent implements OnInit {

  currentTab: string;

  constructor(private router: Router) { }

  ngOnInit(): void {
    this.checkCurrentTab();
  }

  checkCurrentTab() {
    let tab = this.router.url.substring(this.router.url.lastIndexOf('/') + 1);
    if(tab == "tag-cloud-users") {
      this.currentTab = "navUsers";
    } else if (tab == "tag-cloud-connections") {
      this.currentTab = "navConnections";
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
