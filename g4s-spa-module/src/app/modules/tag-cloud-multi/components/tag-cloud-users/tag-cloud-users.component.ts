import { Component, OnInit } from '@angular/core';
import { CloudData, CloudOptions } from 'angular-tag-cloud-module';
import { NgxSpinnerService } from 'ngx-spinner';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { prepareDataForTagCloudMulti } from 'src/shared/tag-cloud-multi-data-generator';
import { Pairlist } from 'src/shared/utils/pairlist.model';

@Component({
  selector: 'app-tag-cloud-users',
  templateUrl: './tag-cloud-users.component.html',
  styleUrls: ['./tag-cloud-users.component.css']
})
export class TagCloudUsersComponent implements OnInit {

  userEmail: string;

  options: CloudOptions = {
    width: 1,
    height: 500,
    overflow: false,
    zoomOnHover: {
      scale: 2,
      transitionTime: 0.5,
      delay: 0.3
    },
    realignOnResize: true
  };

  data: CloudData[];

  tags: Pairlist = new Pairlist();

  constructor(private spinner: NgxSpinnerService,
    private cService: PlayerService) { }

  ngOnInit(): void {
    this.getConnections();
  }

  getConnections(): void {
    this.spinner.show();
    this.cService.getPlayers().subscribe({ next: data => {
      for(let player of data) {
        for(let tag of player.tags) {
          this.tags.addOrChange(tag);
        }
      }
      if(this.tags.length() != 0) {
        this.data = prepareDataForTagCloudMulti(this.tags);
      } else {
        this.data = [];
      }
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
      }
    });
  }

}
