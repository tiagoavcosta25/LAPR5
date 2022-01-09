import { Component, OnInit } from '@angular/core';
import { CloudData, CloudOptions } from 'angular-tag-cloud-module';
import { NgxSpinnerService } from 'ngx-spinner';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { GettingConnection } from 'src/shared/models/connection/getting-connection.model';
import { prepareDataForTagCloud } from 'src/shared/tag-cloud-data-generator';

@Component({
  selector: 'app-tag-cloud-connections',
  templateUrl: './tag-cloud-connections.component.html',
  styleUrls: ['./tag-cloud-connections.component.css']
})
export class TagCloudConnectionsComponent implements OnInit {

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

  constructor(private spinner: NgxSpinnerService,
    private cService: ConnectionService) { }

  ngOnInit(): void {
    this.getConnections();
  }

  getConnections(): void {
    this.spinner.show();
    this.cService.getAllConnections().subscribe({ next: data => {
      let tags: string[] = [];
      for(let con of data) {
        for(let tag of con.tags) {
          if(!tags.some(x => x == tag)) {
            tags.push(tag);
          }
        }
      }
      if(tags.length != 0) {
        this.data = prepareDataForTagCloud(tags);
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
