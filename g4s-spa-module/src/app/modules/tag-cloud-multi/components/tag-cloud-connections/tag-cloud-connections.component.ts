import { Component, OnInit } from '@angular/core';
import { CloudData, CloudOptions } from 'angular-tag-cloud-module';
import { NgxSpinnerService } from 'ngx-spinner';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { prepareDataForTagCloudMulti } from 'src/shared/tag-cloud-multi-data-generator';
import { Pairlist } from 'src/shared/utils/pairlist.model';

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

  tags: Pairlist = new Pairlist();

  constructor(private spinner: NgxSpinnerService,
    private cService: ConnectionService) { }

  ngOnInit(): void {
    this.getConnections();
  }

  getConnections(): void {
    this.spinner.show();
    this.cService.getAllConnections().subscribe({ next: data => {
      for(let con of data) {
        for(let tag of con.tags) {
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
