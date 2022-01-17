import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { CloudData, CloudOptions } from 'angular-tag-cloud-module';
import { NgxSpinnerService } from 'ngx-spinner';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { GettingConnection } from 'src/shared/models/connection/getting-connection.model';
import { prepareDataForTagCloud } from 'src/shared/tag-cloud-data-generator';
import { Pairlist } from 'src/shared/utils/pairlist.model';

@Component({
  selector: 'app-profile-tag-cloud-conn',
  templateUrl: './profile-tag-cloud-conn.component.html',
  styleUrls: ['./profile-tag-cloud-conn.component.css']
})
export class ProfileTagCloudConnComponent implements OnInit {

  userEmail: string;

  connections: GettingConnection[];

  options: CloudOptions = {
    width: 1,
    height: 200,
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
    private cService: ConnectionService,
    private activatedRoute: ActivatedRoute) { }

  ngOnInit(): void {
    this.activatedRoute.params.subscribe(params => {
      this.userEmail = params['email'];
    })
    this.getPlayer();
  }

  getPlayer(): void {
    this.spinner.show();
    this.cService.getConnections(this.userEmail).subscribe({ next: data => {
      this.connections = data;
      let tags = new Pairlist();

      for(let conn of this.connections){
        //tags = [...tags, ...conn.tags];
        for(let tag of conn.tags) {
          tags.addOrChange(tag);
        }
      }

      if(tags.length() != 0) {
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
