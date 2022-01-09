import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { CloudData, CloudOptions } from 'angular-tag-cloud-module';
import { NgxSpinnerService } from 'ngx-spinner';
import { DobPlayer } from '../../../models/dob-player.model copy';
import { PlayerService } from '../../../services/player.service';
import { prepareDataForTagCloud } from 'src/shared/tag-cloud-data-generator';

@Component({
  selector: 'app-profile-tag-cloud',
  templateUrl: './profile-tag-cloud.component.html',
  styleUrls: ['./profile-tag-cloud.component.css']
})
export class ProfileTagCloudComponent implements OnInit {

  userEmail: string;

  player: DobPlayer;

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
    private pService: PlayerService,
    private activatedRoute: ActivatedRoute) { }

  ngOnInit(): void {
    this.activatedRoute.params.subscribe(params => {
      this.userEmail = params['email'];
    })
    this.getPlayer();
  }

  getPlayer(): void {
    this.spinner.show();
    this.pService.getOnlyPlayerByEmail(this.userEmail).subscribe({ next: data => {
      this.player = data;
      this.data = prepareDataForTagCloud(data.tags);
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
      }
    });
  }

}
