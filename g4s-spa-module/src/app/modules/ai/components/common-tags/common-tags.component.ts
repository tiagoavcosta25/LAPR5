import { Component, OnInit } from '@angular/core';
import { NgxSpinnerService } from 'ngx-spinner';
import { AiService } from '../../services/ai.service';
import { Location } from '@angular/common';
import { Player } from 'src/shared/models/player/player.model';
import { PlayerService } from 'src/app/modules/player/services/player.service';


@Component({
  selector: 'app-common-tags',
  templateUrl: './common-tags.component.html',
  styleUrls: ['./common-tags.component.css']
})
export class CommonTagsComponent implements OnInit {

  step: number;

  result: [[][]];

  error: boolean;


  constructor(private aiService: AiService,
    private pService: PlayerService,
    private location : Location,
    private spinner: NgxSpinnerService) { }

  ngOnInit(): void {
    this.getUsersWithXCommonTags(1);
  }

  getUsersWithXCommonTags(x: number): void {
    this.spinner.show();
    this.aiService.getUsersWithXCommonTags(1).subscribe({ next: data => {
      this.result = data;
      this.getPlayers();
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
        this.error = true;
      }
    });
  } 

  getPlayers(): void {
    this.pService.getPlayers().subscribe({ next: data => {
      var tempPlayers = data;
    
      for(let tagUser of this.result) {
        console.log(tagUser[0]);
        console.log(tagUser[1]);
      }
      //this.players = lst;
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
        this.error = true;
      }
    });
  } 

  changeValues() {}

}
