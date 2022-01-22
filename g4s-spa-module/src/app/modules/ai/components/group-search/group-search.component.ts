import { Component, OnInit } from '@angular/core';
import { NgxSpinnerService } from 'ngx-spinner';
import { Player } from 'src/shared/models/player/player.model';
import { AiService } from '../../services/ai.service';

@Component({
  selector: 'app-group-search',
  templateUrl: './group-search.component.html',
  styleUrls: ['./group-search.component.css']
})
export class GroupSearchComponent implements OnInit {

  algoPlayers: Player[] | undefined;

  ntags: number = 1;

  nusers: number= 1;

  tags: string[];

  taglist: string = "";

  constructor(private aService: AiService,
    private spinner: NgxSpinnerService) { }

  ngOnInit(): void {
    this.getGroups();
  }

  getGroups(): void {
    this.spinner.show();
    this.aService.getGroups(this.ntags, this.nusers, this.taglist).subscribe({ next: data => {
      console.log(data);
      this.spinner.hide()
    },
      error: _error => {
        this.spinner.hide();
      }
    });
  }

  reset() {
    this.algoPlayers = undefined;
    this.tags = [];
    this.taglist = "";
  }

  removeTag(tag: string) {
    let index = this.tags.findIndex( t => t == tag);
    if (index != -1) {
      this.tags.splice(index, 1);
    }
  }
}
