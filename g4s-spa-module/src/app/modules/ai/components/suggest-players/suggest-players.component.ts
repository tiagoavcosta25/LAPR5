import { Component, OnInit } from '@angular/core';
import { FormBuilder, Validators } from '@angular/forms';
import { NgxSpinnerService } from 'ngx-spinner';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { Player } from 'src/shared/models/player/player.model';
import { AiService } from '../../services/ai.service';

@Component({
  selector: 'app-suggest-players',
  templateUrl: './suggest-players.component.html',
  styleUrls: ['./suggest-players.component.css']
})
export class SuggestPlayersComponent implements OnInit {

  suggestPlayersForm = this.fb.group({
    scope: ['', [Validators.required, Validators.min(1), Validators.max(10)]]
  });

  email: string;
  success: any;
  player: Player;
  suggestedIdList: string[];
  suggestedList: Player[];
  showForm: boolean = true;
  showList: boolean = false;

  constructor(
    private spinner: NgxSpinnerService,
    private aService: AiService,
    private pService: PlayerService,
    private fb: FormBuilder) { 

      this.showForm = true;
      this.showList = false
    }

  ngOnInit(): void {
    this.suggestedIdList = [];
    this.suggestedList = [];
    this.player = new Player;
    this.email = "john@email.com";
  }

  getSuggestedPlayers(){
    this.spinner.show();
    this.pService.getOnlyPlayerByEmail(this.email).subscribe({ next: data => {
      this.player = data;
      console.log(this.player);
      this.aService.getSuggestedPlayers(this.email, this.suggestPlayersForm.value.scope).subscribe({ next: data => {
      this.suggestedIdList = data;
      let temp = new Player;
      for(let id of this.suggestedIdList){
        this.pService.getOnlyPlayerByEmail(this.email).subscribe({ next: data => {
          temp = data;
          this.suggestedList.push(temp);
          this.showForm = false;
          this.showList = true;
          this.spinner.hide();
        },
          error: _error => {
            this.spinner.hide();
          }
        });
      }
      
    },
      error: _error => {
        this.spinner.hide();
      }
    });
    },
      error: _error => {
        this.spinner.hide();
      }
    });
  }

  getErrorMessageScopeRequired() {
    return this.suggestPlayersForm.controls['scope'].hasError('required') ? 'Scope is required' : '';
  }

  getErrorMessageScopeInvalid() {
    return (this.suggestPlayersForm.controls['scope'].hasError('min') || this.suggestPlayersForm.controls['scope'].hasError('max'))
      ? 'Scope should be between 1 and 10' : '';
  }

  clearSucess() {
    delete this.success;
  }

  get f() { return this.suggestPlayersForm.controls; }

}
