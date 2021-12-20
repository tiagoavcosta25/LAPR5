import { Component, OnInit } from '@angular/core';
import { FormBuilder, Validators } from '@angular/forms';
import { NgxSpinnerService } from 'ngx-spinner';
import { ChangeEmotionalStatus } from 'src/shared/models/player/change-emotional-status.model';
import { Player } from 'src/shared/models/player/player.model';
import { PlayerService } from '../../services/player.service';
import { Location } from '@angular/common';
import { emotionalStatusEnum } from 'src/shared/models/player/emotional-status-enum.model';
import { DobPlayer } from '../../models/dob-player.model copy';

@Component({
  selector: 'app-update-emotional-status',
  templateUrl: './update-emotional-status.component.html',
  styleUrls: ['./update-emotional-status.component.css']
})
export class UpdateEmotionalStatusComponent implements OnInit {

  error: boolean;

  success?: boolean;

  successMessage: string = "Emotional status updated sucessfully!";
  
  errorMessage: string = "There was an error updating the emotional status!";

  currentPlayer: DobPlayer;

  ces: ChangeEmotionalStatus;

  emotionForm = this.fb.group({
    emotionalStatus: ['', Validators.required]
  })

  constructor(private pService: PlayerService,
    private spinner: NgxSpinnerService,
    private location: Location,
    private fb: FormBuilder) { }

  ngOnInit(): void {
    this.ces = new ChangeEmotionalStatus;
    this.getCurrentPlayer(localStorage.getItem("currentPlayer")!);
  }

  getCurrentPlayer(email: string): void {
    this.spinner.show();
    this.pService.getOnlyPlayerByEmail(email).subscribe({ next: data => {
      this.currentPlayer = data;
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
        this.error = true;
      }
    });
  }


  save(): void {
    this.createEmotionalStatus();
    this.spinner.show();
    this.pService.updateEmotionalStatus(this.currentPlayer.email, this.ces)
    .subscribe({ next: data => {
      if(data) {
        this.success = true;
        this.currentPlayer.emotionalStatus = this.ces.emotionalStatus;
        this.ces = new ChangeEmotionalStatus;
      }
      this.spinner.hide();
    },
      error: _error => {
        this.success = false;
        this.ces = new ChangeEmotionalStatus;
        this.spinner.hide();
      }
    });
  }

  createEmotionalStatus() {
    this.ces.playerEmail = "email1@gmail.com";
    this.ces.emotionalStatus = this.emotionForm.value.emotionalStatus;
  }

  clearSuccess() {
    delete this.success;
  }

  goBack(): void {
    this.location.back();
  }

  refresh(): void {
    window.location.reload();
  }

  get f() { return this.emotionForm.controls; }

  get emotionalStatus() { return this.currentPlayer.emotionalStatus}

  get emotions() : string[] {
    var emots = [];
    for(const emotion in emotionalStatusEnum) {
      if (isNaN(Number(emotion))){
        emots.push(emotion.toString());
      }
    }
    return emots;
  }

  getErrorMessageNameRequired() {
    return this.emotionForm.controls['emotionalStatus'].hasError('required') ? 'Emotional status is required' : '';
  }

}
