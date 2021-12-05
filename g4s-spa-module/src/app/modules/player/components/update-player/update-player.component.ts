import { Component, OnInit } from '@angular/core';
import { Player } from 'src/shared/models/player/player.model';
import { PlayerService } from '../../services/player.service';
import { Validators, FormBuilder, FormControl, FormArray } from '@angular/forms';
import { NgxSpinnerService } from 'ngx-spinner';
import { emotionalStatusEnum } from 'src/shared/models/player/emotional-status-enum.model';
import { DobPlayer } from '../../models/dob-player.model copy';

@Component({
  selector: 'app-update-player',
  templateUrl: './update-player.component.html',
  styleUrls: ['./update-player.component.css']
})
export class UpdatePlayerComponent implements OnInit {

  success: any;
  successMessage: string = "Player updated sucessfully!";
  errorMessage: string = "There was an error updating a player!";

  playerForm = this.fb.group({
    name: ['', Validators.required],
    email: ['', [Validators.required, Validators.pattern(/^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$/)]],
    password: ['', [Validators.minLength(8)]],
    day: ['', [Validators.required, Validators.min(1), Validators.max(31)]],
    month: ['', [Validators.required, Validators.min(1), Validators.max(12)]],
    year: ['', [Validators.required, Validators.min(1900), Validators.max(2100)]],
    phoneNumber: ['', [Validators.required, Validators.pattern(/^9[0-9]{8}$/)]],
    emotionalStatus: ['', Validators.required],
    facebook: ['', ],
    linkedIn: ['', ],
    tags: this.fb.array([])
  });

  p: DobPlayer;

  email: string;

  constructor(
              private spinner: NgxSpinnerService,
              private service: PlayerService,
              private fb: FormBuilder) { }

  ngOnInit(): void {
    this.email = localStorage.getItem('currentPlayer')!.trim();
    this.p = new DobPlayer();
    this.getPlayerByEmail();
  }

  getPlayerByEmail(){
    this.spinner.show();

      this.service.getOnlyPlayerByEmail(this.email).subscribe({ next: data => {
        this.p = data;
        let date = new Date(this.p.dateOfBirth);
        this.playerForm.controls['name'].setValue(this.p.name);
        this.playerForm.controls['email'].setValue(this.p.email);
        this.playerForm.controls['day'].setValue(date.getDate());
        this.playerForm.controls['month'].setValue(date.getMonth());
        this.playerForm.controls['year'].setValue(date.getFullYear());
        this.playerForm.controls['phoneNumber'].setValue(this.p.phoneNumber);
        this.playerForm.controls['emotionalStatus'].setValue(this.p.emotionalStatus);
        this.playerForm.controls['facebook'].setValue(this.p.facebook);
        this.playerForm.controls['linkedIn'].setValue(this.p.linkedIn);
        for(let tag of this.p.tags)
        {
          if(!this.playerForm.value.tags.includes(tag))
            this.playerForm.value.tags.push(tag);
        }
        this.spinner.hide();
      },
        error: _error => {
          this.spinner.hide();
        }
      });
  }

  addTag(){
    let tags = this.playerForm.get('tags') as FormArray;
    tags.push(this.fb.group({
      tag: ['', Validators.required]
    }));
  }

  removeTag(value: string) {
    let tags = this.playerForm.get('tags') as FormArray;
    tags.removeAt(tags.value.findIndex((tag: { tag: string; }) => tag.tag === value))
  }

  get tagsFormGroups() {
    return this.playerForm.get('tags') as FormArray;
  }

  updatePlayer() {
    this.p.name = this.playerForm.value.name;
    this.p.email = this.playerForm.value.email;
    let date = new Date();
    date.setDate(this.playerForm.value.day);
    date.setMonth(this.playerForm.value.month);
    date.setFullYear(this.playerForm.value.year);
    this.p.dateOfBirth = this.playerForm.value.year + '-' + this.playerForm.value.month + '-' + this.playerForm.value.day + 'T00:00:00';
    this.p.phoneNumber = Number(this.playerForm.value.phoneNumber);
    this.p.emotionalStatus = this.playerForm.value.emotionalStatus;
    this.p.facebook = this.playerForm.value.facebook;
    this.p.linkedIn = this.playerForm.value.linkedIn;
    for(let tag of this.playerForm.value.tags)
    {
      if(!this.p.tags.includes(tag.tag))
        this.p.tags.push(tag.tag);
    }
  }

  getErrorMessageNameRequired() {
    return this.playerForm.controls['name'].hasError('required') ? 'Name is required' : '';
  }
  
  getErrorMessageEmailRequired() {
    return this.playerForm.controls['email'].hasError('required') ? 'Email is required' : '';
  }

  getErrorMessageEmailInvalid() {
    return this.playerForm.controls['email'].hasError('pattern') ? 'Email is not valid' : '';
  }

  getErrorMessagePasswordInvalid() {
    return this.playerForm.controls['password'].hasError('minlength') ? 'Password needs to be at least 8 characters' : '';
  }

  getErrorMessageDayRequired() {
    return this.playerForm.controls['day'].hasError('required') ? 'Day is required' : '';
  }

  getErrorMessageDayInvalid() {
    return (this.playerForm.controls['day'].hasError('min') || this.playerForm.controls['day'].hasError('max'))
      ? 'Day should be between 1 and 31' : '';
  }

  getErrorMessageMonthRequired() {
    return this.playerForm.controls['month'].hasError('required') ? 'Month is required' : '';
  }

  getErrorMessageMonthInvalid() {
    return (this.playerForm.controls['month'].hasError('min') || this.playerForm.controls['month'].hasError('max'))
      ? 'Month should be between 1 and 12' : '';
  }

  getErrorMessageYearRequired() {
    return this.playerForm.controls['year'].hasError('required') ? 'Year is required' : '';
  }

  getErrorMessageYearInvalid() {
    return (this.playerForm.controls['year'].hasError('min') || this.playerForm.controls['year'].hasError('max'))
      ? 'Year should be between 1900 and 2100' : '';
  }

  getErrorMessagePhoneNumberRequired() {
    return this.playerForm.controls['phoneNumber'].hasError('required') ? 'Phone number is required' : '';
  }

  getErrorMessagePhoneNumberInvalid() {
    return this.playerForm.controls['phoneNumber'].hasError('pattern') ? 'Phone number is not valid' : '';
  }

  getErrorMessageEmotionalStatusRequired() {
    return this.playerForm.controls['emotionalStatus'].hasError('required') ? 'Emotional status is required' : '';
  }

  getErrorMessageTagRequired() {
    return 'Created tag name is required';
  }

  save(): void {
    this.updatePlayer();
    this.spinner.show();
    this.service.updatePlayer(this.p)
    .subscribe({ next: data => {
      if(data) {
        this.success = true;
        let tags = this.playerForm.get('tags') as FormArray;
        for(let tagT of this.p.tags) {
          tags.removeAt(tags.value.findIndex((tag: { tag: string; }) => tag.tag === tagT))
        }
        this.p = new DobPlayer;
      }
      this.spinner.hide();
    },
      error: _error => {
        this.success = false;
        this.p = new DobPlayer;
        this.spinner.hide();
      }
    });
  }

  clearSucess() {
    delete this.success;
  }

  get f() { return this.playerForm.controls; }

  get emotions() : string[] {
    var emots = [];
    for(const emotion in emotionalStatusEnum) {
      if (isNaN(Number(emotion))){
        emots.push(emotion.toString());
      }
    }
    return emots;
  }

}
