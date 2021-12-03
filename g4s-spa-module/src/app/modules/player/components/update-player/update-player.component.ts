import { Component, OnInit } from '@angular/core';
import { Player } from 'src/shared/models/player/player.model';
import { PlayerService } from '../../services/player.service';
import { Validators, FormBuilder, FormControl, FormArray } from '@angular/forms';
import { NgxSpinnerService } from 'ngx-spinner';
import { emotionalStatusEnum } from 'src/shared/models/player/emotional-status-enum.model';

@Component({
  selector: 'app-update-player',
  templateUrl: './update-player.component.html',
  styleUrls: ['./update-player.component.css']
})
export class UpdatePlayerComponent implements OnInit {

  success: any;
  successMessage: string = "Player created sucessfully!";
  errorMessage: string = "There was an error creating a player!";

  playerForm = this.fb.group({
    name: ['', Validators.required],
    email: ['', [Validators.required, Validators.pattern(/^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$/)]],
    password: ['', [Validators.required, Validators.minLength(8)]],
    day: ['', [Validators.required, Validators.min(1), Validators.max(31)]],
    month: ['', [Validators.required, Validators.min(1), Validators.max(12)]],
    year: ['', [Validators.required, Validators.min(1900), Validators.max(2100)]],
    phoneNumber: ['', [Validators.required, Validators.pattern(/^9[0-9]{8}$/)]],
    emotionalStatus: ['', Validators.required],
    facebook: ['', ],
    linkedIn: ['', ],
    tags: this.fb.array([])
  });

  p: Player;

  id: string;

  constructor(
              private spinner: NgxSpinnerService,
              private service: PlayerService,
              private fb: FormBuilder) { }

  ngOnInit(): void {
    this.id = "115ac456-8a98-4e60-9966-02c38dd0ee64";
    this.p = new Player();
    this.getPlayerById(this.id);
  }

  getPlayerById(id: string){
    this.spinner.show();
    
    console.log(id);

      this.service.getPlayerById(id).subscribe({ next: data => {
        this.p = data;
      },
        error: _error => {
        }
      });
      this.playerForm.controls['name'].setValue(this.p.name);
      this.playerForm.controls['email'].setValue(this.p.email);
      this.playerForm.controls['day'].setValue(this.p.day);
      this.playerForm.controls['month'].setValue(this.p.month);
      this.playerForm.controls['year'].setValue(this.p.year);
      this.playerForm.controls['phoneNumber'].setValue(this.p.phoneNumber);
      this.playerForm.controls['emotionalStatus'].setValue(this.p.emotionalStatus);
      this.playerForm.controls['facebook'].setValue(this.p.facebook);
      this.playerForm.controls['linkedIn'].setValue(this.p.linkedIn);
      for(let tag of this.p.tags)
    {
      if(!this.playerForm.value.tags.includes(tag))
        this.playerForm.value.tags.push(tag);
    }
    console.log(this.p);
    this.spinner.hide();
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
    this.p.day = Number(this.playerForm.value.day);
    this.p.month = Number(this.playerForm.value.month);
    this.p.year = Number(this.playerForm.value.year);
    this.p.phoneNumber = Number(this.playerForm.value.phoneNumber);
    this.p.emotionalStatus = this.playerForm.value.emotionalStatus;
    this.p.facebook = this.playerForm.value.facebook;
    this.p.linkedIn = this.playerForm.value.linkedIn;
    for(let tag of this.playerForm.value.tags)
    {
      if(!this.p.tags.includes(tag.tag))
        this.p.tags.push(tag.tag);
    }
    console.log(this.p.name);
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

  getErrorMessagePasswordRequired() {
    return this.playerForm.controls['password'].hasError('required') ? 'Password is required' : '';
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
        this.playerForm.reset();
        this.p = new Player;
      }
      this.spinner.hide();
    },
      error: _error => {
        this.success = false;
        this.p = new Player;
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
