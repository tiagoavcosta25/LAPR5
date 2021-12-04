import { Component, OnInit } from '@angular/core';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { RequestService } from '../../services/request.service';
import { FormArray, FormBuilder, Validators } from '@angular/forms';
import { NgxSpinnerService } from 'ngx-spinner';
import { Location } from '@angular/common';
import { Player } from 'src/shared/models/player/player.model';
import { GetMutualFriends } from 'src/shared/models/player/get-mutual-friends.model';
import { CreatingConnectionRequest } from '../../models/creating-connection-request.model';

@Component({
  selector: 'app-request-introduction',
  templateUrl: './request-introduction.component.html',
  styleUrls: ['./request-introduction.component.css']
})
export class RequestIntroductionComponent implements OnInit {

  error: boolean;

  success?: boolean;

  successMessage: string = "Introduction requested with sucess";
  
  errorMessage: string = "There was an error with the request!";

  stepReachable: number;
  stepMutual: number;

  reachablePlayers: Player[];

  reachablePlayerIdSelected: string;

  mutualFriendIdSelected: string;

  reachablePlayer: Player;
  mutualFriend: Player;

  mutualFriends: Player[];

  playerEmail: string;

  c: CreatingConnectionRequest;

  requestForm = this.fb.group({
    targetMessage: ['', [Validators.required, Validators.min(1), Validators.max(100)]],
    middleManMessage: ['', [Validators.required, Validators.min(1), Validators.max(255)]],
    connectionStrength: ['', [Validators.required, Validators.min(1), Validators.max(100)]],
    tags: this.fb.array([])
  })

  constructor(private cService: ConnectionService,
    private rService: RequestService,
    private spinner: NgxSpinnerService,
    private location: Location,
    private fb: FormBuilder) { }

    ngOnInit(): void {
      this.c = new CreatingConnectionRequest;
      this.reachablePlayer = new Player;
      this.mutualFriend = new Player;
      this.playerEmail = 'email1@gmail.com';
      this.getReachablePlayers();
    }

    setStepReachable(index: number) {
      this.stepReachable = index;
    }

    setStepMutual(index: number) {
      this.stepMutual = index;
    }

    get f() { return this.requestForm.controls; }

    get tagsFormGroups() {
      return this.requestForm.get('tags') as FormArray;
    }

    addTag(){
      let tags = this.requestForm.get('tags') as FormArray;
      tags.push(this.fb.group({
        tag: ['', Validators.required]
      }));
    }

    addTagWithValue(value: string){
      let tags = this.requestForm.get('tags') as FormArray;
      tags.push(this.fb.group({
        tag: [value, Validators.required]
      }));
    }

    removeTag(value: string) {
      let tags = this.requestForm.get('tags') as FormArray;
      tags.removeAt(tags.value.findIndex((tag: { tag: string; }) => tag.tag === value))
    }

    getReachablePlayerById(id: string): void {
      this.reachablePlayerIdSelected = id;
      for(let p of this.reachablePlayers) {
        if (p.id == id) {
          this.reachablePlayer = p;
        }
      }
    }

    getMutualFriendById(id: string): void {
      this.mutualFriendIdSelected = id;
      for(let p of this.mutualFriends) {
        if (p.id == id) {
          this.mutualFriend = p;
        }
      }
    }

    getReachablePlayers(): void {
      this.spinner.show();
      this.cService.getReachablePlayers(this.playerEmail).subscribe({ next: data => {
        this.reachablePlayers = data;
        this.spinner.hide();
      },
        error: _error => {
          this.spinner.hide();
          this.error = true;
        }
      });
    }

    getMutualFriends(target: Player): void {
      this.spinner.show();
      this.cService.getMutualFriends(this.playerEmail, target.email).subscribe({ next: data => {
        this.mutualFriends = data;
        this.spinner.hide();
      },
        error: _error => {
          this.spinner.hide();
          this.error = true;
        }
      });
      /*let friend = new Player();
      friend.email = "email1@gmail.com";
      friend.name = "user1";
      friend.id = "a0164888-d6af-4fd5-ba01-ffce1d1cd0a0"
      this.mutualFriends = [friend];*/
      this.spinner.hide();
    }

    createRequest() {
      console.log(this.reachablePlayerIdSelected);
      this.c.player = this.reachablePlayerIdSelected;
      this.c.playerToTargetMessage = this.requestForm.value.targetMessage;
      this.c.playerToMiddleManMessage = this.requestForm.value.middleManMessage;
      this.c.middleManToTargetMessage = "";
      this.c.middleMan = this.mutualFriendIdSelected;
      this.c.currentStatus = "introduction_pending";
      this.c.tags = [];
      for(let tag of this.requestForm.value.tags)
      {
        if(!this.c.tags.includes(tag.tag))
          this.c.tags.push(tag.tag);
      }
    }

    registerRequest(): void {
      this.createRequest();
      console.log(this.c);
      this.spinner.show();
      this.rService.registerIntroductionRequest(this.c)
      .subscribe({ next: data => {
        if(data) {
          this.success = true;
          this.successMessage = this.successMessage;
          this.c = new CreatingConnectionRequest;
        }
        this.spinner.hide();
        this.refreshOnTime();
      },
        error: _error => {
          this.success = false;
          this.c = new CreatingConnectionRequest;
          this.spinner.hide();
        }
      });
    }

    timeLeft: number = 2;
    interval: any;

    refreshOnTime() {
      this.interval = setInterval(() => {
        if(this.timeLeft > 0) {
          this.timeLeft--;
        } else {
          this.refresh();
        }
      },1000)
    }

    getErrorMessageTargetMessageRequired() {
      return this.requestForm.controls['targetMessage'].hasError('required') ? 'Target Message is required' : '';
    }

    getErrorMessageTargetMessageInvalid() {
      return (this.requestForm.controls['targetMessage'].hasError('min') || this.requestForm.controls['targetMessage'].hasError('max'))
        ? 'Target Message should be between 1 and 255 characters' : '';
    }

    getErrorMessageMiddleManMessageRequired() {
      return this.requestForm.controls['targetMessage'].hasError('required') ? 'Target Message is required' : '';
    }

    getErrorMessageMiddleManMessageInvalid() {
      return (this.requestForm.controls['middleMan'].hasError('min') || this.requestForm.controls['middleMan'].hasError('max'))
        ? 'Middle Man Message should be between 1 and 255 characters' : '';
    }

    getErrorMessageConnectionStrengthRequired() {
      return this.requestForm.controls['connectionStrength'].hasError('required') ? 'ConnectionStrength is required' : '';
    }

    getErrorMessageConnectionStrengthInvalid() {
      return (this.requestForm.controls['connectionStrength'].hasError('min') || this.requestForm.controls['connectionStrength'].hasError('max'))
        ? 'Connection Strength should be between 1 and 100' : '';
    }

    getErrorMessageTagRequired() {
      return 'Tag name is required';
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

}
