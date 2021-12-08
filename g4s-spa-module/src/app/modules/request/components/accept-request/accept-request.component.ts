import { Component, OnInit } from '@angular/core';
import { FormArray, FormBuilder, Validators } from '@angular/forms';
import { NgxSpinnerService } from 'ngx-spinner';
import { AcceptRequest } from 'src/shared/models/requests/accept-request.model';
import { TargetPendingRequest } from 'src/shared/models/requests/target-pending-request.model';
import { RequestService } from '../../services/request.service';
import { Location } from '@angular/common';
import { TargetIntroductionPendingRequest } from 'src/shared/models/requests/target-introduction-pending-request.model';
import { Player } from 'src/shared/models/player/player.model';

@Component({
  selector: 'app-accept-request',
  templateUrl: './accept-request.component.html',
  styleUrls: ['./accept-request.component.css']
})
export class AcceptRequestComponent implements OnInit {

  error: boolean;

  success?: boolean;

  successMessage: string;
  
  successMessageAccept: string = "Request accepted sucessfully! Refreshing in 2 seconds.";

  successMessageDeny: string = "Request denied sucessfully! Refreshing in 2 seconds.";
  
  errorMessage: string = "There was an error with the request!";

  step: number;

  requests: TargetPendingRequest[];

  chosenRequest: TargetPendingRequest;

  requestIdSelected: string;

  r: AcceptRequest;

  requestForm = this.fb.group({
    connectionStrength: ['', [Validators.required, Validators.min(1), Validators.max(100)]],
    tags: this.fb.array([])
  })

  constructor(private rService: RequestService,
    private spinner: NgxSpinnerService,
    private location: Location,
    private fb: FormBuilder) { }

    ngOnInit(): void {
      this.r = new AcceptRequest;
      this.getRequests('email1@gmail.com');
    }

    setStep(index: number) {
      this.step = index;
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
  
    getRequestById(id: string): void {
      this.requestIdSelected = id;
      for(let request of this.requests) {
        if (request.id == id) {
          this.chosenRequest = request;
        }
      }
    }

    createUpdatedConnection() {
      this.r.id = this.requestIdSelected;
      this.r.strength = this.requestForm.value.connectionStrength;
      this.r.tags = [];
      for(let tag of this.requestForm.value.tags)
      {
        if(!this.r.tags.includes(tag.tag))
          this.r.tags.push(tag.tag);
      }
    }

    getMiddleManMessage(request: TargetPendingRequest): string | null {
      var intr = <TargetIntroductionPendingRequest>request;
      if(intr.middleManToTargetMessage != undefined) {
        return intr.middleManToTargetMessage;
      }
      return null;
    }

    getTargetMessageShort(request: TargetPendingRequest): string | null {
      var intr = <TargetIntroductionPendingRequest>request;
      if(intr.playerToTargetMessage != undefined) {
        return intr.playerToTargetMessage.substring(0,50) + "...";
      }
      return null;
    }

    getMiddleMan(request: TargetPendingRequest): Player | null {
      var intr = <TargetIntroductionPendingRequest>request;
      if(intr.middleManToTargetMessage != undefined) {
        return intr.middleMan;
      }
      return null;
    }

    getRequests(email: string): void {
      this.spinner.show();
      this.rService.getRequests(email).subscribe({ next: data => {
        this.requests = data;
        this.spinner.hide();
      },
        error: _error => {
          this.spinner.hide();
          this.error = true;
        }
      });
    }

    accept(): void {
      this.createUpdatedConnection();
      this.spinner.show();
      this.rService.acceptRequest(this.requestIdSelected, this.r)
      .subscribe({ next: data => {
        if(data) {
          this.success = true;
          this.successMessage = this.successMessageAccept;
          this.r = new AcceptRequest;
        }
        this.spinner.hide();
        this.refreshOnTime();
      },
        error: _error => {
          this.success = false;
          this.r = new AcceptRequest;
          this.spinner.hide();
        }
      });
    }

    deny(): void {
      this.spinner.show();
      this.rService.denyRequest(this.requestIdSelected)
      .subscribe({ next: data => {
        if(data) {
          this.success = true;
          this.successMessage = this.successMessageDeny;
        }
        this.spinner.hide();
        this.refreshOnTime();
      },
        error: _error => {
          this.success = false;
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
