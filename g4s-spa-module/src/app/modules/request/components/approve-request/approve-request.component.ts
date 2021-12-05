import { Component, OnInit } from '@angular/core';
import { FormArray, FormBuilder, Validators } from '@angular/forms';
import { NgxSpinnerService } from 'ngx-spinner';
import { ApproveRequest } from 'src/shared/models/requests/approve-request.model';
import { RequestService } from '../../services/request.service';
import { Location } from '@angular/common';
import { IntroductionRequest } from 'src/shared/models/requests/introduction-request.model';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { Player } from 'src/shared/models/player/player.model';
import { MiddleManIntroductionRequest } from 'src/shared/models/requests/middleman-introduction-pending-request.mode';


@Component({
  selector: 'app-approve-request',
  templateUrl: './approve-request.component.html',
  styleUrls: ['./approve-request.component.css']
})
export class ApproveRequestComponent implements OnInit {

  error: boolean;

  success?: boolean;

  successMessage: string;
  
  successMessageApproved: string = "Request approved sucessfully! Refreshing in 2 seconds.";

  successMessageDisapproved: string = "Request disapproved sucessfully! Refreshing in 2 seconds.";
  
  errorMessage: string = "There was an error with the request!";

  step: number;

  requests: IntroductionRequest[];
  fullRequests: MiddleManIntroductionRequest[];

  chosenRequest: IntroductionRequest;

  requestIdSelected: string;

  r: ApproveRequest;

  requestForm = this.fb.group({
    middleManToTargetMessage: ['', [Validators.required, Validators.min(1), Validators.max(100)]]
  })

  constructor(private rService: RequestService,
    private pService: PlayerService,
    private spinner: NgxSpinnerService,
    private location: Location,
    private fb: FormBuilder) { 
      this.requests = [];
      this.fullRequests = [];}

    ngOnInit(): void {
      this.r = new ApproveRequest;
      let playerEmail = localStorage.getItem('currentPlayer')!.trim();
      this.getMiddleManRequests(playerEmail);
    }

    setStep(index: number) {
      this.step = index;
    }

    get f() { return this.requestForm.controls; }
  
    getConnectionById(id: string): void {
      this.requestIdSelected = id;
      for(let request of this.requests) {
        if (request.id == id) {
          this.chosenRequest = request;
        }
      }
    }

    getPlayers(lstReq:IntroductionRequest[]){
      if(lstReq.length <= 0){
        return;
      }
      let req = lstReq.pop()!;

        this.pService.getPlayerById(req.player).subscribe({ next: data => {
          let player = data;
  
          this.pService.getPlayerById(req.target).subscribe({ next: data => {
            let target = data;
    
            let tempReq = new MiddleManIntroductionRequest();
            tempReq.id = req.id;
            tempReq.player = player;
            tempReq.target = target;
            tempReq.playerToMiddleManMessage = req.playerToMiddleManMessage;

            this.fullRequests.push(tempReq);

            this.getPlayers(lstReq);
    
            return;
          },
            error: _error => {
            }
          });
  
          return;
        },
          error: _error => {
          }
        });
      return;
      
    }

    createUpdatedConnection(status: string) {
      this.r.middleManToTargetMessage = this.requestForm.value.middleManToTargetMessage;
      this.r.status = status;
    }

    getMiddleManRequests(email: string): void {
      this.spinner.show();
      this.rService.getMiddleManRequests(email).subscribe({ next: data => {
        this.requests = data;
        this.getPlayers(this.requests);
        this.spinner.hide();
      },
        error: _error => {
          this.spinner.hide();
          this.error = true;
        }
      });
      this.spinner.hide();
    }

    approve(): void {
      this.createUpdatedConnection("request_pending");
      this.spinner.show();
      this.rService.approveRequest(this.requestIdSelected, this.r)
      .subscribe({ next: data => {
        if(data) {
          this.success = true;
          this.successMessage = this.successMessageApproved;
          this.r = new ApproveRequest;
        }
        this.spinner.hide();
        this.refreshOnTime();
      },
        error: _error => {
          this.success = false;
          this.r = new ApproveRequest;
          this.spinner.hide();
        }
      });
    }

    disapprove(): void {
      this.createUpdatedConnection("introduction_refused");
      this.spinner.show();
      this.rService.approveRequest(this.requestIdSelected, this.r)
      .subscribe({ next: data => {
        if(data) {
          this.success = true;
          this.successMessage = this.successMessageDisapproved;
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

    getErrorMessageMiddleManToTargetMessageRequired() {
      return this.requestForm.controls['middleManToTargetMessage'].hasError('required') ? 'Middle Man To Target Message is required' : '';
    }

    getErrorMessageMiddleManToTargetMessageInvalid() {
      return (this.requestForm.controls['middleManToTargetMessage'].hasError('min') || this.requestForm.controls['middleManToTargetMessage'].hasError('max'))
        ? 'Middle Man To Target Message should be between 1 and 100' : '';
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
