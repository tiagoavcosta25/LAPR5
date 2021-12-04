import { Component, OnInit } from '@angular/core';
import { FormBuilder, Validators } from '@angular/forms';
import { NgxSpinnerService } from 'ngx-spinner';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { Player } from 'src/shared/models/player/player.model';
import { NetworkService } from '../../services/network.service';

@Component({
  selector: 'app-safest-route',
  templateUrl: './safest-route.component.html',
  styleUrls: ['./safest-route.component.css']
})
export class SafestRouteComponent implements OnInit {

  safestForm = this.fb.group({
    email: ['', [Validators.required, Validators.pattern(/^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$/)]],
    scope: ['', [Validators.required, Validators.min(1), Validators.max(10)]]
  });

  email: string;
  success: any;
  playersIds: string[];
  players: Player[];
  showForm: boolean = true;
  showGraph: boolean = false;

  constructor(
    private spinner: NgxSpinnerService,
    private nService: NetworkService,
    private pService: PlayerService,
    private fb: FormBuilder) { 

      this.showForm = true;
      this.showGraph = false
    }

  ngOnInit(): void {
    this.players = [];
    this.email = "email1@gmail.com";
  }

  getPath(){
    /*this.nService.getSafestRoute(c).subscribe({ next: data => {
      this.players.push(data);
    },
      error: _error => {
      }
    });*/
  }
  
  getErrorMessageEmailRequired() {
    return this.safestForm.controls['email'].hasError('required') ? 'Email is required' : '';
  }

  getErrorMessageEmailInvalid() {
    return this.safestForm.controls['email'].hasError('pattern') ? 'Email is not valid' : '';
  }

  getErrorMessageScopeRequired() {
    return this.safestForm.controls['scope'].hasError('required') ? 'Scope is required' : '';
  }

  getErrorMessageScopeInvalid() {
    return (this.safestForm.controls['scope'].hasError('min') || this.safestForm.controls['scope'].hasError('max'))
      ? 'Scope should be between 1 and 10' : '';
  }

  clearSucess() {
    delete this.success;
  }

  get f() { return this.safestForm.controls; }

}
