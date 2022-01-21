import { Component, OnInit } from '@angular/core';
import { FormBuilder, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { NgxSpinnerService } from 'ngx-spinner';
import { environment } from 'src/environments/environment';
import { PlayerService } from '../player/services/player.service';

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css']
})
export class LoginComponent implements OnInit {

  loginError: boolean = false;

  wrongLogin: boolean = false;

  loginForm = this.fb.group({
    email: [''],
    password: ['']
  });

  constructor(
    private fb: FormBuilder,
    private pService: PlayerService,
    private router: Router,
    private spinner: NgxSpinnerService) { 
    }

  ngOnInit(): void {
  }

  login(): void {
    this.spinner.show();
    this.loginError = false;
    this.wrongLogin = false;
    let email = this.loginForm.value.email;
    let password = this.loginForm.value.password;
    this.pService.login(email, password).subscribe({ next: data => {
      let code = data;
      console.log(code);
      if(code == 1) {
        localStorage.clear();
        localStorage.setItem('currentPlayer', this.loginForm.value.email);
        this.router.navigate(['/get-feed']);
      } else {
        this.wrongLogin = true;
        console.log("Erro no login");
        this.resetForm();
      }
      this.spinner.hide();
    },
      error: _error => {
        this.loginError = true;
        this.spinner.hide();
      }
    });
  }

  resetForm(): void {
    this.loginForm.reset();
  }

  get f() { return this.loginForm.controls; }
}
