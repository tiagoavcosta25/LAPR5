import { Component, OnInit } from '@angular/core';
import { FormBuilder, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { environment } from 'src/environments/environment';

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css']
})
export class LoginComponent implements OnInit {

  loginForm = this.fb.group({
    email: [''],
    password: ['']
  });

  router: Router;

  constructor(
    private fb: FormBuilder,
    private r: Router) { 
      this.router = r;
    }

  ngOnInit(): void {
  }

  login(){
    localStorage.setItem('currentPlayer', this.loginForm.value.email);
    console.log(localStorage.getItem('currentPlayer'));
  }

  get f() { return this.loginForm.controls; }
}
