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

  constructor(
    private fb: FormBuilder,
    private router: Router) { 
    }

  ngOnInit(): void {
  }

  login(){
    localStorage.clear();
    localStorage.setItem('currentPlayer', this.loginForm.value.email);
    this.router.navigate(['/get-players']);
  }

  get f() { return this.loginForm.controls; }
}
