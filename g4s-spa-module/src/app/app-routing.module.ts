import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { DashboardComponent } from './layout/dashboard/dashboard.component';
import { RegisterPlayerComponent } from './player/register-player/register-player.component';
import { LoginComponent } from './layout/login/login.component';
import { HeaderComponent } from './layout/header/header.component';

const routes: Routes = [
  { path: '', redirectTo: 'login', pathMatch: 'full' },
  { path: 'login',  component: LoginComponent },
  { path: 'header',  component: HeaderComponent },
  { path: 'dashboard',  component: DashboardComponent },
  { path: 'register-player',  component: RegisterPlayerComponent },
  { path: 'login',  component: LoginComponent }
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
