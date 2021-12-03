import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { DashboardComponent } from './modules/layout/components/dashboard/dashboard.component';
import { RegisterPlayerComponent } from './modules/player/components/register-player/register-player.component';
import { GetPlayersComponent } from './modules/player/components/get-players/get-players.component';
import { LoginComponent } from './modules/login/login.component';
import { HeaderComponent } from './modules/layout/components/header/header.component';
import { EditConnectionComponent } from './modules/connection/components/edit-connection/edit-connection.component';
import { AcceptRequestComponent } from './modules/request/components/accept-request/accept-request.component';
import { RequestIntroductionComponent } from './modules/request/components/request-introduction/request-introduction.component';

const routes: Routes = [
  { path: '', redirectTo: 'request-introduction', pathMatch: 'full' },
  { path: 'login',  component: LoginComponent },
  { path: 'header',  component: HeaderComponent },
  { path: 'dashboard',  component: DashboardComponent },
  { path: 'register-player',  component: RegisterPlayerComponent },
  { path: 'get-players',  component: GetPlayersComponent },
  { path: 'request-introduction',  component: RequestIntroductionComponent },
  { path: 'edit-connection',  component: EditConnectionComponent },
  { path: 'accept-request',  component: AcceptRequestComponent },
  { path: 'login',  component: LoginComponent }
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
