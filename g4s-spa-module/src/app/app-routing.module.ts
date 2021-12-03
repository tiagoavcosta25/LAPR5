import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { DashboardComponent } from './modules/layout/components/dashboard/dashboard.component';
import { RegisterPlayerComponent } from './modules/player/components/register-player/register-player.component';
import { GetPlayersComponent } from './modules/player/components/get-players/get-players.component';
import { LoginComponent } from './modules/login/login.component';
import { HeaderComponent } from './modules/layout/components/header/header.component';
import { EditConnectionComponent } from './modules/connection/components/edit-connection/edit-connection.component';

const routes: Routes = [
  { path: '', redirectTo: 'login', pathMatch: 'full' },
  { path: 'login',  component: LoginComponent },
  { path: 'header',  component: HeaderComponent },
  { path: 'dashboard',  component: DashboardComponent },
  { path: 'register-player',  component: RegisterPlayerComponent },
  { path: 'get-players',  component: GetPlayersComponent },
  { path: 'edit-connection',  component: EditConnectionComponent },
  { path: 'login',  component: LoginComponent }
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
