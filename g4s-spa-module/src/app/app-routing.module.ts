import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { RegisterPlayerComponent } from './modules/player/components/register-player/register-player.component';
import { GetPlayersComponent } from './modules/player/components/get-players/get-players.component';
import { LoginComponent } from './modules/login/login.component';
import { EditConnectionComponent } from './modules/connection/components/edit-connection/edit-connection.component';
import { AcceptRequestComponent } from './modules/request/components/accept-request/accept-request.component';
import { RequestIntroductionComponent } from './modules/request/components/request-introduction/request-introduction.component';
import { ApproveRequestComponent } from './modules/request/components/approve-request/approve-request.component';
import { UpdatePlayerComponent } from './modules/player/components/update-player/update-player.component';
import { SearchPlayerComponent } from './modules/player/components/search-player/search-player.component';
import { GetNetworkComponent } from './modules/network/components/get-network/get-network.component';
import { LayoutComponent } from './modules/layout/layout.component';
import { StrongestRouteComponent } from './modules/ai/components/strongest-route/strongest-route.component';
import { CommonTagsComponent } from './modules/ai/components/common-tags/common-tags.component';

const routes: Routes = [
  { path: '', redirectTo: 'get-players', pathMatch: 'full' },
  { path: '', 
    component: LayoutComponent,
    children: [
      { path: 'register-player',  component: RegisterPlayerComponent },
      { path: 'get-players',  component: GetPlayersComponent },
      { path: 'get-network',  component: GetNetworkComponent },
      { path: 'request-introduction',  component: RequestIntroductionComponent },
      { path: 'edit-connection',  component: EditConnectionComponent },
      { path: 'accept-request',  component: AcceptRequestComponent },
      { path: 'approve-request',  component: ApproveRequestComponent },
      { path: 'update-player',  component: UpdatePlayerComponent },
      { path: 'search-player', component: SearchPlayerComponent},
      { path: 'strongest-route', component: StrongestRouteComponent}
    ]
  },
  { path: 'login',  component: LoginComponent },
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
