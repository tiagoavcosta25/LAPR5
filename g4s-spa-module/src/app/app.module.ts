import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';

import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { DashboardComponent } from './modules/layout/components/dashboard/dashboard.component';
import { LoginComponent } from './modules/login/login.component';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { MatToolbarModule } from '@angular/material/toolbar';
import { MatSidenavModule } from '@angular/material/sidenav';
import { MatListModule } from '@angular/material/list';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatDividerModule } from '@angular/material/divider';
import { HttpClientModule } from '@angular/common/http';
import { RegisterPlayerComponent } from './modules/player/components/register-player/register-player.component';
import { HeaderComponent } from './modules/layout/components/header/header.component';
import { PlayerComponent } from './modules/player/player.component';
import { GetNetworkComponent } from './modules/network/components/get-network/get-network.component';
import { NetworkComponent } from './modules/network/network.component';
import { SafestRouteComponent } from './modules/network/components/safest-route/safest-route.component';
import { SharedModule } from '../shared/shared.module';
import { GetPlayersComponent } from './modules/player/components/get-players/get-players.component';
import { RequestComponent } from './modules/request/request.component';
import { CreateIntRequestComponent } from './modules/request/components/create-int-request/create-int-request.component';


@NgModule({
  declarations: [
    AppComponent,
    DashboardComponent,
    LoginComponent,
    RegisterPlayerComponent,
    HeaderComponent,
    PlayerComponent,
    GetNetworkComponent,
    NetworkComponent,
    SafestRouteComponent,
    GetPlayersComponent,
    RequestComponent,
    CreateIntRequestComponent
  ],
  imports: [
    BrowserModule,
    AppRoutingModule,
    MatToolbarModule,
    MatSidenavModule,
    MatListModule,
    MatButtonModule,
    MatIconModule,
    MatDividerModule,
    BrowserAnimationsModule,
    HttpClientModule,
    SharedModule
  ],
  bootstrap: [AppComponent]
})
export class AppModule { }
