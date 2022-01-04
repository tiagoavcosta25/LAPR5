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
import { SafestRouteComponent } from './modules/ai/components/safest-route/safest-route.component';
import { AStarComponent } from './modules/ai/components/a-star/a-star.component';
import { SharedModule } from '../shared/shared.module';
import { GetPlayersComponent } from './modules/player/components/get-players/get-players.component';
import { RequestComponent } from './modules/request/request.component';
import { RequestIntroductionComponent } from './modules/request/components/request-introduction/request-introduction.component';
import { NgxSpinnerModule } from 'ngx-spinner';
import { ConnectionComponent } from './modules/connection/connection.component';
import { EditConnectionComponent } from './modules/connection/components/edit-connection/edit-connection.component';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatExpansionModule } from '@angular/material/expansion'
import { MatNativeDateModule } from '@angular/material/core';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule} from '@angular/material/select';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { AcceptRequestComponent } from './modules/request/components/accept-request/accept-request.component';
import { SearchPlayerComponent } from './modules/player/components/search-player/search-player.component'
import { UpdatePlayerComponent } from './modules/player/components/update-player/update-player.component';
import { ApproveRequestComponent } from './modules/request/components/approve-request/approve-request.component';
import { LayoutComponent } from './modules/layout/layout.component';
import { SuggestPlayersComponent } from './modules/ai/components/suggest-players/suggest-players.component';
import { StrongestRouteComponent } from './modules/ai/components/strongest-route/strongest-route.component';
import { ShortestRouteComponent } from './modules/ai/components/shortest-route/shortest-route.component';
import { UpdateEmotionalStatusComponent } from './modules/player/components/update-emotional-status/update-emotional-status.component';
import { FeedComponent } from './modules/feed/feed.component';
import { CreatePostComponent } from './modules/feed/components/create-post/create-post.component';
import { GetFeedComponent } from './modules/feed/components/get-feed/get-feed.component';
import { GeneralComponent } from './modules/general/general.component';
import { TermsAndCondComponent } from './modules/general/components/terms-and-cond/terms-and-cond.component';

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
    AStarComponent,
    GetPlayersComponent,
    RequestComponent,
    RequestIntroductionComponent,
    ConnectionComponent,
    EditConnectionComponent,
    AcceptRequestComponent,
    SearchPlayerComponent,
    UpdatePlayerComponent,
    ApproveRequestComponent,
    LayoutComponent,
    SuggestPlayersComponent,
    StrongestRouteComponent,
    ShortestRouteComponent,
    UpdateEmotionalStatusComponent,
    FeedComponent,
    CreatePostComponent,
    GetFeedComponent,
    GeneralComponent,
    TermsAndCondComponent
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
    SharedModule,
    NgxSpinnerModule,
    ReactiveFormsModule,
    FormsModule,
    NgxSpinnerModule,
    BrowserAnimationsModule,
    MatExpansionModule, 
    MatNativeDateModule, 
    MatFormFieldModule,
    MatInputModule,
    MatSelectModule,
    MatDatepickerModule

  ],
  bootstrap: [AppComponent]
})
export class AppModule { }
