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
import { ProfileComponent } from './modules/player/components/profile/profile.component';
import { ProfileHeaderComponent } from './modules/player/components/profile/profile-header/profile-header.component';
import { ProfileTimelineComponent } from './modules/player/components/profile/profile-timeline/profile-timeline.component';
import { ProfileFriendsComponent } from './modules/player/components/profile/profile-friends/profile-friends.component';
import { ProfileAboutComponent } from './modules/player/components/profile/profile-about/profile-about.component';
import { TagCloudModule } from 'angular-tag-cloud-module';
import { ProfileTagCloudComponent } from './modules/player/components/profile/profile-tag-cloud/profile-tag-cloud.component';
import { ProfileCommonFriendsComponent } from './modules/player/components/profile/profile-common-friends/profile-common-friends.component';
import { TagCloudMultiComponent } from './modules/tag-cloud-multi/tag-cloud-multi.component';
import { TagCloudUsersComponent } from './modules/tag-cloud-multi/components/tag-cloud-users/tag-cloud-users.component';
import { TagCloudConnectionsComponent } from './modules/tag-cloud-multi/components/tag-cloud-connections/tag-cloud-connections.component';
import { ProfileTagCloudConnComponent } from './modules/player/components/profile/profile-tag-cloud-conn/profile-tag-cloud-conn/profile-tag-cloud-conn.component';
import { LeaderboardComponent } from './modules/leaderboard/leaderboard.component';
import { LeaderboardDimensionComponent } from './modules/leaderboard/components/leaderboard-dimension/leaderboard-dimension.component';
import { LeaderboardStrengthComponent } from './modules/leaderboard/components/leaderboard-strength/leaderboard-strength.component';
import { AipathsComponent } from './modules/ai/components/aipaths/aipaths.component';
import { GroupSearchComponent } from './modules/ai/components/group-search/group-search.component';
import { AboutUsComponent } from './modules/general/components/about-us/about-us.component';
import { ContactUsComponent } from './modules/general/components/contact-us/contact-us.component';

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
    TermsAndCondComponent,
    ProfileComponent,
    ProfileHeaderComponent,
    ProfileTimelineComponent,
    ProfileFriendsComponent,
    ProfileAboutComponent,
    ProfileTagCloudComponent,
    ProfileCommonFriendsComponent,
    TagCloudMultiComponent,
    TagCloudUsersComponent,
    TagCloudConnectionsComponent,
    ProfileTagCloudConnComponent,
    LeaderboardComponent,
    LeaderboardDimensionComponent, 
    LeaderboardStrengthComponent, AipathsComponent, GroupSearchComponent, AboutUsComponent, ContactUsComponent
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
    MatDatepickerModule,
    TagCloudModule

  ],
  bootstrap: [AppComponent]
})
export class AppModule { }
