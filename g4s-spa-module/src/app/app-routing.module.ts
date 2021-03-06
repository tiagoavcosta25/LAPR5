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
import { ShortestRouteComponent } from './modules/ai/components/shortest-route/shortest-route.component';
import { StrongestRouteComponent } from './modules/ai/components/strongest-route/strongest-route.component';
import { UpdateEmotionalStatusComponent } from './modules/player/components/update-emotional-status/update-emotional-status.component';
import { SafestRouteComponent } from './modules/ai/components/safest-route/safest-route.component';
import { AStarComponent } from './modules/ai/components/a-star/a-star.component';
import { SuggestPlayersComponent } from './modules/ai/components/suggest-players/suggest-players.component';
import { CreatePostComponent } from './modules/feed/components/create-post/create-post.component';
import { GetFeedComponent } from './modules/feed/components/get-feed/get-feed.component';
import { GeneralComponent } from './modules/general/general.component';
import { TermsAndCondComponent } from './modules/general/components/terms-and-cond/terms-and-cond.component';
import { ProfileComponent } from './modules/player/components/profile/profile.component';
import { ProfileTimelineComponent } from './modules/player/components/profile/profile-timeline/profile-timeline.component';
import { ProfileFriendsComponent } from './modules/player/components/profile/profile-friends/profile-friends.component';
import { ProfileAboutComponent } from './modules/player/components/profile/profile-about/profile-about.component';
import { ProfileTagCloudComponent } from './modules/player/components/profile/profile-tag-cloud/profile-tag-cloud.component';
import { ProfileCommonFriendsComponent } from './modules/player/components/profile/profile-common-friends/profile-common-friends.component';
import { TagCloudMultiComponent } from './modules/tag-cloud-multi/tag-cloud-multi.component';
import { TagCloudUsersComponent } from './modules/tag-cloud-multi/components/tag-cloud-users/tag-cloud-users.component';
import { TagCloudConnectionsComponent } from './modules/tag-cloud-multi/components/tag-cloud-connections/tag-cloud-connections.component';
import { ProfileTagCloudConnComponent } from './modules/player/components/profile/profile-tag-cloud-conn/profile-tag-cloud-conn/profile-tag-cloud-conn.component';
import { LeaderboardDimensionComponent } from './modules/leaderboard/components/leaderboard-dimension/leaderboard-dimension.component';
import { LeaderboardStrengthComponent } from './modules/leaderboard/components/leaderboard-strength/leaderboard-strength.component';
import { LeaderboardComponent } from './modules/leaderboard/leaderboard.component';
import { AipathsComponent } from './modules/ai/components/aipaths/aipaths.component';
import { GroupSearchComponent } from './modules/ai/components/group-search/group-search.component';
import { AboutUsComponent } from './modules/general/components/about-us/about-us.component';
import { ContactUsComponent } from './modules/general/components/contact-us/contact-us.component';

const routes: Routes = [
  { path: '', redirectTo: 'login', pathMatch: 'full' },
  { path: 'tag-cloud-multi', redirectTo: 'tag-cloud-multi/tag-cloud-users'},
  { path: 'leaderboard', redirectTo: 'leaderboard/leaderboard-dimension'},

  { path: '', 
    component: LayoutComponent,
    children: [
      { path: 'create-post',  component: CreatePostComponent },
      { path: 'get-feed',  component: GetFeedComponent },
      { path: 'get-players',  component: GetPlayersComponent },
      { path: 'request-introduction',  component: RequestIntroductionComponent },
      { path: 'edit-connection',  component: EditConnectionComponent },
      { path: 'accept-request',  component: AcceptRequestComponent },
      { path: 'approve-request',  component: ApproveRequestComponent },
      { path: 'update-player',  component: UpdatePlayerComponent },
      { path: 'search-player', component: SearchPlayerComponent},
      { path: 'safest-route', component: SafestRouteComponent},
      { path: 'ai-paths', component: AipathsComponent},
      { path: 'group-search', component: GroupSearchComponent},
      { path: 'a-star', component: AStarComponent},
      { path: 'shortest-route', component: ShortestRouteComponent},
      { path: 'strongest-route', component: StrongestRouteComponent},
      { path: 'suggest-players', component: SuggestPlayersComponent},
      { path: 'update-emotional-status', component: UpdateEmotionalStatusComponent},
      { path: 'get-network',  component: GetNetworkComponent },
      { path: 'profile/:email', component: ProfileComponent, children: [
        { path: '', component: ProfileTimelineComponent },
        { path: 'profile-timeline', component: ProfileTimelineComponent },
        { path: 'profile-about', component: ProfileAboutComponent },
        { path: 'profile-tag-cloud',  component: ProfileTagCloudComponent },
        { path: 'profile-tag-cloud-conn',  component: ProfileTagCloudConnComponent },
        { path: 'profile-friends',  component: ProfileFriendsComponent },
        { path: 'profile-common-friends',  component: ProfileCommonFriendsComponent }
      ]},
      { path: 'tag-cloud-multi', component: TagCloudMultiComponent, children: [
        { path: 'tag-cloud-users', component: TagCloudUsersComponent },
        { path: 'tag-cloud-connections', component: TagCloudConnectionsComponent }
      ]},
      { path: 'leaderboard', component: LeaderboardComponent, children: [
        { path: 'leaderboard-dimension', component: LeaderboardDimensionComponent },
        { path: 'leaderboard-strength', component: LeaderboardStrengthComponent }
      ]}
    ]
  },
  { path: 'login',  component: LoginComponent },
  { path: 'register-player',  component: RegisterPlayerComponent },
  { path: '', 
    component: GeneralComponent,
    children: [
      { path: 'terms-and-cond',  component: TermsAndCondComponent },
      { path: 'about-us',  component: AboutUsComponent },
      { path: 'contact-us',  component: ContactUsComponent }
    ]
  },
];

@NgModule({
  imports: [RouterModule.forRoot(routes, {
    paramsInheritanceStrategy: 'always'
  })],
  exports: [RouterModule]
})
export class AppRoutingModule { }
