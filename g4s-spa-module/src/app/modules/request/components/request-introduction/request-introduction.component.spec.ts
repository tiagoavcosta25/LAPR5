import { HttpClientTestingModule } from '@angular/common/http/testing';
import { DebugElement } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { of } from 'rxjs';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { DobPlayer } from 'src/app/modules/player/models/dob-player.model copy';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { Player } from 'src/shared/models/player/player.model';
import { ConnectionRequest } from 'src/shared/models/requests/connection-request.model';
import { IntroductionRequest } from 'src/shared/models/requests/introduction-request.model';
import { CreatingConnectionRequest } from '../../models/creating-connection-request.model';
import { RequestService } from '../../services/request.service';

import { RequestIntroductionComponent } from './request-introduction.component';

describe('CreateIntRequestComponent', () => {
  let component: RequestIntroductionComponent;
  let fixture: ComponentFixture<RequestIntroductionComponent>;

  let cService: ConnectionService;
  let rService: RequestService;
  let pService: PlayerService;

  let spy: jasmine.Spy;
  let spy2: jasmine.Spy;
  let spy3: jasmine.Spy;
  let spy4: jasmine.Spy;
  let de: DebugElement;

  let c = new CreatingConnectionRequest();
  let c2 = new IntroductionRequest();
  let mutualFriends : Player[];
  mutualFriends = [];
  let reachablePlayer = new Player;
  let player = new DobPlayer;
  let mutualFriend = new Player;
  let reachablePlayers: Player[];

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ RequestIntroductionComponent ],
      imports: [HttpClientTestingModule, ReactiveFormsModule],
      providers: [PlayerService, RequestService, ConnectionService]
    })
    .compileComponents();
  });

  beforeEach(() => {
    localStorage.setItem('currentPlayer', 'email1@gmail.com');
    fixture = TestBed.createComponent(RequestIntroductionComponent);
    component = fixture.componentInstance;
    de = fixture.debugElement;

    pService = de.injector.get<PlayerService>(PlayerService);
    rService = de.injector.get<RequestService>(RequestService);
    cService = de.injector.get<ConnectionService>(ConnectionService);

    spy = spyOn(cService, 'getMutualFriends').and.returnValue(of(mutualFriends));
    spy2 = spyOn(cService, 'getReachablePlayers').and.returnValue(of(reachablePlayers));
    spy2 = spyOn(pService, 'getOnlyPlayerByEmail').and.returnValue(of(player));
    spy2 = spyOn(rService, 'registerIntroductionRequest').and.returnValue(of(c2));
    
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('mutualFriend should be mt', () =>{
    expect(component.mutualFriend.name).toBe(mutualFriend.name);
  });

  it('mutualFriends should be mts', () =>{
    expect(component.mutualFriends.length).toBe(mutualFriends.length);
  });

  it('reachablePlayer should be rp', () =>{
    expect(component.reachablePlayer.name).toBe(reachablePlayer.name);
  });

  it('player should be p', () =>{
    expect(component.player.name).toBe(player.name);
  });

  it('success should be undefined', () =>{
    expect(component.success).toBeUndefined();
  });

  it('successMessage should be Introduction requested with sucess', () =>{
    expect(component.successMessage).toBe("Introduction requested with sucess")
  });

  it('successMessage should be There was an error with the request!', () =>{
    expect(component.errorMessage).toBe("There was an error with the request!")
  });

  it('currentPlayer should be email1@gmail.com', () =>{
    expect(localStorage.getItem('currentPlayer')).toBe("email1@gmail.com")
  });
});
