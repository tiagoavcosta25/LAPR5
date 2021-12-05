import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { DirectRequest } from 'src/shared/models/requests/direct-request.model';
import { ConnectionService } from '../../../connection/services/connection.service';
import { RequestService } from '../../../request/services/request.service';
import { PlayerService } from '../../services/player.service';
import { of } from 'rxjs';
import { SearchPlayerComponent } from './search-player.component';
import { Player } from 'src/shared/models/player/player.model';
import { DebugElement } from '@angular/core';
import { GettingConnection } from 'src/shared/models/connection/getting-connection.model';
import { CreatingDirectRequest } from 'src/shared/models/requests/creating-direct-request.model';


describe('SearchPlayerComponent', () => {
  let component: SearchPlayerComponent;
  let fixture: ComponentFixture<SearchPlayerComponent>;

  let pService: PlayerService;
  let rService: RequestService;
  let cService: ConnectionService;

  let spy: jasmine.Spy;
  let spy2: jasmine.Spy;
  let spy3: jasmine.Spy;
  let spy4: jasmine.Spy;
  let de: DebugElement;

  let p = new Player();
  let p2 = new Player();
  let dr = new DirectRequest();
  let c = new GettingConnection();

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [SearchPlayerComponent],
      imports: [HttpClientTestingModule, ReactiveFormsModule],
      providers: [PlayerService, RequestService, ConnectionService]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(SearchPlayerComponent);
    component = fixture.componentInstance;
    de = fixture.debugElement;

    pService = de.injector.get<PlayerService>(PlayerService);
    rService = de.injector.get<RequestService>(RequestService);
    cService = de.injector.get<ConnectionService>(ConnectionService);

    p.id = "1", p.name = "user1", p.email = "email1@gmail.com", p.year = 1,
    p.month = 1, p.day = 1, p.phoneNumber = 1, p.emotionalStatus = "joyful", p.facebook = "facebook.com",
    p.linkedIn = "linedIn.com", p.tags = [];
    p2 = p;
    p2.id = "2";

    c.id = "1", c.player = p, c.friend = p, c.connectionStrength = 1, c.tags = [];

    dr.id = "1", dr.player = "1", dr.target = "2", dr.playerToTargetMessage = "asd", dr.currentStatus = "request_pending", dr.strength = 3, dr.tags = []; 

    spy = spyOn(pService, 'getPlayerByEmail').and.returnValue(of([p]));

    spy2 = spyOn(cService, 'getConnections').and.returnValue(of([c]));

    spy3 = spyOn(pService, 'searchPlayers').and.returnValue(of([p2]));

    spy4 = spyOn(rService, 'sendDirectRequest').and.returnValue(of(dr));
    
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('currentPlayer should be p', () =>{
    expect(component.currentPlayer).toEqual(p)
  });

  it('error should be unnedefined', () =>{
    expect(component.error).toBeUndefined()
  });

  it('success should be unnedefined', () =>{
    expect(component.success).toBeUndefined()
  });

  it('successMessage should be unnedefined', () =>{
    expect(component.successMessage).toBeUndefined()
  });

  it('successMessageAccept should be string', () =>{
    expect(component.successMessageAccept).toBe("Request accepted sucessfully! Refreshing in 2 seconds.")
  });

  it('successMessageDeny should be string', () =>{
    expect(component.successMessageDeny).toBe("Request denied sucessfully! Refreshing in 2 seconds.")
  });

  it('step should be undefined', () =>{
    expect(component.step).toBeUndefined
  });

  it('players should be unnedefined', () =>{
    expect(component.players).toBeUndefined()
  });

  it('friends should be p', () =>{
    expect(component.friends).toEqual([p])
  });

  it('chosenPlayer should be unnedefined', () =>{
    expect(component.chosenPlayer).toBeUndefined()
  });

  it('playerIdSelected should be unnedefined', () =>{
    expect(component.playerIdSelected).toBeUndefined()
  });

  it('r should be unnedefined', () =>{
    expect(component.r).toEqual(new DirectRequest)
  });

  it('currentPlayerUpdate should update player', () => {
    expect(spy).toHaveBeenCalled;
    expect(component.currentPlayer).toEqual(p)
  })

  it('getFriends should return connections', () => {
    expect(spy2).toHaveBeenCalled();
    expect(component.friends.length).toEqual(1);
  })

  it('search should return list', () => {
    component.currentPlayer = p;
    component.search();
    expect(spy3).toHaveBeenCalled();
    expect(component.players).toBeDefined(),
    expect(component.players).toEqual([])
  })

  it('sendRequest should return success', () => {
    component.timeLeft = 10000;
    component.currentPlayer = p;
    component.chosenPlayer = p2;
    component.sendRequest();
    expect(spy4).toHaveBeenCalled();
    expect(component.success).toEqual(true),
    expect(component.successMessage).toEqual(component.successMessageAccept),
    expect(component.r).toEqual(new CreatingDirectRequest)
  })
});
