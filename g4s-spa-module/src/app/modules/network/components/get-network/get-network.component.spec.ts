import { HttpClientTestingModule } from '@angular/common/http/testing';
import { DebugElement } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { RouterTestingModule } from '@angular/router/testing';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { RequestService } from 'src/app/modules/request/services/request.service';

import { GetNetworkComponent } from './get-network.component';

describe('GetNetworkComponent', () => {
  let component: GetNetworkComponent;
  let fixture: ComponentFixture<GetNetworkComponent>;

  let cService: ConnectionService;
  let rService: RequestService;
  let pService: PlayerService;

  let spy: jasmine.Spy;
  let de: DebugElement;

  let showForm = true;
  let showGraph = false;
  let players = [];
  let playersIds = [];
  let playersTempIds = [];
  let network = [];

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ GetNetworkComponent ],
      imports: [HttpClientTestingModule, ReactiveFormsModule, RouterTestingModule],
      providers: [PlayerService, RequestService, ConnectionService]
    })
    .compileComponents();
  });

  beforeEach(() => {
    localStorage.setItem('currentPlayer', 'email1@gmail.com');
    fixture = TestBed.createComponent(GetNetworkComponent);
    component = fixture.componentInstance;
    de = fixture.debugElement;

    pService = de.injector.get<PlayerService>(PlayerService);
    rService = de.injector.get<RequestService>(RequestService);
    cService = de.injector.get<ConnectionService>(ConnectionService);
    
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('showForm should be sf', () =>{
    expect(component.showForm).toBe(showForm);
  });

  it('showGraph should be sg', () =>{
    expect(component.showGraph).toBe(showGraph);
  });

  it('players should be ps', () =>{
    expect(component.players.length).toBe(players.length);
  });

  it('playersIds should be pids', () =>{
    expect(component.playersIds.length).toBe(playersIds.length);
  });

  it('playersTempIds should be ptids', () =>{
    expect(component.playersIds.length).toBe(playersTempIds.length);
  });

  it('network should be nt', () =>{
    expect(component.network.length).toBe(network.length);
  });

  it('success should be undefined', () =>{
    expect(component.success).toBeUndefined();
  });

  it('currentPlayer should be email1@gmail.com', () =>{
    expect(localStorage.getItem('currentPlayer')).toBe("email1@gmail.com")
  });
});