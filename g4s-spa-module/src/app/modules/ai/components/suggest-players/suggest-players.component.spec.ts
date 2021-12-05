import { HttpClientTestingModule } from '@angular/common/http/testing';
import { DebugElement } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { of } from 'rxjs';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { DobPlayer } from 'src/app/modules/player/models/dob-player.model copy';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { RequestService } from 'src/app/modules/request/services/request.service';
import { Player } from 'src/shared/models/player/player.model';
import { AiService } from '../../services/ai.service';

import { SuggestPlayersComponent } from './suggest-players.component';

describe('SuggestPlayersComponent', () => {
  let component: SuggestPlayersComponent;
  let fixture: ComponentFixture<SuggestPlayersComponent>;

  let cService: ConnectionService;
  let aService: AiService;
  let pService: PlayerService;

  let spy: jasmine.Spy;
  let de: DebugElement;

  let email: string;
  email = 'email1@gmail.com';
  let player: DobPlayer;
  let suggestedIdList: string[];
  let suggestedList: Player[];
  let showForm: boolean = true;
  let showList: boolean = false;
  let step: number;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ SuggestPlayersComponent ],
      imports: [HttpClientTestingModule, ReactiveFormsModule],
      providers: [PlayerService, RequestService, ConnectionService]
    })
    .compileComponents();
  });

  beforeEach(() => {
    localStorage.setItem('currentPlayer', 'email1@gmail.com');
    fixture = TestBed.createComponent(SuggestPlayersComponent);
    component = fixture.componentInstance;
    de = fixture.debugElement;

    pService = de.injector.get<PlayerService>(PlayerService);
    aService = de.injector.get<AiService>(AiService);
    cService = de.injector.get<ConnectionService>(ConnectionService);
    
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('email should be email', () =>{
    expect(component.email).toBe(email);
  });

  it('player should be p', () =>{
    expect(component.player.name).toBeUndefined();
  });

  it('suggestedIdList should be sil', () =>{
    expect(component.suggestedIdList.length).toBe(0);
  });

  it('suggestedList should be sl', () =>{
    expect(component.suggestedList.length).toBe(0);
  });

  it('showForm should be sf', () =>{
    expect(component.showForm).toBe(showForm);
  });

  it('showList should be sl', () =>{
    expect(component.showList).toBe(showList);
  });

  it('step should be s', () =>{
    expect(component.step).toBe(step);
  });

  it('success should be undefined', () =>{
    expect(component.success).toBeUndefined();
  });

  it('error should be undefined', () =>{
    expect(component.error).toBeUndefined();
  });

  it('currentPlayer should be email1@gmail.com', () =>{
    expect(localStorage.getItem('currentPlayer')).toBe("email1@gmail.com")
  });
});
