import { HttpClientTestingModule } from '@angular/common/http/testing';
import { templateJitUrl } from '@angular/compiler';
import { DebugElement } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { NgxSpinnerModule } from 'ngx-spinner';
import { of } from 'rxjs';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { Player } from 'src/shared/models/player/player.model';
import { AiService } from '../../services/ai.service';

import { StrongestRouteComponent } from './strongest-route.component';

describe('StrongestRouteComponent', () => {
  let component: StrongestRouteComponent;
  let fixture: ComponentFixture<StrongestRouteComponent>;

  let aiService: AiService;
  let pService: PlayerService;

  let spy: jasmine.Spy;
  let spy2: jasmine.Spy;
  let de: DebugElement;

  let p: Player = new Player;
  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ StrongestRouteComponent ],
      imports: [HttpClientTestingModule, NgxSpinnerModule],
      providers: [AiService, PlayerService]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(StrongestRouteComponent);
    component = fixture.componentInstance;
    de = fixture.debugElement;

    aiService = de.injector.get<AiService>(AiService);
    pService = de.injector.get<PlayerService>(PlayerService);


    p.id = "1", p.name = "user1", p.email = "email1@gmail.com", p.year = 1,
    p.month = 1, p.day = 1, p.phoneNumber = 1, p.emotionalStatus = "joyful", p.facebook = "facebook.com",
    p.linkedIn = "linedIn.com", p.tags = [];

    let s: string[] = [];
    spy = spyOn(aiService, 'getStrongestRoute').and.returnValue(of(s));
    spy2 = spyOn(pService, 'getPlayers').and.returnValue(of([p]));


    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('step should be undefined', () => {
    expect(component.step).toBeUndefined();
  });

  it('strongest route should be empty', () => {
    expect(component.strongestRoute).toEqual([]);
  });
  
  it('error should be undefined', () => {
    expect(component.error).toBeUndefined();
  });

  it('players should be empty', () => {
    expect(component.players).toEqual([]);
  });

  it('tempPlayers should have value', () => {
    expect(component.tempPlayers).toEqual([p]);
  });

  it('tempPlayers should have data', () => {
    component.getPlayers();
    expect(component.tempPlayers).toEqual([p]);
  })

  it('strongest route should have data', () => {
    component.getStrongestRoute("email1@gmail.com", "email3@gmail.com");
    expect(component.strongestRoute).toEqual([]);
  })
});
