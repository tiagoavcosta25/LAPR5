import { HttpClientTestingModule } from '@angular/common/http/testing';
import { DebugElement } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { of } from 'rxjs';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { RequestService } from 'src/app/modules/request/services/request.service';
import { AiService } from '../../services/ai.service';

import { AStarComponent } from './a-star.component';

describe('AStarComponent', () => {
  let component: AStarComponent;
  let fixture: ComponentFixture<AStarComponent>;

  let cService: ConnectionService;
  let aService: AiService;
  let pService: PlayerService;

  let spy: jasmine.Spy;
  let de: DebugElement;

  let AStar : string[];
  AStar = [];

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ AStarComponent ],
      imports: [HttpClientTestingModule, ReactiveFormsModule],
      providers: [PlayerService, RequestService, ConnectionService]
    })
    .compileComponents();
  });

  beforeEach(() => {
    localStorage.setItem('currentPlayer', 'email1@gmail.com');
    fixture = TestBed.createComponent(AStarComponent);
    component = fixture.componentInstance;
    de = fixture.debugElement;

    pService = de.injector.get<PlayerService>(PlayerService);
    aService = de.injector.get<AiService>(AiService);
    cService = de.injector.get<ConnectionService>(ConnectionService);

    spy = spyOn(aService, 'getSafestRoute').and.returnValue(of(AStar));
    
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('AStar should be sr', () =>{
    expect(component.AStar.length).toBe(AStar.length);
  });

  it('error should be undefined', () =>{
    expect(component.error).toBeUndefined();
  });

  it('currentPlayer should be email1@gmail.com', () =>{
    expect(localStorage.getItem('currentPlayer')).toBe("email1@gmail.com")
  });
});
