import { HttpClientTestingModule } from '@angular/common/http/testing';
import { DebugElement } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { of } from 'rxjs';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { RequestService } from '../../services/request.service';

import { AcceptRequestComponent } from './accept-request.component';

describe('AcceptRequestComponent', () => {
  let component: AcceptRequestComponent;
  let fixture: ComponentFixture<AcceptRequestComponent>;

  let cService: ConnectionService;
  let rService: RequestService;
  let pService: PlayerService;

  let spy: jasmine.Spy;
  let spy2: jasmine.Spy;
  let spy3: jasmine.Spy;
  let spy4: jasmine.Spy;
  let de: DebugElement;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ AcceptRequestComponent ],
      imports: [HttpClientTestingModule, ReactiveFormsModule],
      providers: [PlayerService, RequestService, ConnectionService]
    })
    .compileComponents();
  });

  beforeEach(() => {
    localStorage.setItem('currentPlayer', 'email1@gmail.com');
    fixture = TestBed.createComponent(AcceptRequestComponent);
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

  it('success should be undefined', () =>{
    expect(component.success).toBeUndefined();
  });

  it('error should be undefined', () =>{
    expect(component.error).toBeUndefined();
  });

  it('successMessage should be undefined', () =>{
    expect(component.successMessage).toBeUndefined();
  });

  it('successMessage should be Request accepted sucessfully! Refreshing in 2 seconds.', () =>{
    expect(component.successMessageAccept).toBe("Request accepted sucessfully! Refreshing in 2 seconds.")
  });

  it('successMessage should be Request accepted sucessfully! Refreshing in 2 seconds.', () =>{
    expect(component.successMessageDeny).toBe("Request accepted sucessfully! Refreshing in 2 seconds.")
  });

  it('currentPlayer should be email1@gmail.com', () =>{
    expect(localStorage.getItem('currentPlayer')).toBe("email1@gmail.com")
  });
});
