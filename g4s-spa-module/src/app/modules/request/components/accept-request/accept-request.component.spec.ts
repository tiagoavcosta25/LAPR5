import { HttpClientTestingModule } from '@angular/common/http/testing';
import { DebugElement } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { MatExpansionModule } from '@angular/material/expansion';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { NgxSpinnerModule } from 'ngx-spinner';
import { of } from 'rxjs';
import { Player } from 'src/shared/models/player/player.model';
import { AcceptRequest } from 'src/shared/models/requests/accept-request.model';
import { TargetDirectPendingRequest } from 'src/shared/models/requests/target-direct-pending-request.model';
import { RequestService } from '../../services/request.service';

import { AcceptRequestComponent } from './accept-request.component';

describe('AcceptRequestComponent', () => {
  let component: AcceptRequestComponent;
  let fixture: ComponentFixture<AcceptRequestComponent>;

  let rService: RequestService;

  let spy: jasmine.Spy;
  let spy2: jasmine.Spy;
  let spy3: jasmine.Spy;
  let de: DebugElement;

  let p = new Player;
  let tPending = new TargetDirectPendingRequest;
  let aRequest = new AcceptRequest;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ AcceptRequestComponent ],
      imports: [HttpClientTestingModule, ReactiveFormsModule, MatExpansionModule, NgxSpinnerModule, BrowserAnimationsModule],
      providers: [RequestService]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(AcceptRequestComponent);
    component = fixture.componentInstance;
    de = fixture.debugElement;
    component.timeLeft = 100000;
    rService = de.injector.get<RequestService>(RequestService);

    p.id = "1", p.name = "user1", p.email = "email1@gmail.com", p.year = 1,
    p.month = 1, p.day = 1, p.phoneNumber = 1, p.emotionalStatus = "joyful", p.facebook = "facebook.com",
    p.linkedIn = "linedIn.com", p.tags = [];

    tPending.id = "1", tPending.player = p, tPending.target = p, tPending.playerToTargetMessage = "1231";

    aRequest.id ="1", aRequest.strength = 1, aRequest.tags = []; 

    spy = spyOn(rService, 'getRequests').and.returnValue(of([tPending]));
    spy2 = spyOn(rService, 'acceptRequest').and.returnValue(of(aRequest));
    spy3 = spyOn(rService, 'denyRequest').and.returnValue(of(aRequest));


    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('error should be undefined', () => {
    expect(component.error).toBeUndefined();
  });

  it('success should be undefined', () => {
    expect(component.success).toBeUndefined();
  });

  it('success message accept should contain sucessfully!', () => {
    expect(component.successMessageAccept).toContain("accepted sucessfully");
  });

  it('sucess message deny should contain sucessfully!', () => {
    expect(component.successMessageDeny).toContain("denied sucessfully!");
  });

  it('error message should contain error', () => {
    expect(component.errorMessage).toContain("error");
  });

  it('step should be undefined', () => {
    expect(component.step).toBeUndefined();
  });

  it('requests should have data', () => {
    expect(component.requests).toEqual([tPending]);
  });

  it('chosenRequest to be undefined', () => {
    expect(component.chosenRequest).toBeUndefined();
  });

  it('requestIdSelected to be undefined', () => {
    expect(component.requestIdSelected).toBeUndefined();
  });

  it('r to be have value', () => {
    expect(component.r).toEqual(new AcceptRequest);
  });

  it('r to be have value', () => {
    expect(component.r).toEqual(new AcceptRequest);
  });

  it('requests to be have value', () => {
    component.getRequests("email1@gmail.com");
    expect(component.requests).toEqual([tPending]);
  });

  it('r should be new', () => {
    component.accept();
    expect(component.r).toEqual(new AcceptRequest);
  });

  it('success message should be success accept', () => {
    component.accept();
    expect(component.successMessage).toContain("accepted");
  });

  it('r should be new', () => {
    component.deny();
    expect(component.r).toEqual(new AcceptRequest);
  });

  it('sucess message should be sucess deny', () => {
    component.deny();
    expect(component.successMessage).toContain("denied");
  });


});
