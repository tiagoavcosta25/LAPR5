import { HttpClientTestingModule } from '@angular/common/http/testing';
import { DebugElement } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { of } from 'rxjs';
import { DobPlayer } from '../../models/dob-player.model copy';
import { PlayerService } from '../../services/player.service';

import { UpdatePlayerComponent } from './update-player.component';

describe('UpdatePlayerComponent', () => {
  let component: UpdatePlayerComponent;
  let fixture: ComponentFixture<UpdatePlayerComponent>;

  let pService: PlayerService;

  let spy: jasmine.Spy;
  let spy2: jasmine.Spy;
  let de: DebugElement;

  let p = new DobPlayer();
  let np = new DobPlayer();

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ UpdatePlayerComponent ],
      imports: [HttpClientTestingModule, ReactiveFormsModule],
      providers: [PlayerService]
    })
    .compileComponents();
  });

  beforeEach(() => {
    localStorage.setItem('currentPlayer', 'email1@gmail.com');
    fixture = TestBed.createComponent(UpdatePlayerComponent);
    component = fixture.componentInstance;
    de = fixture.debugElement;

    pService = de.injector.get<PlayerService>(PlayerService);

    p.id = "1", p.name = "user1", p.email = "email1@gmail.com", p.dateOfBirth = "2000-1-1T00:00:00",
     p.phoneNumber = 1, p.emotionalStatus = "joyful", p.facebook = "facebook.com",
    p.linkedIn = "linedIn.com", p.tags = [];

    spy = spyOn(pService, 'getOnlyPlayerByEmail').and.returnValue(of(p));
    spy2 = spyOn(pService, 'updatePlayer').and.returnValue(of(p));
    
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('p should be p', () =>{
    expect(component.p).toBe(p);
  });

  it('success should be undefined', () =>{
    expect(component.success).toBeUndefined();
  });

  it('successMessage should be Player updated sucessfully!', () =>{
    expect(component.successMessage).toBe("Player updated sucessfully!")
  });

  it('successMessage should be There was an error updating a player!', () =>{
    expect(component.errorMessage).toBe("There was an error updating a player!")
  });

  it('currentPlayer should be email1@gmail.com', () =>{
    expect(localStorage.getItem('currentPlayer')).toBe("email1@gmail.com")
  });
});
