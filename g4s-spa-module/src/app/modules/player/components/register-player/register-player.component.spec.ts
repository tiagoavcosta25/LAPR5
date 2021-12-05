import { DebugElement } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { PlayerService } from '../../services/player.service';
import { of } from 'rxjs';

import { RegisterPlayerComponent } from './register-player.component';
import { Player } from 'src/shared/models/player/player.model';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { CreatingPlayer } from '../../models/creating-player.model';
import { By } from '@angular/platform-browser';

describe('RegisterPlayerComponent', () => {
  let component: RegisterPlayerComponent;
  let fixture: ComponentFixture<RegisterPlayerComponent>;

  let pService: PlayerService;

  let spy: jasmine.Spy;
  let de: DebugElement;

  let p = new Player();
  let p2 = new CreatingPlayer();
  let np = new CreatingPlayer();

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [RegisterPlayerComponent],
      imports: [HttpClientTestingModule, ReactiveFormsModule],
      providers: [PlayerService]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(RegisterPlayerComponent);
    component = fixture.componentInstance;

    fixture = TestBed.createComponent(RegisterPlayerComponent);
    component = fixture.componentInstance;
    de = fixture.debugElement;

    pService = de.injector.get<PlayerService>(PlayerService);

    p.id = "1", p.name = "user1", p.email = "email1@gmail.com", p.year = 1,
    p.month = 1, p.day = 1, p.phoneNumber = 1, p.emotionalStatus = "joyful", p.facebook = "facebook.com",
    p.linkedIn = "linedIn.com", p.tags = [];

    p2.name = "user1", p2.email = "email1@gmail.com", p2.year = 1,
    p2.month = 1, p2.day = 1, p2.phoneNumber = 1, p2.emotionalStatus = "joyful", p2.facebook = "facebook.com",
    p2.linkedIn = "linedIn.com", p2.tags = [];
    spy = spyOn(pService, 'registerPlayer').and.returnValue(of(p));
    
    fixture.detectChanges();
  });

  

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('p should be np', () =>{
    expect(component.p.name).toBe(np.name);
  });

  it('success should be undefined', () =>{
    expect(component.success).toBeUndefined();
  });

  it('successMessage should be Player created sucessfully!', () =>{
    expect(component.successMessage).toBe("Player created sucessfully!")
  });

  it('successMessage should be There was an error creating a player!', () =>{
    expect(component.errorMessage).toBe("There was an error creating a player!")
  });
});
