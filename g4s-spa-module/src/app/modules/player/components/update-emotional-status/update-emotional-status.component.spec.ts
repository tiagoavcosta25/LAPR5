import { HttpClientTestingModule } from '@angular/common/http/testing';
import { DebugElement } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormBuilder, ReactiveFormsModule } from '@angular/forms';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { NgxSpinnerModule } from 'ngx-spinner';
import { of } from 'rxjs';
import { ChangeEmotionalStatus } from 'src/shared/models/player/change-emotional-status.model';
import { DobPlayer } from '../../models/dob-player.model copy';
import { PlayerService } from '../../services/player.service';
import { UpdateEmotionalStatusComponent } from './update-emotional-status.component';

describe('UpdateEmotionalStatusComponent', () => {
  let component: UpdateEmotionalStatusComponent;
  let fixture: ComponentFixture<UpdateEmotionalStatusComponent>;

  var pService: PlayerService;

  let spy: jasmine.Spy;
  let spy2: jasmine.Spy;
  let de: DebugElement;

  let p: DobPlayer = new DobPlayer;
  let es: ChangeEmotionalStatus = new ChangeEmotionalStatus;
  

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ UpdateEmotionalStatusComponent ],
      imports: [HttpClientTestingModule, ReactiveFormsModule, NgxSpinnerModule],
      providers: [PlayerService]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(UpdateEmotionalStatusComponent);
    component = fixture.componentInstance;
    de = fixture.debugElement;

    pService = de.injector.get<PlayerService>(PlayerService);
    p.id = "1", p.name = "user1", p.email = "email1@gmail.com", p.dateOfBirth = "", p.phoneNumber = 1, p.emotionalStatus = "joyful", p.facebook = "facebook.com",
    p.linkedIn = "linedIn.com", p.tags = [];

    es.emotionalStatus = "joyful", es.playerEmail = "email1@gmail.com";

    spy = spyOn(pService, 'getOnlyPlayerByEmail').and.returnValue(of(p));

    spy = spyOn(pService, 'updateEmotionalStatus').and.returnValue(of(es));


    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('error should be undefined', () =>{
    expect(component.error).toBeUndefined()
  });

  it('success should be undefined', () =>{
    expect(component.success).toBeUndefined()
  });

  it('success message should contain success', () =>{
    expect(component.successMessage).toContain("sucessfully!")
  });

  it('error message should contain success', () =>{
    expect(component.errorMessage).toContain("error")
  });

  it('current player should be equal', () =>{
    expect(component.currentPlayer).toEqual(p)
  });

  it('ces should be equal', () =>{
    expect(component.ces).toEqual(new ChangeEmotionalStatus)
  });

  it('current player should have data', () =>{
    component.getCurrentPlayer("email1@gmail.com");
    expect(component.currentPlayer).toEqual(p)
  });

  it('ces should be empty', () =>{
    component.save();
    expect(component.ces).toEqual(new ChangeEmotionalStatus)
  });
});
