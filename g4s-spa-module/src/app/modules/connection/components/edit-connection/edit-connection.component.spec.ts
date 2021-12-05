import { HttpClientTestingModule } from '@angular/common/http/testing';
import { DebugElement } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { MatExpansionModule } from '@angular/material/expansion';
import { By } from '@angular/platform-browser';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { NgxSpinnerModule, NgxSpinnerService } from 'ngx-spinner';
import { of } from 'rxjs';
import { Connection } from 'src/shared/models/connection/connection.model';
import { GettingConnection } from 'src/shared/models/connection/getting-connection.model';
import { UpdatingConnection } from 'src/shared/models/connection/updating-connection.model';
import { Player } from 'src/shared/models/player/player.model';
import { ConnectionService } from '../../services/connection.service';

import { EditConnectionComponent } from './edit-connection.component';

describe('EditConnectionComponent', () => {
  let component: EditConnectionComponent;
  let fixture: ComponentFixture<EditConnectionComponent>;

  let cService: ConnectionService;
  
  let spy: jasmine.Spy;
  let spy2: jasmine.Spy;
  let de: DebugElement;

  let p: Player = new Player;
  let c: GettingConnection = new GettingConnection;
  let con: Connection = new Connection;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ EditConnectionComponent ],
      imports: [HttpClientTestingModule, ReactiveFormsModule, MatExpansionModule, NgxSpinnerModule, BrowserAnimationsModule],
      providers: [ConnectionService]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(EditConnectionComponent);
    component = fixture.componentInstance;
    de = fixture.debugElement;

    cService = de.injector.get<ConnectionService>(ConnectionService);
    p.id = "1", p.name = "user1", p.email = "email1@gmail.com", p.year = 1,
    p.month = 1, p.day = 1, p.phoneNumber = 1, p.emotionalStatus = "joyful", p.facebook = "facebook.com",
    p.linkedIn = "linedIn.com", p.tags = [];

    c.id = "1", c.player = p, c.friend = p, c.connectionStrength = 1, c.tags = [];

    con.id = "1", con.player = "1", con.friend = "1", con.connectionStrength = 1, con.tags = [];

    spy = spyOn(cService, 'getConnections').and.returnValue(of([c]));
    spy2 = spyOn(cService, 'updateConnection').and.returnValue(of(con));

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

  it('success message should contain sucessfully', () =>{
    expect(component.successMessage).toContain("sucessfully!")
  });

  it('error message should contain error', () =>{
    expect(component.errorMessage).toContain("error")
  });

  it('step should be undefined', () =>{
    expect(component.step).toBeUndefined()
  });

  it('connections shoud return', () =>{
    let lst = [c];
    expect(component.connections).toEqual(lst)
  });

  it('chosenConnection shoud be undefined', () =>{
    expect(component.chosenConnection).toBeUndefined()
  });

  it('connectionIdSelected shoud be undefined', () =>{
    expect(component.connectionIdSelected).toBeUndefined()
  });

  it('c shoud be empty', () =>{
    let c: UpdatingConnection = new UpdatingConnection;
    expect(component.c).toEqual(c)
  });

  it('connections shoud have data', () =>{
    component.getConnections("email1@gmail.com");
    expect(component.connections).toEqual([c])
  });

  it('c shoud have be new Connection', () =>{
    component.save();
    expect(component.c).toEqual(new Connection)
  });

  it('success should be undefined', () => {
    expect(component.success).toBeUndefined();
  }) 

});
