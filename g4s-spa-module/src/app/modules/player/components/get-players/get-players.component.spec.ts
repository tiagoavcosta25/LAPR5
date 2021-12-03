import { ComponentFixture, TestBed } from '@angular/core/testing';

import { GetPlayersComponent } from './get-players.component';

describe('GetPlayersComponent', () => {
  let component: GetPlayersComponent;
  let fixture: ComponentFixture<GetPlayersComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ GetPlayersComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(GetPlayersComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
