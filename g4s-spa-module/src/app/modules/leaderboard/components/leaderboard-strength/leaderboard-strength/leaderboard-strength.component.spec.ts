import { ComponentFixture, TestBed } from '@angular/core/testing';

import { LeaderboardStrengthComponent } from './leaderboard-strength.component';

describe('LeaderboardStrengthComponent', () => {
  let component: LeaderboardStrengthComponent;
  let fixture: ComponentFixture<LeaderboardStrengthComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ LeaderboardStrengthComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(LeaderboardStrengthComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
