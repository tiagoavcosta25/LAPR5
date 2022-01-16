import { ComponentFixture, TestBed } from '@angular/core/testing';

import { LeaderboardDimensionComponent } from './leaderboard-dimension.component';

describe('LeaderboardDimensionComponent', () => {
  let component: LeaderboardDimensionComponent;
  let fixture: ComponentFixture<LeaderboardDimensionComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ LeaderboardDimensionComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(LeaderboardDimensionComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
