import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SuggestPlayersComponent } from './suggest-players.component';

describe('SuggestPlayersComponent', () => {
  let component: SuggestPlayersComponent;
  let fixture: ComponentFixture<SuggestPlayersComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ SuggestPlayersComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(SuggestPlayersComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
