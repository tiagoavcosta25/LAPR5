import { ComponentFixture, TestBed } from '@angular/core/testing';

import { GetFeedComponent } from './get-feed.component';

describe('GetFeedComponent', () => {
  let component: GetFeedComponent;
  let fixture: ComponentFixture<GetFeedComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ GetFeedComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(GetFeedComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
