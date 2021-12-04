import { ComponentFixture, TestBed } from '@angular/core/testing';

import { StrongestRouteComponent } from './strongest-route.component';

describe('StrongestRouteComponent', () => {
  let component: StrongestRouteComponent;
  let fixture: ComponentFixture<StrongestRouteComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ StrongestRouteComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(StrongestRouteComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
