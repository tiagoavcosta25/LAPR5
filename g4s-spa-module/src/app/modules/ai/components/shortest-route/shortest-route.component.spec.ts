import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ShortestRouteComponent } from './shortest-route.component';

describe('ShortestRouteComponent', () => {
  let component: ShortestRouteComponent;
  let fixture: ComponentFixture<ShortestRouteComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ShortestRouteComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(ShortestRouteComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
