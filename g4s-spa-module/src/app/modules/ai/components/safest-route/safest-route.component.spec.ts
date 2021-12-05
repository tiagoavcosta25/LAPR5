import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SafestRouteComponent } from './safest-route.component';

describe('SafestRouteComponent', () => {
  let component: SafestRouteComponent;
  let fixture: ComponentFixture<SafestRouteComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ SafestRouteComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(SafestRouteComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
