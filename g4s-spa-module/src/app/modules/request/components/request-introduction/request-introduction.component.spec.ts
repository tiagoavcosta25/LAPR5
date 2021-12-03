import { ComponentFixture, TestBed } from '@angular/core/testing';

import { RequestIntroductionComponent } from './request-introduction.component';

describe('CreateIntRequestComponent', () => {
  let component: RequestIntroductionComponent;
  let fixture: ComponentFixture<RequestIntroductionComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ RequestIntroductionComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(RequestIntroductionComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
