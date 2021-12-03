import { ComponentFixture, TestBed } from '@angular/core/testing';

import { CreateIntRequestComponent } from './create-int-request.component';

describe('CreateIntRequestComponent', () => {
  let component: CreateIntRequestComponent;
  let fixture: ComponentFixture<CreateIntRequestComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ CreateIntRequestComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateIntRequestComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
