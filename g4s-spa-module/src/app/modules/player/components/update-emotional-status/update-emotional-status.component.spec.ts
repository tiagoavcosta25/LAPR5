import { ComponentFixture, TestBed } from '@angular/core/testing';

import { UpdateEmotionalStatusComponent } from './update-emotional-status.component';

describe('UpdateEmotionalStatusComponent', () => {
  let component: UpdateEmotionalStatusComponent;
  let fixture: ComponentFixture<UpdateEmotionalStatusComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ UpdateEmotionalStatusComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(UpdateEmotionalStatusComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
