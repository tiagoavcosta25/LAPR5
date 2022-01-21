import { ComponentFixture, TestBed } from '@angular/core/testing';

import { AipathsComponent } from './aipaths.component';

describe('AipathsComponent', () => {
  let component: AipathsComponent;
  let fixture: ComponentFixture<AipathsComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ AipathsComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(AipathsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
