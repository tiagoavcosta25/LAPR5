import { ComponentFixture, TestBed } from '@angular/core/testing';

import { CommonTagsComponent } from './common-tags.component';

describe('CommonTagsComponent', () => {
  let component: CommonTagsComponent;
  let fixture: ComponentFixture<CommonTagsComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ CommonTagsComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(CommonTagsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
