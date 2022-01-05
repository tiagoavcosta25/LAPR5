import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ProfileTagCloudComponent } from './profile-tag-cloud.component';

describe('ProfileTagCloudComponent', () => {
  let component: ProfileTagCloudComponent;
  let fixture: ComponentFixture<ProfileTagCloudComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ProfileTagCloudComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(ProfileTagCloudComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
