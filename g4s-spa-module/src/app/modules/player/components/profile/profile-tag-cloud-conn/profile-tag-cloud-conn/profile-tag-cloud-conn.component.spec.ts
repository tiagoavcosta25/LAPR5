import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ProfileTagCloudConnComponent } from './profile-tag-cloud-conn.component';

describe('ProfileTagCloudConnComponent', () => {
  let component: ProfileTagCloudConnComponent;
  let fixture: ComponentFixture<ProfileTagCloudConnComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ProfileTagCloudConnComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(ProfileTagCloudConnComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
