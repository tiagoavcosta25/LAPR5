import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ProfileCommonFriendsComponent } from './profile-common-friends.component';

describe('ProfileCommonFriendsComponent', () => {
  let component: ProfileCommonFriendsComponent;
  let fixture: ComponentFixture<ProfileCommonFriendsComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ProfileCommonFriendsComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(ProfileCommonFriendsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
