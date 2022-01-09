import { ComponentFixture, TestBed } from '@angular/core/testing';

import { TagCloudUsersComponent } from './tag-cloud-users.component';

describe('TagCloudUsersComponent', () => {
  let component: TagCloudUsersComponent;
  let fixture: ComponentFixture<TagCloudUsersComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ TagCloudUsersComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(TagCloudUsersComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
