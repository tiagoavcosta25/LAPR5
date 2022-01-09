import { ComponentFixture, TestBed } from '@angular/core/testing';

import { TagCloudMultiComponent } from './tag-cloud-multi.component';

describe('TagCloudMultiComponent', () => {
  let component: TagCloudMultiComponent;
  let fixture: ComponentFixture<TagCloudMultiComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ TagCloudMultiComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(TagCloudMultiComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
