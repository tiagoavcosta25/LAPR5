import { ComponentFixture, TestBed } from '@angular/core/testing';

import { TagCloudConnectionsComponent } from './tag-cloud-connections.component';

describe('TagCloudConnectionsComponent', () => {
  let component: TagCloudConnectionsComponent;
  let fixture: ComponentFixture<TagCloudConnectionsComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ TagCloudConnectionsComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(TagCloudConnectionsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
