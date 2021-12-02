import { ComponentFixture, TestBed } from '@angular/core/testing';

import { GetNetworkComponent } from './get-network.component';

describe('GetNetworkComponent', () => {
  let component: GetNetworkComponent;
  let fixture: ComponentFixture<GetNetworkComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ GetNetworkComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(GetNetworkComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
