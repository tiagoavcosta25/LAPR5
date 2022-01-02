import { Component, OnInit } from '@angular/core';
import { DebugElement } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FeedService } from '../../services/feed.service';
import { of } from 'rxjs';
import { Player } from 'src/shared/models/player/player.model';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { FormArray, FormBuilder, ReactiveFormsModule, Validators } from '@angular/forms';
import { By } from '@angular/platform-browser';
import { NgxSpinnerService } from 'ngx-spinner';

@Component({
  selector: 'app-create-post',
  templateUrl: './create-post.component.html',
  styleUrls: ['./create-post.component.css']
})
export class CreatePostComponent implements OnInit {

  success: any;
  successMessage: string = "Post created sucessfully!";
  errorMessage: string = "There was an error creating your post!";

  postForm = this.fb.group({
    content: ['', [Validators.required, Validators.min(1), Validators.max(255)]]
  });

  constructor(
              private spinner: NgxSpinnerService,
              private service: FeedService,
              private fb: FormBuilder) { }

  ngOnInit(): void {
  }

  createPost() {
    //
  }
  
  getErrorMessageContentRequired() {
    return this.postForm.controls['content'].hasError('required') ? 'Content is required' : '';
  }

  getErrorMessageContentInvalid() {
    return (this.postForm.controls['content'].hasError('min') || this.postForm.controls['content'].hasError('max'))
      ? 'Content should be between 1 and 255 characters' : '';
  }

  save(): void {
    this.createPost();
    this.spinner.show();

    this.success = true;
    this.postForm.reset();
    this.spinner.hide();
  }

  clearSucess() {
    delete this.success;
  }

  get f() { return this.postForm.controls; }

}
