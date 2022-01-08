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
import { Post } from 'src/shared/models/feed/post.model';
import { CreatingPost } from '../../models/creating-post.model';
import { emotionalStatusEnum } from 'src/shared/models/player/emotional-status-enum.model';
import { PlayerService } from 'src/app/modules/player/services/player.service';

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
    content: ['', [Validators.required, Validators.min(1), Validators.max(255)]],
    tags: this.fb.array([])
  });

  p: CreatingPost;

  email: string;

  constructor(
              private spinner: NgxSpinnerService,
              private service: FeedService,
              private pService: PlayerService,
              private fb: FormBuilder) { }

  ngOnInit(): void {
    this.p = new CreatingPost();
    this.email = localStorage.getItem('currentPlayer')!.trim();
  }

  createPost() {
    this.p.content = this.postForm.value.content;
    for(let tag of this.postForm.value.tags)
    {
      if(!this.p.tags.includes(tag.tag))
        this.p.tags.push(tag.tag);
    }
  }

  addTag(){
    let tags = this.postForm.get('tags') as FormArray;
    tags.push(this.fb.group({
      tag: ['', Validators.required]
    }));
  }

  removeTag(value: string) {
    let tags = this.postForm.get('tags') as FormArray;
    tags.removeAt(tags.value.findIndex((tag: { tag: string; }) => tag.tag === value))
  }

  get tagsFormGroups() {
    return this.postForm.get('tags') as FormArray;
  }
  
  getErrorMessageContentRequired() {
    return this.postForm.controls['content'].hasError('required') ? 'Content is required' : '';
  }

  getErrorMessageContentInvalid() {
    return (this.postForm.controls['content'].hasError('min') || this.postForm.controls['content'].hasError('max'))
      ? 'Content should be between 1 and 255 characters' : '';
  }

  getErrorMessageTagRequired() {
    return 'Created tag name is required';
  }

  save(): void {
    this.createPost();
    this.spinner.show();

    this.pService.getOnlyPlayerByEmail(this.email)
    .subscribe({ next: player => {
      if(player) {
        this.p.creatorId = player.id;
        this.service.createPost(this.p)
        .subscribe({ next: data => {
          if(data) {
            this.success = true;
            let tags = this.postForm.get('tags') as FormArray;
            for(let tagT of this.p.tags) {
              tags.removeAt(tags.value.findIndex((tag: { tag: string; }) => tag.tag === tagT))
            }
            this.postForm.reset();
            this.p = new CreatingPost();
          }
          this.spinner.hide();
        },
          error: _error => {
            this.success = false;
            this.p = new CreatingPost();
            this.spinner.hide();
          }
        });
      }
      this.spinner.hide();
    },
      error: _error => {
        this.success = false;
        this.p = new CreatingPost();
        this.spinner.hide();
      }
    });
  }

  clearSucess() {
    delete this.success;
  }

  get f() { return this.postForm.controls; }

  get emotions() : string[] {
    var emots = [];
    for(const emotion in emotionalStatusEnum) {
      if (isNaN(Number(emotion))){
        emots.push(emotion.toString());
        console.log(emotion.toString());
      }
    }
    return emots;
  }

}
