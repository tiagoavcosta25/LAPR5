import { Component, OnInit } from '@angular/core';
import { Post } from 'src/shared/models/feed/post.model';
import { FormArray, FormBuilder, Validators } from '@angular/forms';
import { NgxSpinnerService } from 'ngx-spinner';
import { Location } from '@angular/common';
import { FeedService } from '../../services/feed.service';

@Component({
  selector: 'app-get-feed',
  templateUrl: './get-feed.component.html',
  styleUrls: ['./get-feed.component.css']
})
export class GetFeedComponent implements OnInit {

  constructor(private service: FeedService,
    private spinner: NgxSpinnerService,
    private location: Location,
    private fb: FormBuilder) { }

  feed: Post[];

  post: Post;

  error: boolean;

  success?: boolean;

  successMessage: string;
  
  successMessageLike: string = "Post liked sucessfully!";

  successMessageDislike: string = "Post disliked sucessfully!";

  successMessageComment: string = "Post commented sucessfully!";
  
  errorMessage: string = "There was an error with reaction!";

  step: number;

  postForm = this.fb.group({
  })

  ngOnInit(): void {
    this.feed = [];
    this.post = new Post();
    this.post.content = "Hello friends, just wanted to thank you guys for this wonderful year. Happy New Year to everyone!";
    this.post.creatorId = "User1";
    this.post.date = "31/12/2021";
    console.log(this.post);
    this.feed.push(this.post);
    let p2 = new Post();
    p2.content = "Just saw the new Spiderman movie at my New Year's Eve Party, it was awesome! #spoilers";
    p2.creatorId = "John";
    p2.date = "02/01/2022";
    this.feed.push(p2);
    let p3 = new Post();
    p3.content = "I just got my first friend on here, many more to go! #g4s";
    p3.creatorId = "Jane";
    p3.date = "11/11/2021";
    this.feed.push(p3);
  }

  setStep(index: number) {
    this.step = index;
  }

  get f() { return this.postForm.controls; }

  like(): void {
    this.spinner.show();

    this.success = true;
    this.successMessage = this.successMessageLike;
    this.postForm.reset();
    this.spinner.hide();
  }

  dislike(): void {
    this.spinner.show();

    this.success = true;
    this.successMessage = this.successMessageDislike;
    this.postForm.reset();
    this.spinner.hide();
    
  }

  comment(): void {
    this.spinner.show();

    this.success = true;
    this.successMessage = this.successMessageComment;
    this.postForm.reset();
    this.spinner.hide();
    
  }

  timeLeft: number = 2;
  interval: any;

  refreshOnTime() {
    this.interval = setInterval(() => {
      if(this.timeLeft > 0) {
        this.timeLeft--;
      } else {
        this.refresh();
      }
    },1000)
  }

  clearSuccess() {
    delete this.success;
  }

  goBack(): void {
    this.location.back();
  }

  refresh(): void {
    window.location.reload();
  }

}
