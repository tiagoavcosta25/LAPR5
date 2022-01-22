import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { NgxSpinnerService } from 'ngx-spinner';
import { CreatingPost } from 'src/app/modules/feed/models/creating-post.model';
import { FeedService } from 'src/app/modules/feed/services/feed.service';
import { Post } from 'src/shared/models/feed/post.model';
import { PlayerLike } from 'src/shared/models/player/player-like.model';
import { CreateComment } from 'src/shared/models/posts/create-comment.model';
import { DobPlayer } from '../../../models/dob-player.model copy';
import { PlayerService } from '../../../services/player.service';

@Component({
  selector: 'app-profile-timeline',
  templateUrl: './profile-timeline.component.html',
  styleUrls: ['./profile-timeline.component.css']
})
export class ProfileTimelineComponent implements OnInit {

  userEmail: string;
  
  currentPlayer: DobPlayer;

  posts: Post[];

  currentUserEmail: string;

  currentUser: DobPlayer;

  tags: string[];

  ownProf: boolean;

  constructor(private activatedRoute: ActivatedRoute,
    private fService: FeedService,
    private pService: PlayerService,
    private spinner: NgxSpinnerService) { }

  ngOnInit(): void {
    this.tags = [];
    this.currentUserEmail = localStorage.getItem("currentPlayer")!;
    this.getLoggedPlayer();
    this.activatedRoute.params.subscribe(params => {
      this.userEmail = params['email'];
      this.getCurrentPlayer();
      this.checkIfOwnProfile();
    })
  }

  getCurrentPlayer(): void {
    this.spinner.show();
    this.pService.getOnlyPlayerByEmail(this.userEmail).subscribe({ next: data => {
      this.currentPlayer = data;
      this.getPostsByUser();
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
      }
    });
  }

  getLoggedPlayer(): void {
    this.spinner.show();
    this.pService.getOnlyPlayerByEmail(this.currentUserEmail).subscribe({ next: data => {
      this.currentUser = data;
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
      }
    });
  }

  getPostsByUser(): void {
    this.spinner.show();
    this.fService.getPostsByUser(this.currentPlayer.id).subscribe({ next: data => {
      this.posts = data;
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
      }
    });
  }

  getDate(date: Date): string {
    var today = new Date();
    let dateF = new Date(date);
    if(today.getFullYear() === dateF.getFullYear()) {
      if(today.getMonth() === dateF.getMonth()) {
        if(today.getDate() === dateF.getDate()) {
          return "Today";
        } else if(today.getDate() == dateF.getDate() + 1) {
          return "Yesterday";
        } else {
          return dateF.getDate() + "/" + (dateF.getMonth() + 1).toString().padStart(2, "0");
        }
      } else {
        return dateF.getDate() + "/" + (dateF.getMonth() + 1).toString().padStart(2, "0");
      }
    } else {
      return dateF.getDate() + "/" + (dateF.getMonth() + 1).toString().padStart(2, "0") + "/" + dateF.getFullYear();
    }
  }

  getTime(date: Date): string {
    var today = new Date();
    let dateF = new Date(date);
    if(today.getFullYear() === dateF.getFullYear()) {
      if(today.getMonth() === dateF.getMonth()) {
        if(today.getDate() === dateF.getDate()) {
          if((today.getTime() - dateF.getTime()) / 60000 / 60 <= 1) {
            if((today.getTime() - dateF.getTime()) / 60000 <= 1) {
              return "Just now";
            } else if((today.getTime() - dateF.getTime()) / 60000 <= 15) {
              return "A few minutes ago";
            } else {
              return "One hour ago";
            }
          }
        }
      }
    }
    return dateF.getHours().toString().padStart(2, "0") + ":" + dateF.getMinutes().toString().padStart(2, "0");;
  }

  scroll(name: string) {
    let el = document.getElementById(name);
    let headerOffset = 120;
    let elementPosition = el?.getBoundingClientRect().top;
    if(elementPosition != undefined) {
      let offsetPosition = elementPosition + window.scrollY - headerOffset;
      window.scrollTo({
        top:  offsetPosition,
        behavior: "smooth"
      })
    }
  }

  comment(name: string, post: Post): void {
    let el = document.getElementById(name);
    let val = "";
    if(el != undefined) {
      val = ((<HTMLInputElement>el).value);
      (<HTMLInputElement>el).value = "";
    }
    if(val == "") {
      return;
    }
    this.spinner.show();
    let createComment: CreateComment = new CreateComment(post.id, this.currentUserEmail, this.currentUser.avatar, this.currentUser.name, val);
    let commentedPost: Post;
    this.fService.commentPost(createComment).subscribe({ next: data => {
      commentedPost = data;
      for(let i = 0; i < this.posts.length; i++) {
        if(this.posts[i].id == commentedPost.id) {
          this.posts[i] = commentedPost;
        }
      }
      this.spinner.hide();
    },
      error: _error => {
        this.spinner.hide();
      }
    });
  }

  likePost(post: Post): void {
    let like: PlayerLike = new PlayerLike(post.id, this.currentUser.id);
    let likedPost: Post;
    if(post.likes.some(x => x == this.currentUser.id)) {
      this.fService.unlikePost(like).subscribe({ next: data => {
        likedPost = data;
        for(let i = 0; i < this.posts.length; i++) {
          if(this.posts[i].id == likedPost.id) {
            this.posts[i] = likedPost;
          }
        }
      },
        error: _error => {
        }
      });
    } else {
      this.fService.likePost(like).subscribe({ next: data => {
        likedPost = data;
        for(let i = 0; i < this.posts.length; i++) {
          if(this.posts[i].id == likedPost.id) {
            this.posts[i] = likedPost;
          }
        }
      },
        error: _error => {
        }
      });
    }
  }

  dislikePost(post: Post): void {
    let dislike: PlayerLike = new PlayerLike(post.id, this.currentUser.id);
    let dislikedPost: Post;
    if(post.dislikes.some(x => x == this.currentUser.id)) {
      this.fService.undislikePost(dislike).subscribe({ next: data => {
        dislikedPost = data;
        for(let i = 0; i < this.posts.length; i++) {
          if(this.posts[i].id == dislikedPost.id) {
            this.posts[i] = dislikedPost;
          }
        }
      },
        error: _error => {
        }
      });
    } else {
      this.fService.dislikePost(dislike).subscribe({ next: data => {
        dislikedPost = data;
        for(let i = 0; i < this.posts.length; i++) {
          if(this.posts[i].id == dislikedPost.id) {
            this.posts[i] = dislikedPost;
          }
        }
      },
        error: _error => {
        }
      });
    }
  }

  getLikeCount(post: Post): string {
    return this.generalCounter(post.likes.length);
  }

  getDislikeCount(post: Post): string {
    return this.generalCounter(post.dislikes.length);
  }

  getCommentCount(post:Post): string {
    return this.generalCounter(post.comments.length);
  }

  generalCounter(n: number): string {
    if(n > 1000000) {
      return ((n) / 1000000).toFixed(0).toString() + "M";
    } else if (n > 1000) {
      return ((n) / 1000).toFixed(1).toString() + "K";
    }
    return (n).toString();
  }

  checkIfLiked(post: Post): boolean {
    for(let l of post.likes) {
      if(l == this.currentUser.id) {
        return true;
      }
    }
    return false;
  }

  checkIfDisliked(post: Post): boolean {
    for(let d of post.dislikes) {
      if(d == this.currentUser.id) {
        return true;
      }
    }
    return false;
  }

  post(): void {
    this.spinner.show();
    let el = document.getElementById('shareinput');
    let val = "";
    if(el != undefined) {
      val = ((<HTMLInputElement>el).value);
      (<HTMLInputElement>el).value = "";
    }
    let createPost: CreatingPost = new CreatingPost();
    createPost.content = val;
    createPost.creatorId = this.currentUser.id;
    createPost.creatorEmail = this.currentUserEmail;
    createPost.avatar = this.currentUser.avatar;
    createPost.name = this.currentUser.name;
    createPost.tags = this.tags;
    this.fService.createPost(createPost).subscribe({ next: _data => {
      this.spinner.hide();
      this.ngOnInit();
    },
      error: _error => {
        this.spinner.hide();
      }
    });
  }

  addTag() {
    let el = document.getElementById('tagvalue');
    let val = "";
    if(el != undefined) {
      val = ((<HTMLInputElement>el).value);
      (<HTMLInputElement>el).value = "";
    }
    if(this.tags.indexOf(val) < 0) {
      this.tags.push(val);
    }
  }

  removeTag(tag: string) {
    let index = this.tags.findIndex( t => t == tag);
    if (index != -1) {
      this.tags.splice(index, 1);
    }
  }

  checkIfOwnProfile() {
    this.ownProf = this.userEmail == this.currentUserEmail;
  }
}
