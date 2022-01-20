import { Component, OnDestroy, OnInit } from '@angular/core';
import { Post } from 'src/shared/models/feed/post.model';
import { NgxSpinnerService } from 'ngx-spinner';
import { FeedService } from '../../services/feed.service';
import { DobPlayer } from 'src/app/modules/player/models/dob-player.model copy';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { CreateComment } from 'src/shared/models/posts/create-comment.model';
import { PlayerLike } from 'src/shared/models/player/player-like.model';
import { CreatingPost } from '../../models/creating-post.model';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { firstValueFrom, forkJoin, Observable } from 'rxjs';

@Component({
  selector: 'app-get-feed',
  templateUrl: './get-feed.component.html',
  styleUrls: ['./get-feed.component.css']
})
export class GetFeedComponent implements OnInit, OnDestroy {

  obs: Observable<Post[]>[] = [];

  usersFinished: number = 0;

  posts: Post[];

  currentUserEmail: string;

  currentUser: DobPlayer;

  tags: string[];

  constructor(private fService: FeedService,
    private pService: PlayerService,
    public spinner: NgxSpinnerService,
    private cService: ConnectionService) { }

  ngOnDestroy(): void {
    this.obs = [];
  }

  async ngOnInit(): Promise<void> {
    this.tags = [];
    this.currentUserEmail = localStorage.getItem("currentPlayer")!;
    this.getFriendsPosts();
    this.getLoggedPlayer();
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

  getFriendsPosts(): void {
    this.spinner.show();
    var connections = [];
    this.cService.getConnections(this.currentUserEmail).subscribe({ next: async data => {
      connections = data;
      for(let con of connections) {
        const ob = this.fService.getPostsByUser(con.friend.email);
        this.obs.push(ob);
      }
      let ob = this.fService.getPostsByUser(this.currentUserEmail);
      this.obs.push(ob);
      forkJoin(this.obs).subscribe(results => {
        let tempPosts = [];
        for(let pList of results) {
          for(let p of pList) {
            tempPosts.push(p);
          }
        }
        tempPosts.sort((b,a) => (a.createdAt > b.createdAt) ? 1 : ((b.createdAt > a.createdAt) ? -1 : 0));
        this.posts = tempPosts;    
        this.spinner.hide();
      })
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
    let createComment: CreateComment = new CreateComment(post.id, this.currentUserEmail, this.currentUser.name, val);
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
    let like: PlayerLike = new PlayerLike(post.id, this.currentUserEmail);
    let likedPost: Post;
    if(post.likes.some(x => x == this.currentUserEmail)) {
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
    let dislike: PlayerLike = new PlayerLike(post.id, this.currentUserEmail);
    let dislikedPost: Post;
    if(post.dislikes.some(x => x == this.currentUserEmail)) {
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
      if(l == this.currentUserEmail) {
        return true;
      }
    }
    return false;
  }

  checkIfDisliked(post: Post): boolean {
    for(let d of post.dislikes) {
      if(d == this.currentUserEmail) {
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
    createPost.creatorId = this.currentUserEmail;
    createPost.name = this.currentUser.name;
    createPost.tags = this.tags;
    this.fService.createPost(createPost).subscribe({ next: _data => {
      this.ngOnDestroy();
      this.ngOnInit();
      this.spinner.hide();
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
}
