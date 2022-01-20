import { HttpClient, HttpErrorResponse, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { catchError, Observable, throwError } from 'rxjs';
import { environment } from 'src/environments/environment';
import { Dcalc } from 'src/shared/models/feed/dcalc.dto';
import { Post } from 'src/shared/models/feed/post.model';
import { PlayerLike } from 'src/shared/models/player/player-like.model';
import { CreateComment } from 'src/shared/models/posts/create-comment.model';
import { CreatingPost } from '../models/creating-post.model';

@Injectable({
  providedIn: 'root'
})
export class FeedService {

  postUrl: string = environment.feedUrl + '/post/';  // URL to web api

  httpOptions = {
    headers: new HttpHeaders({ 'Content-Type': 'application/json' })
  };

  constructor(
    private http: HttpClient) {
  }

  /** POST: create new post to the database */
  createPost(post: CreatingPost): Observable<Post> {
    return this.http.post<Post>(this.postUrl, post, this.httpOptions).pipe(
      catchError(this.handleError)
    );
  }

  /* GET players by email */
  getPostsByUser(email: string): Observable<Post[]> {
    return this.http.get<Post[]>(this.postUrl + email).pipe(
      catchError(this.handleError)
    );
  }

  /** PATCH: comment exising post in the database */
  commentPost(comment: CreateComment): Observable<Post> {
    return this.http.patch<Post>(this.postUrl + "addcomment", comment, this.httpOptions).pipe(
      catchError(this.handleError)
    );
  }

  /** PATCH: like exising post in the database */
  likePost(like: PlayerLike): Observable<Post> {
    return this.http.patch<Post>(this.postUrl + "like", like, this.httpOptions).pipe(
      catchError(this.handleError)
    );
  }

  /** PATCH: unlike exising post in the database */
  unlikePost(like: PlayerLike): Observable<Post> {
    return this.http.patch<Post>(this.postUrl + "unlike", like, this.httpOptions).pipe(
      catchError(this.handleError)
    );
  }

  /** PATCH: dislike exising post in the database */
  dislikePost(dislike: PlayerLike): Observable<Post> {
    return this.http.patch<Post>(this.postUrl + "dislike", dislike, this.httpOptions).pipe(
      catchError(this.handleError)
    );
  }

  /** PATCH: undislike exising post in the database */
  undislikePost(like: PlayerLike): Observable<Post> {
    return this.http.patch<Post>(this.postUrl + "undislike", like, this.httpOptions).pipe(
      catchError(this.handleError)
    );
  }

  /* GET: calculate D value */
  dCalc(emailA: string, emailB: string): Observable<Dcalc> {
    return this.http.get<Dcalc>(this.postUrl + "dcalc/" + emailA + "/" + emailB).pipe(
      catchError(this.handleError)
    );
  }

  private handleError(err: HttpErrorResponse) {
    console.error('An error occurred: ', err.error.errors.message);
    if (err.error instanceof ErrorEvent) {
      console.error('An error occurred: ', err.error.message);
    }
    else {
      console.error(
        `Web Api returned code ${err.status}, ` + ` Response body was: ${err.error}`
      );
    }
    return throwError(() => err);
  }
}
