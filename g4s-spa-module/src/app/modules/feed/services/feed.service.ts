import { HttpClient, HttpErrorResponse, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { catchError, Observable, throwError } from 'rxjs';
import { environment } from 'src/environments/environment';
import { Post } from 'src/shared/models/feed/post.model';
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
    console.log(this.postUrl);
    console.log(post);
    return this.http.post<Post>(this.postUrl, post, this.httpOptions).pipe(
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
