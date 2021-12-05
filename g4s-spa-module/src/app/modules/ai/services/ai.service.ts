import { HttpClient, HttpErrorResponse, HttpHeaders, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { catchError, Observable, throwError } from 'rxjs';
import { environment } from 'src/environments/environment';

@Injectable({
  providedIn: 'root'
})
export class AiService {

  aiUrl = environment.aiUrl;

  httpOptions = {
    headers: new HttpHeaders({ 'Content-Type': 'application/json' })
  };

  constructor(private http: HttpClient) { }

  /** GET: returns connections from server */
  getStrongestRoute(emailPlayer: string, emailTarget: string): Observable<string[]> {
    const params = new HttpParams()
      .set('emailPlayer', emailPlayer)
      .set('emailTarget', emailTarget);
    const url = this.aiUrl + '/api/strongest-route';
    return this.http.get<string[]>(url, { params: params }).pipe(
      catchError(this.handleError)
    );
  }

  /** GET: returns connections from server */
  getshortestRoute(emailPlayer: string, emailTarget: string): Observable<string[]> {
    const params = new HttpParams()
      .set('emailPlayer', emailPlayer)
      .set('emailTarget', emailTarget);
    const url = this.aiUrl + '/api/shortest-route';
    return this.http.get<string[]>(url, { params: params }).pipe(
      catchError(this.handleError)
    );
  }

    /** GET: returns connections from server */
    getUsersWithXCommonTags(x: number): Observable<[[][]]> {
      const params = new HttpParams()
      .set('num', x);
      const url = this.aiUrl + '/api/common-tags';
      return this.http.get<[[][]]>(url, { params: params }).pipe(
        catchError(this.handleError)
      );
    }

  /** GET: returns safest route from server */
  getSafestRoute(emailPlayer: string, emailTarget: string, threshold: number): Observable<string[]> {
    const params = new HttpParams()
      .set('emailPlayer', emailPlayer)
      .set('emailTarget', emailTarget)
      .set('threshold', threshold);
    const url = this.aiUrl + '/api/safest-route';
    return this.http.get<string[]>(url, { params: params }).pipe(
      catchError(this.handleError)
    );
  }

  /** GET: searches for players from server */
  getSuggestedPlayers(emailPlayer: string, scope: number): Observable<string[]> {
    const params = new HttpParams()
    .set('emailPlayer', emailPlayer)
    .set('scope', scope);
    const url = this.aiUrl + '/api/safest-route';
    return this.http.get<string[]>(url, { params: params }).pipe(
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
