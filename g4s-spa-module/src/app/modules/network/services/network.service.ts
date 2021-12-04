import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders, HttpErrorResponse, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import { catchError } from 'rxjs/operators';
import { throwError } from 'rxjs';
import { environment } from 'src/environments/environment';
import { Player } from 'src/shared/models/player/player.model';

@Injectable({
  providedIn: 'root'
})
export class NetworkService {

  networkUrl: string = environment.aiUrl; // URL to web api
  
  httpOptions = {
    headers: new HttpHeaders({ 'Content-Type': 'application/json' })
  };

  constructor(
    private http: HttpClient) {
  }

  /** GET: searches for players from server */
  getSafestRoute(emailPlayer: string, emailTarget: string, threshold: number): Observable<string[]> {
    const params = new HttpParams()
    .set('emailPlayer', emailPlayer)
    .set('emailTarget', emailTarget)
    .set('threshold', threshold);
    return this.http.get<string[]>(this.networkUrl + '/safest-route/', { params: params }).pipe(
      catchError(this.handleError)
    );
  }

  /** GET: searches for players from server */
  getSuggestedPlayers(emailPlayer: string, scope: number): Observable<string[]> {
    const params = new HttpParams()
    .set('emailPlayer', emailPlayer)
    .set('scope', scope);
    return this.http.get<string[]>(this.networkUrl, { params: params }).pipe(
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
