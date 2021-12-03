import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders, HttpErrorResponse, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import { catchError } from 'rxjs/operators';
import { throwError } from 'rxjs';

import { environment } from 'src/environments/environment';
import { Player } from 'src/shared/models/player/player.model';
import { CreatingPlayer } from '../models/creating-player.model';

@Injectable({
  providedIn: 'root'
})
export class PlayerService {

  playerUrl: string = environment.apiUrl + '/players/';  // URL to web api

  connectionUrl = environment.apiUrl + '/players/';
  
  httpOptions = {
    headers: new HttpHeaders({ 'Content-Type': 'application/json' })
  };

  constructor(
    private http: HttpClient) {
  }

  /** GET players from the server */
  getPlayers(): Observable<Player[]> {
    return this.http.get<Player[]>(this.playerUrl);
  }

  /* GET players by email */
  getPlayerByEmail(email: string): Observable<Player[]> {
    email = email.trim();

    // Add safe, URL encoded search parameter if there is a search term
    const options = email ?
     { params: new HttpParams().set('email', email) } : {};
    return this.http.get<Player[]>(this.playerUrl).pipe(
      catchError(this.handleError)
    );
  }

  /** POST: add a new player to the database */
  registerPlayer(player: CreatingPlayer): Observable<Player> {
    return this.http.post<Player>(this.playerUrl, player, this.httpOptions).pipe(
      catchError(this.handleError)
    );
  }

  /** DELETE: delete the player from the server */
  deletePlayer(id: number): Observable<unknown> {
    const url = `${this.playerUrl}/${id}`; // DELETE api/players/42
    return this.http.delete(url, this.httpOptions);
  }

  /** GET: get player by id */
  getPlayerById(id: string): Observable<Player> {
    return this.http.get<Player>(this.connectionUrl + id).pipe(
      catchError(this.handleError)
    );
  }

  /** GET: searches for players from server */
  searchPlayers(filter: string, value: string): Observable<Player[]> {
    const params = new HttpParams()
    .set('filter', filter)
    .set('value', value);
    return this.http.get<Player[]>(this.connectionUrl + 'search', { params: params }).pipe(
      catchError(this.handleError)
    );
  }


  /** POST: update data's player to the database */
  updatePlayer(player: Player): Observable<Player> {
    return this.http.put<Player>(this.playerUrl, player, this.httpOptions).pipe(
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
