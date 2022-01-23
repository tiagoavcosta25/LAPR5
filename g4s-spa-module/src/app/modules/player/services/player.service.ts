import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders, HttpErrorResponse, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import { catchError } from 'rxjs/operators';
import { throwError } from 'rxjs';

import { environment } from 'src/environments/environment';
import { Player } from 'src/shared/models/player/player.model';
import { CreatingPlayer } from '../models/creating-player.model';
import { ChangeEmotionalStatus } from 'src/shared/models/player/change-emotional-status.model';
import { DobPlayer } from '../models/dob-player.model copy';

@Injectable({
  providedIn: 'root'
})
export class PlayerService {

  playerUrl: string = environment.apiUrl + '/players/';  // URL to web api

  httpOptions = {
    headers: new HttpHeaders({ 'Content-Type': 'application/json' })
  };

  constructor(
    private http: HttpClient) {
  }

  /** GET players from the server */
  getPlayerNumber(): Observable<number> {
    return this.http.get<number>(this.playerUrl + "number");
  }

  /** Login */
  login(playerEmail: string, playerPassword: string): Observable<number> {
    const params = new HttpParams()
    .set('playerEmail', playerEmail)
    .set('playerPassword', playerPassword);
    return this.http.get<number>(this.playerUrl + "login", { params: params }).pipe(
      catchError(this.handleError)
    );
  }

  /** GET players from the server */
  getPlayers(): Observable<Player[]> {
    return this.http.get<Player[]>(this.playerUrl);
  }

  /* GET players by email */
  getOnlyPlayerByEmail(email: string): Observable<DobPlayer> {
    return this.http.get<DobPlayer>(this.playerUrl + "email/" + email).pipe(
    catchError(this.handleError)
    );
  }

  /** POST: add a new player to the database */
  registerPlayer(player: CreatingPlayer): Observable<Player> {
    return this.http.post<Player>(this.playerUrl, player, this.httpOptions).pipe(
      catchError(this.handleError)
    );
  }

  /** DELETE: deactivate the player from the server */
  deactivatePlayer(id: string): Observable<unknown> {
    return this.http.delete(this.playerUrl + id, this.httpOptions);
  }

  /** DELETE: delete the player from the server */
  deletePlayer(id: string): Observable<unknown> {
    return this.http.delete(this.playerUrl + id + "/hard", this.httpOptions);
  }

  /** GET: get player by id */
  getPlayerById(id: string): Observable<Player> {
    return this.http.get<Player>(this.playerUrl + id).pipe(
      catchError(this.handleError)
    );
  }

  /** GET: searches for players from server */
  searchPlayers(filter: string, value: string): Observable<Player[]> {
    const params = new HttpParams()
    .set('filter', filter)
    .set('value', value);
    return this.http.get<Player[]>(this.playerUrl + 'search', { params: params }).pipe(
      catchError(this.handleError)
    );
  }


  /** POST: update data's player to the database */
  updatePlayer(player: DobPlayer): Observable<DobPlayer> {
    console.log(player);
    return this.http.put<DobPlayer>(this.playerUrl, player, this.httpOptions).pipe(
      catchError(this.handleError)
    );
  }

  /** PATCH: updates emotional status */
  updateEmotionalStatus(email:string, emotionalStatus: ChangeEmotionalStatus): Observable<ChangeEmotionalStatus> {
    return this.http.patch<ChangeEmotionalStatus>(this.playerUrl + "emotionalStatus/" + email , emotionalStatus, this.httpOptions).pipe(
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
