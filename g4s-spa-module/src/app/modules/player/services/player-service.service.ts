import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { HttpHeaders } from '@angular/common/http';

import { Observable } from 'rxjs';
import { Player } from 'src/shared/models/player.model';

const httpOptions = {
  headers: new HttpHeaders({
    'Content-Type':  'application/json'
  })
};

@Injectable({
  providedIn: 'root'
})
export class PlayerServiceService {

  playersUrl = 'https://socialnetworkapi51.azurewebsites.net/api/players';  // URL to web api

  constructor(
    private http: HttpClient) {
  }

  /** GET players from the server */
  getPlayers(): Observable<Player[]> {
    return this.http.get<Player[]>(this.playersUrl);
  }

  /* GET players by email */
  getPlayerByEmail(email: string): Observable<Player[]> {
    email = email.trim();

    // Add safe, URL encoded search parameter if there is a search term
    const options = email ?
     { params: new HttpParams().set('email', email) } : {};

    return this.http.get<Player[]>(this.playersUrl, options);
  }

  /** POST: add a new player to the database */
  addPlayer(player: Player): Observable<Player> {
    return this.http.post<Player>(this.playersUrl, player, httpOptions);
  }

  /** DELETE: delete the player from the server */
  deletePlayer(id: number): Observable<unknown> {
    const url = `${this.playersUrl}/${id}`; // DELETE api/players/42
    return this.http.delete(url, httpOptions);
  }
}
