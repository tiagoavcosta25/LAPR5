import { HttpClient, HttpErrorResponse, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { catchError, Observable, throwError } from 'rxjs';
import { environment } from 'src/environments/environment';
import { Connection } from 'src/shared/models/connection/connection.model';
import { GettingConnection } from 'src/shared/models/connection/getting-connection.model';
import { UpdatingConnection } from 'src/shared/models/connection/updating-connection.model';


@Injectable({
  providedIn: 'root'
})
export class ConnectionService {

  connectionUrl = environment.apiUrl + '/connections/';
  
  httpOptions = {
    headers: new HttpHeaders({ 'Content-Type': 'application/json' })
  };

  constructor(private http: HttpClient) { }


  /** GET: returns connections from server */
  getConnections(email: string): Observable<GettingConnection[]> {
    return this.http.get<GettingConnection[]>(this.connectionUrl + 'user/' + email).pipe(
      catchError(this.handleError)
    );
  }

  /** PATCH: updates connection */
  updateConnection(id:string, connection: UpdatingConnection): Observable<Connection> {
    return this.http.patch<Connection>(this.connectionUrl + id , connection, this.httpOptions).pipe(
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