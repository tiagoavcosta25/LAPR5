import { HttpClient, HttpErrorResponse, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { catchError, Observable, throwError } from 'rxjs';
import { environment } from 'src/environments/environment';
import { AcceptRequest } from 'src/shared/models/requests/accept-request.model';
import { ApproveRequest } from 'src/shared/models/requests/approve-request.model';
import { IntroductionRequest } from 'src/shared/models/requests/introduction-request.model';
import { CreatingDirectRequest } from 'src/shared/models/requests/creating-direct-request.model';
import { DirectRequest } from 'src/shared/models/requests/direct-request.model';
import { TargetPendingRequest } from 'src/shared/models/requests/target-pending-request.model';

@Injectable({
  providedIn: 'root'
})
export class RequestService {

  requestUrl = environment.apiUrl + '/connectionRequests/';

  httpOptions = {
    headers: new HttpHeaders({  'Content-Type' : 'application/json'}) 
  };

  constructor(private http: HttpClient) { }


  /** GET: returns requests from server */
  getRequests(email: string): Observable<TargetPendingRequest[]> {
    return this.http.get<TargetPendingRequest[]>(this.requestUrl + 'pendingRequests/' + email).pipe(
      catchError(this.handleError)
    );
  }

  /** PATCH: accept request */
  acceptRequest(id:string, request: AcceptRequest): Observable<AcceptRequest> {
    return this.http.patch<AcceptRequest>(this.requestUrl + 'pendingRequests/' + id + '/accept', request, this.httpOptions).pipe(
      catchError(this.handleError)
    );
  }

  /** PATCH: deny request */
  denyRequest(id:string): Observable<AcceptRequest> {
    return this.http.patch<AcceptRequest>(this.requestUrl + 'pendingRequests/' + id + '/deny', this.httpOptions).pipe(
      catchError(this.handleError)
    );
  }

  //** POST: adds direct request to servidor */
  sendDirectRequest(p: CreatingDirectRequest): Observable<DirectRequest> {
    return this.http.post<DirectRequest>(this.requestUrl + 'dir', p, this.httpOptions).pipe(
      catchError(this.handleError)
    );
  }

    /** GET: returns requests from server */
  getMiddleManRequests(email: string): Observable<IntroductionRequest[]> {
    return this.http.get<IntroductionRequest[]>(this.requestUrl + 'middleManRequests/' + email).pipe(
      catchError(this.handleError)
    );
  }

  /** PATCH: deny request */
  approveRequest(id:string, request: ApproveRequest): Observable<IntroductionRequest> {
    return this.http.patch<IntroductionRequest>(this.requestUrl + 'approve/' + id , request, this.httpOptions).pipe(
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
