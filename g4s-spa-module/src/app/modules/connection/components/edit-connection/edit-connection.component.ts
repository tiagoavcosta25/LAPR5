import { Component, OnInit } from '@angular/core';
import { NgxSpinnerService } from 'ngx-spinner';
import { GettingConnection } from 'src/shared/models/connection/getting-connection.model';
import { ConnectionService } from '../../services/connection.service';
import { Location } from '@angular/common';
import { FormArray, FormBuilder, Validators } from '@angular/forms';
import { UpdatingConnection } from 'src/shared/models/connection/updating-connection.model';
import { Connection } from 'src/shared/models/connection/connection.model';


@Component({
  selector: 'app-edit-connection',
  templateUrl: './edit-connection.component.html',
  styleUrls: ['./edit-connection.component.css']
})
export class EditConnectionComponent implements OnInit {

  error: boolean;

  success?: boolean;
  
  successMessage: string = "Connection updated sucessfully!";
  
  errorMessage: string = "There was an error updating the connection!";

  step: number;

  connections: GettingConnection[];

  chosenConnection: GettingConnection;

  connectionIdSelected: string;

  c: UpdatingConnection;

  connectionForm = this.fb.group({
    connectionStrength: ['', [Validators.required, Validators.min(1), Validators.max(100)]],
    tags: this.fb.array([])
  })

  constructor(private cService: ConnectionService,
    private spinner: NgxSpinnerService,
    private location: Location,
    private fb: FormBuilder) { }

    ngOnInit(): void {
      this.c = new UpdatingConnection;
      this.getConnections('email1@gmail.com');
    }

    setStep(index: number) {
      this.step = index;
    }

    loadForm(id: string): void {
      this.connectionIdSelected = id;
      this.getConnectionById();
      this.connectionForm.reset();
      let tags = this.connectionForm.get('tags') as FormArray;
      tags.clear();
      this.connectionForm.get('connectionStrength')?.setValue(this.chosenConnection.connectionStrength);
      for (let tag of this.chosenConnection.tags) {
        this.addTagWithValue(tag);
      }      
    }
  
    getConnectionById(): void {
      for(let connection of this.connections) {
        if (connection.id == this.connectionIdSelected) {
          this.chosenConnection = connection;
        }
      }
    }

    get f() { return this.connectionForm.controls; }

    get tagsFormGroups() {
      return this.connectionForm.get('tags') as FormArray;
    }

    addTag(){
      let tags = this.connectionForm.get('tags') as FormArray;
      tags.push(this.fb.group({
        tag: ['', Validators.required]
      }));
    }

    addTagWithValue(value: string){
      let tags = this.connectionForm.get('tags') as FormArray;
      tags.push(this.fb.group({
        tag: [value, Validators.required]
      }));
    }

    removeTag(value: string) {
      let tags = this.connectionForm.get('tags') as FormArray;
      tags.removeAt(tags.value.findIndex((tag: { tag: string; }) => tag.tag === value))
    }

    createUpdatedConnection() {
      this.c.id = this.connectionIdSelected;
      this.c.connectionStrength = this.connectionForm.value.connectionStrength;
      this.c.tags = [];
      for(let tag of this.connectionForm.value.tags)
      {
        if(!this.c.tags.includes(tag.tag))
          this.c.tags.push(tag.tag);
      }
    }

    getConnections(email: string): void {
      this.spinner.show();
      this.cService.getConnections(email).subscribe({ next: data => {
        this.connections = data;
        this.spinner.hide();
      },
        error: _error => {
          this.spinner.hide();
          this.error = true;
        }
      });
    }

    save(): void {
      this.createUpdatedConnection();
      this.spinner.show();
      this.cService.updateConnection(this.connectionIdSelected, this.c)
      .subscribe({ next: data => {
        if(data) {
          this.success = true;
          this.c = new Connection;
        }
        this.spinner.hide();
      },
        error: _error => {
          this.success = false;
          this.c = new Connection;
          this.spinner.hide();
        }
      });
    }

    getErrorMessageConnectionStrengthRequired() {
      return this.connectionForm.controls['connectionStrength'].hasError('required') ? 'ConnectionStrength is required' : '';
    }

    getErrorMessageConnectionStrengthInvalid() {
      return (this.connectionForm.controls['connectionStrength'].hasError('min') || this.connectionForm.controls['connectionStrength'].hasError('max'))
        ? 'Connection Strength should be between 1 and 100' : '';
    }

    getErrorMessageTagRequired() {
      return 'Tag name is required';
    }

    clearSuccess() {
      delete this.success;
    }

    goBack(): void {
      this.location.back();
    }

    refresh(): void {
      window.location.reload();
    }

}
