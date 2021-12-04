import { Component, OnInit } from '@angular/core';
import { Validators, FormBuilder, FormControl } from '@angular/forms';
import { NgxSpinnerService } from 'ngx-spinner';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { Connection } from 'src/shared/models/connection/connection.model';
import { Player } from 'src/shared/models/player/player.model';
import * as THREE from 'three';
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls';

@Component({
  selector: 'app-get-network',
  templateUrl: './get-network.component.html',
  styleUrls: ['./get-network.component.css']
})
export class GetNetworkComponent implements OnInit {

  getNetworkForm = this.fb.group({
    scope: ['', [Validators.required, Validators.min(1), Validators.max(10)]]
  });

  email: string;
  success: any;
  playersIds: string[];
  players: Player[];
  network: Connection[];
  showForm: boolean = true;
  showGraph: boolean = false;

  constructor(
    private spinner: NgxSpinnerService,
    private cService: ConnectionService,
    private pService: PlayerService,
    private fb: FormBuilder) { 

      this.showForm = true;
      this.showGraph = false
    }

  ngOnInit(): void {
    this.network = [];
    this.playersIds = [];
    this.players = [];
    this.email = "jane@email.com";
  }

  getPlayersByScope(){
      this.spinner.show();

      /*this.cService.getNetwork(this.email, this.getNetworkForm.value.scope).subscribe({ next: data => {
        this.network = data;
      },
        error: _error => {
        }
      });*/

      this.mockGetNetwork();

      this.getPlayers();

      this.initializeGraph();

      this.spinner.hide();
  }

  mockGetNetwork(){
    let con1 = new Connection();
    con1.player = '115ac456-8a98-4e60-9966-02c38dd0ee64';
    con1.friend = 'a0164888-d6af-4fd5-ba01-ffce1d1cd0a0';
    con1.connectionStrength = 4;
    con1.tags = ['gaming', 'coding'];
    let con2 = new Connection();
    con2.player = 'a0164888-d6af-4fd5-ba01-ffce1d1cd0a0';
    con2.friend = '873277b7-6e1d-482f-9a71-e899ceaebfbd';
    con2.connectionStrength = 4;
    this.network = [con1, con2];
    con2.tags = ['music', 'coding'];
  }

  getPlayers(){
    for(let c of this.network){
      if(!this.playersIds.includes(c.player)){
        this.playersIds.push(c.player);
        this.pService.getPlayerById(c.player).subscribe({ next: data => {
          this.players.push(data);
        },
          error: _error => {
          }
        });
      }
      if(!this.playersIds.includes(c.friend)){
        this.playersIds.push(c.friend);
        this.pService.getPlayerById(c.friend).subscribe({ next: data => {
          this.players.push(data);
        },
          error: _error => {
          }
        });
      }
    }
  }

  getErrorMessageScopeRequired() {
    return this.getNetworkForm.controls['scope'].hasError('required') ? 'Scope is required' : '';
  }

  getErrorMessageScopeInvalid() {
    return (this.getNetworkForm.controls['scope'].hasError('min') || this.getNetworkForm.controls['scope'].hasError('max'))
      ? 'Scope should be between 1 and 10' : '';
  }

  clearSucess() {
    delete this.success;
  }

  get f() { return this.getNetworkForm.controls; }
  
  scene: THREE.Scene;
  renderer: THREE.WebGLRenderer;
  camera: THREE.PerspectiveCamera;
  controls: OrbitControls;

  initializeGraph(){
    this.showForm = false;
    this.showGraph = true;

    // Create a scene
    const scene = new THREE.Scene();
    scene.background = new THREE.Color(0xffffff);

    const renderer = new THREE.WebGLRenderer();
    renderer.setSize( window.innerWidth, window.innerHeight );
    document.body.appendChild( renderer.domElement );

    const camera = new THREE.PerspectiveCamera( 45, window.innerWidth / window.innerHeight, 1, 10000 );

    const controls = new OrbitControls( camera, renderer.domElement );

    camera.position.set( 0, 20, 100 );
    controls.update();

    let nodes = [], colors = [0xa93226, 0x884ea0, 0x5b2c6f, 0x2e86c1, 0x5dade2, 0x17a589, 0x27ae60, 
      0x0e6655, 0xf1c40f, 0x9c640c, 0xe67e22, 0x5d6d7e, 0xde3163, 0xff00ff];
    const geometry = new THREE.CircleGeometry(2, 32);

    for(let i=0; i<this.playersIds.length;i++){
      let maxH = 25, minH = -25, maxW = 25, minW = -25;
      let material = new THREE.MeshBasicMaterial( { color: colors[Math.floor(Math.random()*colors.length)]  } );
      let circle = new THREE.Mesh( geometry, material );
      if(i==0){
        circle.position.x = 0;
        circle.position.y = 0;
        circle.position.z = 0;
      }else{
        circle.position.x = Math.random() * (maxW - minW) + minW;
        circle.position.y = Math.random() * (maxH - minH) + minH;
        circle.position.z = 0;
      }
      scene.add(circle);
      nodes.push(circle);
    }

    for(let i=0; i<this.network.length;i++){
      let connections = [];
      for(let j=0; j<this.playersIds.length;j++){
        if(this.network[i].player == this.playersIds[j] || this.network[i].friend == this.playersIds[j] ){
          connections.push( new THREE.Vector3(nodes[j].position.x, nodes[j].position.y, -1) );
          if(connections.length >= 2){
            break;
          } 
        }
      }

      const materialConnections = new THREE.LineBasicMaterial( { color: 0x000 } );
      const geometryConnections = new THREE.BufferGeometry().setFromPoints( connections );
      
      const line = new THREE.Line( geometryConnections, materialConnections );

      scene.add( line );

    }

    renderer.render( scene, camera );    
    }

}
