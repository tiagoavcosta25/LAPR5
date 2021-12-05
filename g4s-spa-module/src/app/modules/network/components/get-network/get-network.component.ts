import { Component, OnInit } from '@angular/core';
import { Validators, FormBuilder, FormControl } from '@angular/forms';
import { NgxSpinnerService } from 'ngx-spinner';
import { observable, Observable } from 'rxjs';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { Connection } from 'src/shared/models/connection/connection.model';
import { Player } from 'src/shared/models/player/player.model';
import * as THREE from 'three';
import { Group } from 'three';
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls';
import { CSS2DRenderer, CSS2DObject } from 'three/examples/jsm/renderers/CSS2DRenderer.js';

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
      this.showGraph = false;
      this.players = [];
      this.playersIds = [];
      this.network = [];
    }

  ngOnInit(): void {
    this.network = [];
    this.playersIds = [];
    this.players = [];
    this.email = "jane@email.com";
  }

  getPlayersByScope(){
      this.spinner.show();

      this.cService.getNetwork(this.email, this.getNetworkForm.value.scope).subscribe({ next: data => {
        this.network = data;

        //this.getPlayers(this.network);
        this.initializeGraph();
        //this.animate();
      this.spinner.hide();
  
        
      },
        error: _error => {
          this.spinner.hide();
        }
      });
  }

  getPlayers(lstCon:Connection[]){
    if(lstCon.length <= 0){
      this.initializeGraph();
      this.spinner.hide();
      console.log(this.playersIds);
      return;
    }
    let con = lstCon.pop()!;

    if(!this.playersIds.includes(con.player)){
      this.playersIds.push(con.player);
      this.pService.getPlayerById(con.player).subscribe({ next: data => {
        let player = data;
        console.log(player);
        if(!this.playersIds.includes(con.friend)){
          this.playersIds.push(con.friend);
          this.pService.getPlayerById(con.friend).subscribe({ next: data => {
            let friend = data;
  
            this.players.push(friend);
  
            this.getPlayers(lstCon);
    
            return;
          },
            error: _error => {
            }
          });
        }
        this.getPlayers(lstCon);
        return;
      },
        error: _error => {
        }
      });
    }
    this.getPlayers(lstCon);
    return;
    
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
  labelRenderer: CSS2DRenderer;
  camera: THREE.PerspectiveCamera;
  miniMapCamera: THREE.OrthographicCamera;
  controls: OrbitControls;

  initializeGraph(){
    this.showForm = false;
    this.showGraph = true;

    // Create a scene
    const scene = new THREE.Scene();
    scene.background = new THREE.Color(0xffffff);

    const renderer = new THREE.WebGLRenderer();
    renderer.setSize( window.innerWidth, window.innerHeight);
    document.body.appendChild( renderer.domElement );
    
    const labelRenderer = new CSS2DRenderer();
    labelRenderer.setSize( window.innerWidth, window.innerHeight );
    labelRenderer.domElement.style.position = 'absolute';
    labelRenderer.domElement.style.top = '0px';
    document.body.appendChild( labelRenderer.domElement );
    
    const camera = new THREE.PerspectiveCamera( 45, window.innerWidth / window.innerHeight, 1, 10000 );

    //creating mini-map camera
    const miniMapCamera = new THREE.OrthographicCamera(-60, 60, 60, -60);
    miniMapCamera.position.z = 10;

    this.controls = new OrbitControls( camera, renderer.domElement );
    this.controls.enableZoom = true;
    this.controls.zoomSpeed = 1.2;
		this.controls.panSpeed = 0.8;

    camera.position.set( 0, 20, 100 );
    this.controls.update();

    let nodes = [], groups = [], colors = [0xa93226, 0x884ea0, 0x5b2c6f, 0x2e86c1, 0x5dade2, 0x17a589, 0x27ae60, 
      0x0e6655, 0xf1c40f, 0x9c640c, 0xe67e22, 0x5d6d7e, 0xde3163, 0xff00ff];
    const geometry = new THREE.CircleGeometry(2, 32);

    for(let i=0; i<this.playersIds.length;i++){
      let maxH = 35, minH = -15, maxW = 25, minW = -25;
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

      nodes.push(circle);
      scene.add(circle);

      const div = document.createElement( 'div' );
      div.className = 'label';
      div.textContent = this.playersIds[i];
      div.style.color = '0x000';
      const playerLabel = new CSS2DObject( div );
      playerLabel.position.setX( 0 );
      playerLabel.position.setY( -11 );
      playerLabel.position.setZ( 0 );
      circle.add( playerLabel );

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
    labelRenderer.render( scene, camera );  
    
    renderer.render( scene, camera );    

    // Create Square
    renderer.setScissorTest(true);
    renderer.setScissor(window.innerWidth - 221, 100, 202, 202);
    renderer.setClearColor(0x000000, 1); // border color
    renderer.clearColor();

    // Create Mini-Graph
    renderer.setViewport(window.innerWidth - 221, 101, 200, 200);
    renderer.setScissor(window.innerWidth - 220, 101, 200, 200);
    renderer.setScissorTest(true);
    miniMapCamera.updateProjectionMatrix();
    renderer.render(scene, miniMapCamera);

    }

    animate() {

      requestAnimationFrame( this.animate );
      // required if controls.enableDamping or controls.autoRotate are set to true
      this.controls.update();
      this.renderer.render( this.scene, this.camera );
    
    }

}
