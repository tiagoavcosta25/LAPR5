import { Component, OnInit } from '@angular/core';
import { Validators, FormBuilder, FormControl } from '@angular/forms';
import { NgxSpinnerService } from 'ngx-spinner';
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

        this.getPlayers();
  
        this.initializeGraph();
  
        this.spinner.hide();
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
    con1.player = '26535db8-c77d-4685-9cb8-906118be935d';
    con1.friend = '2faace0a-429b-4d29-bf95-537354ba190e';
    con1.connectionStrength = 4;
    con1.tags = ['gaming', 'coding'];
    let con2 = new Connection();
    con2.player = '2faace0a-429b-4d29-bf95-537354ba190e';
    con2.friend = '8e215eed-1882-4622-ad0b-efe77379cf3c';
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

    const headerSize = 110;
    const renderer = new THREE.WebGLRenderer();
    // TODO: Set height as window.innerHeight - headerSize to fit the page size
    renderer.setSize( window.innerWidth, window.innerHeight);
    document.body.appendChild( renderer.domElement );
    
    const labelRenderer = new CSS2DRenderer();
    labelRenderer.setSize( window.innerWidth, window.innerHeight );
    labelRenderer.domElement.style.position = 'absolute';
    labelRenderer.domElement.style.top = '0px';
    document.body.appendChild( labelRenderer.domElement );
    
    const camera = new THREE.PerspectiveCamera( 45, window.innerWidth / window.innerHeight, 1, 10000 );

    // Creating miniMap camera with absolute parameters big enough to see the whole graph
    const miniMapCamera = new THREE.OrthographicCamera(-50, 50, 50, -50);

    // Looking at (0, 0, 0) by default
    //miniMapCamera.lookAt(new THREE.Vector3(0, 0, 0));

    // Change z position enough to be able to see the objects
    miniMapCamera.position.set(0, 0, 1);

    const controls = new OrbitControls( camera, renderer.domElement );
    controls.enableZoom = true;
    controls.zoomSpeed = 1.2;
		controls.panSpeed = 0.8;

    camera.position.set( 0, 20, 100 );
    controls.update();

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
      div.textContent = 'Player' + i;
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

    const miniMapBorderColor = 0x000000;
    const miniMapWidth = 200;
    const miniMapHeight = miniMapWidth;
    const borderSize = 1;
    const paddingX = 55;

    // Affect only chosen setScissor pixels
    renderer.setScissorTest(true);

    // Pixels for square border of 200 + 2 ( 2 = 1 for each border side) of width and height
    // Position chosen -> bottom right corner of the screen
    //                      
    //                      ^  _______________  ^
    //                      | |     header    | |
    //                      | |_______________| | headerSize
    //                      | | |             | v
    //   window.innerHeight | | |             |
    //                      | | |             | 
    //                      | | |          <->| paddingX (for display purpose, to push it further from the end of the screen)
    //                      | | |           x | minimap location
    //                      | |_|_____________| 
    //                      v
    //                        <---------------> window.innerWidth
    //
    //
    renderer.setScissor(window.innerWidth - miniMapWidth - paddingX, headerSize,
      miniMapWidth + ( 2 * borderSize ), miniMapHeight + ( 2 * borderSize ));

    // Create miniMap border with given color
    renderer.setClearColor(miniMapBorderColor, 1);
    renderer.clearColor();

    // Set viewport inside miniMap border ( Border has size 1, so if border starts in x = 200 and y = 200, viewport should start in
    // x = 201 and y = 201), with miniMap given width and height
    renderer.setViewport(window.innerWidth - miniMapWidth - paddingX + borderSize, headerSize + borderSize, 
      miniMapWidth, miniMapHeight);

    // Pixels for miniMap of 200 of width and height
    // Position chosen -> bottom right corner of the screen
    renderer.setScissor(window.innerWidth - miniMapWidth - paddingX + borderSize, headerSize + borderSize, 
      miniMapWidth, miniMapHeight);

    // UpdateMatrix and render graph
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
