import { Component, OnInit } from '@angular/core';
import { Validators, FormBuilder, FormControl } from '@angular/forms';
import { NgxSpinnerService } from 'ngx-spinner';
import { observable, Observable } from 'rxjs';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { Connection } from 'src/shared/models/connection/connection.model';
import { Player } from 'src/shared/models/player/player.model';
import * as THREE from 'three';
import { Group, Vector4 } from 'three';
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
  playersTempIds: string[];
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
      this.playersTempIds = [];
      this.network = [];
    }

  ngOnInit(): void {
    this.network = [];
    this.playersIds = [];
    this.players = [];
    //this.email = "email1@gmail.com";
    this.email = localStorage.getItem('currentPlayer')!.trim();
    console.log(this.email);
  }

  getPlayersByScope(){
      this.spinner.show();

      this.cService.getNetwork(this.email, this.getNetworkForm.value.scope).subscribe({ next: data => {
        this.network = data;

        this.getPlayersIds();
        this.getPlayers(this.playersTempIds);
  
        
      },
        error: _error => {
          this.spinner.hide();
        }
      });
  }

  getPlayersIds(){
    
    for(let c of this.network){
      if(!this.playersIds.includes(c.player)){
        this.playersIds.push(c.player);
        this.playersTempIds.push(c.player);
      }
      if(!this.playersIds.includes(c.friend)){
        this.playersIds.push(c.friend);
        this.playersTempIds.push(c.friend);
      }
    }
    
  }

  getPlayers(lstIds:string[]){
    if(lstIds.length <= 0){
      console.log(this.playersIds);
      this.initializeGraph();
      this.spinner.hide();
      return;
    }
    let id = lstIds.pop()!;

      this.pService.getPlayerById(id).subscribe({ next: data => {
        let player = data;
        this.players.push(player); 
        this.getPlayers(lstIds);

        return;
      },
        error: _error => {
        }
      });
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
  animateCallback:any;


  initializeGraph(){
    this.showForm = false;
    this.showGraph = true;

    // Create a scene
    this.scene = new THREE.Scene();
    this.scene.background = new THREE.Color(0xffffff);

    this.renderer = new THREE.WebGLRenderer();
    this.renderer.setSize( window.innerWidth, window.innerHeight);
    document.body.appendChild( this.renderer.domElement );
    //document.appendChild( this.renderer.domElement);
    this.labelRenderer = new CSS2DRenderer();
    this.labelRenderer.setSize( window.innerWidth, window.innerHeight);
    this.labelRenderer.domElement.style.position = 'absolute';
    this.labelRenderer.domElement.style.top = '0px';
    document.body.appendChild( this.labelRenderer.domElement );
    
    this.camera = new THREE.PerspectiveCamera( 45, window.innerWidth / window.innerHeight, 1, 10000 );

    this.controls = new OrbitControls( this.camera, this.renderer.domElement );
    this.controls.enableZoom = true;
    this.controls.enableRotate = false;     
    this.controls.enableDamping = true;     
    this.controls.enablePan = true;     
    this.controls.zoomSpeed = 1.2;
		this.controls.panSpeed = 0.8;
    this.controls.update();

    this.camera.position.set( 0, 20, 100 );
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
      this.scene.add(circle);

      const div = document.createElement( 'div' );
      div.className = 'label';
      div.textContent = this.players[i].name;
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

      this.scene.add( line );

    }

    this.renderer.render( this.scene, this.camera );
    this.labelRenderer.render( this.scene, this.camera );  
    
    this.renderMiniMap();

    window.addEventListener('wheel', (event) => {
      event.preventDefault(); /// prevent scrolling
      
      let zoom = this.camera.zoom; // take current zoom value
      zoom += event.deltaY * -0.01; /// adjust it
      zoom = Math.min(Math.max(.010, zoom), 4); /// clamp the value
    
      this.camera.zoom = zoom /// assign new zoom value
      this.camera.updateProjectionMatrix(); /// make the changes take effect
    }, { passive: false });

    window.addEventListener('resize', () => {
      this.camera.aspect = window.innerWidth / window.innerHeight;
      this.camera.updateProjectionMatrix();
      this.renderer.setSize(window.innerWidth, window.innerHeight);
    })
    this.spinner.hide();
    
    this.animateCallback = {
      callAnimate: (this.animate).bind(this)
    };
    this.animateCallback.callAnimate();

    }

    animate() {

      requestAnimationFrame( this.animateCallback.callAnimate );
      // required if controls.enableDamping or controls.autoRotate are set to true
      this.controls.update();
      this.renderer.render( this.scene, this.camera );
      this.labelRenderer.render( this.scene, this.camera ); 
      this.renderMiniMap();
    
    }

    renderMiniMap() {
    
      // Creating miniMap camera with absolute parameters big enough to see the whole graph
      this.miniMapCamera = new THREE.OrthographicCamera(-50, 50, 50, -50);

      // Looking at (0, 0, 0) by default
      //miniMapCamera.lookAt(new THREE.Vector3(0, 0, 0));
  
      // Change z position enough to be able to see the objects
      this.miniMapCamera.position.set(0, 0, 1);
      const miniMapBorderColor = 0x000000;
      const miniMapWidth = 200;
      const miniMapHeight = miniMapWidth;
      const borderSize = 1;
      const paddingX = 55;
      const paddingY = 125;

      // Affect only chosen setScissor pixels
      this.renderer.setScissorTest(true);

      // Pixels for square border of 200 + 2 ( 2 = 1 for each border side) of width and height
      // Position chosen -> bottom right corner of the screen
      //                      
      //                      ^  _______________  
      //                      | |     header    | 
      //                      | |_______________| 
      //                      | | |             | 
      //   window.innerHeight | | |             |
      //                      | | |             | 
      //                      | | |          <->| paddingX (for display purpose, to push it further from the end of the screen)
      //                      | | |           x | minimap location
      //                      | | |             |
      //                      | | |             |
      //                      | | |             |^
      //                      | | |             || paddingY (for display purpose, to push it further from the end of the screen)
      //                      | |_|_____________|v
      //                      v
      //                        <---------------> window.innerWidth
      //
      //
      this.renderer.setScissor(window.innerWidth - miniMapWidth - paddingX, paddingY,
        miniMapWidth + ( 2 * borderSize ), miniMapHeight + ( 2 * borderSize ));

      // Create miniMap border with given color
      this.renderer.setClearColor(miniMapBorderColor, 1);
      this.renderer.clearColor();

      // Save default viewport
      let vp: Vector4 = new Vector4; 
      this.renderer.getCurrentViewport(vp);
      
      // Set viewport inside miniMap border ( Border has size 1, so if for example border starts in x = 200 and y = 200, viewport should start in
      // x = 201 and y = 201), with miniMap given width and height
      this.renderer.setViewport(window.innerWidth - miniMapWidth - paddingX + borderSize, paddingY + borderSize, 
        miniMapWidth, miniMapHeight);

      // Pixels for miniMap of 200 of width and height
      // Position chosen -> bottom right corner of the screen
      this.renderer.setScissor(window.innerWidth - miniMapWidth - paddingX + borderSize, paddingY + borderSize, 
        miniMapWidth, miniMapHeight);

      // UpdateMatrix and render graph
      this.miniMapCamera.updateProjectionMatrix();
      this.renderer.render(this.scene, this.miniMapCamera);
      
      // Deactivate ScissorTest
      this.renderer.setScissorTest(false);
      
      // Set default viewport
      this.renderer.setViewport(vp);
    }

}
