import { Component, OnInit } from '@angular/core';
import { Validators, FormBuilder, FormControl } from '@angular/forms';
import { NgxSpinnerService } from 'ngx-spinner';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { Player } from 'src/shared/models/player/player.model';
import * as THREE from 'three';
import { Vector3, Vector4 } from 'three';
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls';
import { CSS2DRenderer, CSS2DObject } from 'three/examples/jsm/renderers/CSS2DRenderer.js';
import { Router, Event, NavigationEnd } from '@angular/router';
import { NetworkConnection } from 'src/shared/models/connection/network-connection.dto';
import { firstValueFrom } from 'rxjs';
import { NetworkPlayer } from 'src/shared/models/connection/network-player.dto';
import { NetworkScope } from 'src/shared/models/connection/network-scope.dto';


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
  id: string;
  success: any;
  showForm: boolean = true;
  showGraph: boolean = false;


  connections: NetworkConnection[];
  scopes: NetworkScope[];


  idRFrame: any;

  constructor(
    private spinner: NgxSpinnerService,
    private cService: ConnectionService,
    private pService: PlayerService,
    private fb: FormBuilder, 
    private router: Router) { 

      this.showForm = true;
      this.showGraph = false;
      this.connections = [];
      this.scopes = [];
    }

  ngOnInit(): void {
    this.email = localStorage.getItem('currentPlayer')!.trim();
    
    this.router.events.subscribe((event: Event) => {
      if( event instanceof NavigationEnd ) {
        window.location.reload();
      }
    })
  }

  getPlayersByScope() {
      this.spinner.show();

      this.cService.getNetwork(this.email, this.getNetworkForm.value.scope).subscribe({ next: async data => {
        this.id = await this.getCurrentPlayerId();
        for(let con of data) {
          let netCon = new NetworkConnection(con.id, con.player, con.friend);
          let tempPlayer = await this.getPlayer(netCon.player.id);
          netCon.player.setEmailAndName(tempPlayer.email, tempPlayer.name);
          tempPlayer = await this.getPlayer(netCon.friend.id);
          netCon.friend.setEmailAndName(tempPlayer.email, tempPlayer.name);
          this.connections.push(netCon);
        }
        this.getScopes();
        this.initializeGraph();
        this.spinner.hide();
      },
        error: _error => {
          this.spinner.hide();
        }
      });
  }

  getScopes() {
    let connections: NetworkConnection[] = [];
    this.connections.forEach(val => connections.push(Object.assign({}, val)));
    let toVisit: NetworkPlayer[] = [];
    let visited: NetworkPlayer[] = [];
    let scopeLevel: number[] = [0];
    let currentPlayer: NetworkPlayer;
    let currentScope: number;
    for(let c of connections) {
      if(c.player.id == this.id) {
        toVisit.push(c.player);
        break;
      }
    }
    if(connections.length != 0) {
      while(toVisit.length > 0) {
        currentPlayer = toVisit.splice(0, 1)[0];
        visited.push(currentPlayer);
        currentScope = scopeLevel.splice(0, 1)[0];
        for(let i = 0; i < connections.length; i++) {
          if(connections[i].player.id == currentPlayer.id && !this.scopes.some(x => x.scope <= currentScope && x.friends.some(y => y.id == connections[i].friend.id))) {
            if(this.scopes.some(x => x.player.id == currentPlayer.id)) {
              // Scope with this player exists
              let index = this.scopes.findIndex(x => x.player.id == currentPlayer.id);
              this.scopes[index].friends.push(connections[i].friend);
            } else {
              // Scope with this player does not exist
              let scope = new NetworkScope(connections[i].player, currentScope);
              scope.friends.push(connections[i].friend);
              this.scopes.push(scope);
            }
            if(!visited.some(x => x == connections[i].friend)) {
              toVisit.push(connections[i].friend);
              scopeLevel.push(currentScope + 1);
            }
            connections.splice(i, 1);
            i--;
          }
        }
      }
    }
  }

  async getPlayer(id: string): Promise<Player>{
    return await firstValueFrom(this.pService.getPlayerById(id));
  }

  async getCurrentPlayerId(): Promise<string>{
    return (await firstValueFrom(this.pService.getOnlyPlayerByEmail(this.email))).id;
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
  controlsMiniMap: OrbitControls;
  miniCamXMin: number;
  miniCamYMin: number;
  miniCamXMax: number;
  miniCamYMax: number;
  mousePosition: any;

  initializeGraph(){
    this.showForm = false;
    this.showGraph = true;

    // Create a scene
    this.scene = new THREE.Scene();
    this.scene.background = new THREE.Color(0xffffff);

    this.renderer = new THREE.WebGLRenderer( { alpha: true } );
    this.renderer.setSize( window.innerWidth, window.innerHeight-70);
    this.renderer.domElement.style.position = 'absolute';
    this.renderer.domElement.style.top = '70px';
    this.renderer.domElement.style.left = '0px';
    this.renderer.domElement.style.zIndex = '1';
    document.body.appendChild( this.renderer.domElement );
    this.labelRenderer = new CSS2DRenderer();
    this.labelRenderer.setSize( window.innerWidth, window.innerHeight-70);
    this.labelRenderer.domElement.style.position = 'absolute';
    this.labelRenderer.domElement.style.top = '70px';
    this.renderer.domElement.style.left = '0px';
    document.body.appendChild( this.labelRenderer.domElement );
  
    this.camera = new THREE.PerspectiveCamera( 45, window.innerWidth / window.innerHeight, 1, 10000 );
         
    // Creating miniMap camera with absolute parameters big enough to see the whole graph
    this.miniMapCamera = new THREE.OrthographicCamera(-50, 50, 50, -50);
    
    // Looking at (0, 0, 0) by default
    //miniMapCamera.lookAt(new THREE.Vector3(0, 0, 0));

    this.controls = new OrbitControls(this.camera, this.renderer.domElement);
    this.controls.enableZoom = true;
    this.controls.enablePan = true;
    this.controls.enableRotate = true;
    this.controls.minZoom = 0.2;
    this.controls.maxZoom = 12;
    this.controls.zoomSpeed = 2;
    
    this.camera.position.set( 0, 20, 150 );

    this.controls.update();
    this.controls.enabled = true;

    this.controlsMiniMap = new OrbitControls(this.miniMapCamera, this.renderer.domElement);
    this.controlsMiniMap.enableZoom = true;
    this.controlsMiniMap.enablePan = true;
    this.controlsMiniMap.enableRotate = false;
    this.controlsMiniMap.minZoom = 0.2;
    this.controlsMiniMap.maxZoom = 12;
    this.controlsMiniMap.zoomSpeed = 2;
    this.controlsMiniMap.panSpeed = 4;

    this.miniMapCamera.position.set( 0, 0, 150 );

    this.controlsMiniMap.update();
    this.controlsMiniMap.enabled = false;

    let nodes = [];

    let material, geometry;
    let scopex = 0;
    let scopey = 0; 
    for(let scope of this.scopes){
      let radius = 35 / (scope.scope + 1);
      let angleIncrement = this.calculateAngleIncrement(scope);
      if(scope.player.id == this.id){
        material = new THREE.MeshBasicMaterial( { color: 0xe67e22  } );
        geometry = new THREE.SphereGeometry(4, 32, 16);
        scope.player.setMesh( geometry, material );
        scope.player.sphere.position.x = 0;
        scope.player.sphere.position.y = 0;
        scope.player.sphere.position.z = 0;

        const div = document.createElement( 'div' );
        div.className = 'label';
        div.textContent = scope.player.name;
        div.style.color = '0x000';
        const playerLabel = new CSS2DObject( div );
        playerLabel.position.setX( 0 );
        playerLabel.position.setY( -5 );
        playerLabel.position.setZ( 0 );
        scope.player.sphere.add( playerLabel );

        nodes.push(scope.player);
        this.scene.add(scope.player.sphere);

      } else {
        let index = nodes.findIndex(x => x.id == scope.player.id);
        scopex = nodes[index].sphere.position.x;
        scopey = nodes[index].sphere.position.y;
      }
      let angle = 0;
      for(let scopeFriend of scope.friends) {
        material = new THREE.MeshBasicMaterial( { color: 0x2e86c1  } );
        geometry = new THREE.SphereGeometry(2, 32, 16);
        scope.player.setMesh( geometry, material );
        scopeFriend.setMesh(geometry, material);
        scopeFriend.sphere.position.x = scopex + radius * Math.cos(angle);
        scopeFriend.sphere.position.y = scopey + radius * Math.sin(angle);
        scopeFriend.sphere.position.z = 0;
        angle += angleIncrement;

        const div = document.createElement( 'div' );
        div.className = 'label';
        div.textContent = scopeFriend.name;
        div.style.color = '0x000';
        const playerLabel = new CSS2DObject( div );
        playerLabel.position.setX( 0 );
        playerLabel.position.setY( -3 );
        playerLabel.position.setZ( 0 );
        scopeFriend.sphere.add( playerLabel );

        nodes.push(scopeFriend);
        this.scene.add(scopeFriend.sphere);
      }

    }

    for(let con of this.connections) {
      let indexPlayer = nodes.findIndex(x => x.id == con.player.id);
      let indexFriend = nodes.findIndex(x => x.id == con.friend.id);
      this.createEdge(nodes[indexPlayer].sphere.position, nodes[indexFriend].sphere.position);
    }

    window.addEventListener('resize', () => {
      this.camera.aspect = window.innerWidth / window.innerHeight;
      this.camera.updateProjectionMatrix();
      this.renderer.setSize(window.innerWidth, window.innerHeight-70);
      this.labelRenderer.setSize(window.innerWidth, window.innerHeight-70);
    })

    this.renderer.domElement.addEventListener('mousemove', event => {
      this.mousePosition = new THREE.Vector2(event.clientX, window.innerHeight - event.clientY + 30);
      if( ( event.clientX >= this.miniCamXMin && event.clientX <= this.miniCamXMax ) &&
       ( window.innerHeight - event.clientY + 30 >= this.miniCamYMin && window.innerHeight - event.clientY + 30 <= this.miniCamYMax ) ) {
        this.controls.enabled = false;
        this.controlsMiniMap.enabled = true;
      }
      else {
        this.controls.enabled = true;
        this.controlsMiniMap.enabled = false;
      }
    });


    this.spinner.hide();
    
    this.animate();

  }

  createEdge(position0: Vector3, position1: Vector3) {
    let edgeGeometry = new THREE.CylinderGeometry(0.5, 0.5, 1.0);
    let edgeMaterial = new THREE.MeshBasicMaterial({ color: 0x80ffff });

    // Compute distance between nodes
    const distance = position1.distanceTo(position0);

    // Create a mesh with the specified geometry and material
    const cylinder = new THREE.Mesh(edgeGeometry, edgeMaterial);

    // Set its position
    cylinder.position.set((position0.x + position1.x) / 2.0, (position0.y + position1.y) / 2.0, (position0.z + position1.z) / 2.0);

    // Set its orientation
    const angH = Math.atan2(position1.x - position0.x, position1.z - position0.z);
    const angV = Math.acos((position1.y - position0.y) / distance);
    cylinder.rotateY(angH);
    cylinder.rotateX(angV);

    // Set its length
    cylinder.scale.set(1.0, distance, 1.0);

    // Add it to the scene
    this.scene.add(cylinder);
}

  calculateAngleIncrement(scope: NetworkScope): number {
    let quantity = scope.friends.length;
    let angle = 2.0 * Math.PI / quantity;
    return angle;
  }

  animate() {
    requestAnimationFrame( this.animate.bind(this) );
    // required if controls.enableDamping or controls.autoRotate are set to true
    this.controls.update();
    this.controlsMiniMap.update();
    this.renderer.render( this.scene, this.camera );
    this.labelRenderer.render( this.scene, this.camera );
    this.renderMiniMap();
  }

  renderMiniMap() {
    // Change z position enough to be able to see the objects
    //this.miniMapCamera.position.set(0, 0, 1);
    const miniMapBorderColor = 0x000000;
    const miniMapWidth = 200;
    const miniMapHeight = miniMapWidth;
    const borderSize = 1;
    const paddingX = 55;
    const paddingY = 55;

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

    this.miniCamXMin = window.innerWidth - miniMapWidth - paddingX + borderSize;
    this.miniCamYMin = paddingY + borderSize;
    this.miniCamXMax = this.miniCamXMin + miniMapWidth; 
    this.miniCamYMax = this.miniCamYMin + miniMapHeight;

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
