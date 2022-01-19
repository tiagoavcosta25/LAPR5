import { Component, OnInit } from '@angular/core';
import { Validators, FormBuilder, FormControl } from '@angular/forms';
import { NgxSpinnerService } from 'ngx-spinner';
import { ConnectionService } from 'src/app/modules/connection/services/connection.service';
import { PlayerService } from 'src/app/modules/player/services/player.service';
import { Player } from 'src/shared/models/player/player.model';
import * as THREE from 'three';
import { Vector2, Vector4 } from 'three';
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls';
import { CSS2DRenderer, CSS2DObject } from 'three/examples/jsm/renderers/CSS2DRenderer.js';
import { Router, Event, NavigationEnd } from '@angular/router';
import { NetworkConnection } from 'src/shared/models/connection/network-connection.dto';
import { firstValueFrom } from 'rxjs';
import { NetworkPlayer } from 'src/shared/models/connection/network-player.dto';
import { NetworkScope } from 'src/shared/models/connection/network-scope.dto';
import SpriteText from 'three-spritetext';
import { GLTFLoader } from 'three/examples/jsm/loaders/GLTFLoader.js';

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
          let netCon = new NetworkConnection(con.id, con.player, con.friend, con.connectionStrength);
          let tempPlayer = await this.getPlayer(netCon.player.id);
          netCon.player.setInfo(tempPlayer);
          tempPlayer = await this.getPlayer(netCon.friend.id);
          netCon.friend.setInfo(tempPlayer);
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
  mouse: Vector2 = new THREE.Vector2(0, 0);
  raycaster = new THREE.Raycaster();
  onObject : THREE.Intersection[] = [];
  objectPressed: THREE.Intersection[] = [];
  buttonAdded: any[] = [];
  nodes: NetworkPlayer[]  = [];
  cylinders: NetworkConnection[] = [];
  onCylinder : THREE.Intersection[] = [];
  labelAdded: any[] = [];
  changesActive: boolean = true;
  currentLabel: CSS2DObject;
  currentLabelSelected: CSS2DObject;

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
    
    this.camera.position.set( 0, 20, 140 );

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

    this.miniMapCamera.position.set( 0, 0, 140 );

    this.controlsMiniMap.update();
    this.controlsMiniMap.enabled = false;


    let material, geometry;
    let scopex = 0;
    let scopey = 0; 
    for(let scope of this.scopes){
      let radius = (35 / (scope.scope + 1)) - scope.scope;
      let angleIncrement = this.calculateAngleIncrement(scope);
      scope.angle = angleIncrement;
      if(scope.player.id == this.id){
        material  = new THREE.MeshStandardMaterial({color : 0xe67e22,metalness: 0.2,roughness: 0.55,opacity: 1.0}) ;
        geometry = new THREE.SphereGeometry(4, 32, 16);
        scope.player.setMesh( geometry, material );
        scope.player.sphere.position.x = 0;
        scope.player.sphere.position.y = 0;
        scope.player.sphere.position.z = 0;

        var myText = new SpriteText(scope.player.name);
        myText.color = "0x00000";
        myText.textHeight = 3;
        myText.position.setY(-5); 
        myText.fontSize = 50;
        myText.fontWeight = "500";
        myText.material.depthTest = false;
        scope.player.sphere.add(myText);

        this.nodes.push(scope.player);
        this.scene.add(scope.player.sphere);

      } else {
        let index = this.nodes.findIndex(x => x.id == scope.player.id);
        scopex = this.nodes[index].sphere.position.x;
        scopey = this.nodes[index].sphere.position.y;
      }
      let angle = 0;
      for(let s of this.scopes) {
        for(let i = 0; i < s.friends.length; i++) {
          if(scope.player.email == s.friends[i].email ) {
            if(s.friends.length != 1) {
              angle = s.firstAngle + (s.angle * i) - Math.PI + angleIncrement / 2;
            } 
              scope.firstAngle = angle;
            break;
          }
        }
      }
      for(let scopeFriend of scope.friends) {
        material  = new THREE.MeshStandardMaterial({color : 0x2e86c1,metalness: 0.2,roughness: 0.55,opacity: 1.0}) ;
        geometry = new THREE.SphereGeometry(2, 32, 16);
        scopeFriend.setMesh(geometry, material);
        scopeFriend.sphere.position.x = scopex + radius * Math.cos(angle);
        scopeFriend.sphere.position.y = scopey + radius * Math.sin(angle);
        let connectsNumber = 0;
        for(let s of this.scopes) {
          if(s.player.email == scopeFriend.email) {
            connectsNumber = s.friends.length;
            break;
          }
        }
        scopeFriend.sphere.position.z = 10 * connectsNumber;
        angle += angleIncrement;


        var myText = new SpriteText(scopeFriend.name);
        myText.color = "0x00000";
        myText.textHeight = 3;
        myText.position.setY(-5); 
        myText.fontSize = 50;
        myText.fontWeight = "500";
        myText.material.depthTest = false;
        scopeFriend.sphere.add(myText);


        this.nodes.push(scopeFriend);
        this.scene.add(scopeFriend.sphere);
      }

    }

    for(let con of this.connections) {
      this.createEdge(con);
    }

    //lights
    const colorL = 0xffffff;
    const intensity = 1;
    const lightA = new THREE.AmbientLight(colorL, intensity);
    this.scene.add(lightA);

    const lightP1 = new THREE.PointLight(0xffffff, 0.5);
    lightP1.position.set(-500, 500, 50);
    this.scene.add(lightP1);

    const lightP2 = new THREE.PointLight(0xffffff, 0.5);
    lightP2.position.set(500, -500, 50);
    this.scene.add(lightP2);

    const colorSL = 0xffffff;
    const intensitySL = 1;
    const lightSL = new THREE.SpotLight(colorSL, intensitySL);
    lightSL.target = this.camera;
    lightSL.angle = THREE.MathUtils.degToRad(1);
    lightSL.penumbra = 0.4;
    lightSL.position.z = 140;
    this.camera.add(lightSL);
    this.scene.add(this.camera);

    const positionIncrement = 2;

    window.addEventListener('keydown', event => {
        switch (event.key) {
          case 'w': this.camera.translateZ(-positionIncrement); break;
          case 's': this.camera.translateZ(positionIncrement); break;
          case 'd': this.camera.translateX(positionIncrement); break;
          case 'a': this.camera.translateX(-positionIncrement); break;
          case 'p': this.camera.position.y += positionIncrement; break;
          case 'l': this.camera.position.y -= positionIncrement; break;
          default:
        }
    });

    window.addEventListener('resize', () => {
      this.camera.aspect = window.innerWidth / window.innerHeight;
      this.camera.updateProjectionMatrix();
      this.renderer.setSize(window.innerWidth, window.innerHeight-70);
      this.labelRenderer.setSize(window.innerWidth, window.innerHeight-70);
    })

    this.renderer.domElement.addEventListener('mousemove', event => {
      this.mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
      this.mouse.y = - ( (event.clientY - 70 ) / (window.innerHeight - 70) ) * 2 + 1;

      this.raycaster.setFromCamera(this.mouse, this.camera);
      
      if(this.changesActive) {
        this.checkIntersects();
        this.checkIntersectsConnections(); 
      }
      this.checkMouseInMinimap(event);
    });


    this.renderer.domElement.addEventListener('click', event => {
      this.clickIntersects();
    });

    this.renderer.domElement.addEventListener('auxclick', event => {
      if(event.button == 1) {
        if(this.onObject.length > 0) {
          let player = this.checkWhichPlayerIs((<THREE.Mesh>this.onObject[0].object));
          this.router.navigate(['/profile', player?.email]);
        }
      }
    })

    this.spinner.hide();
    this.animate();

  }

  clickIntersects() {
    if(this.onObject.length > 0) { 
      if(!((<THREE.Mesh>this.onObject[0].object).position.x == 0 && (<THREE.Mesh>this.onObject[0].object).position.y == 0)) {
        if(this.objectPressed.length > 0 && (<THREE.Mesh>this.onObject[0].object).position != (<THREE.Mesh>this.objectPressed[0].object).position) {
          this.resetColor();

          this.removeButtons();
        }
        this.objectPressed = this.onObject;

        const loader = new GLTFLoader();
        loader.load(
          'assets/layout/network/avatar/scene.gltf',
          ( gltf ) => {
              // called when the resource is loaded
              var object = gltf.scene;
              object.scale.set(0.15, 0.15, 0.15);
              object.position.setX(14);
              object.position.setY(-4);
              object.position.setZ(1);
              (<THREE.Mesh>this.objectPressed[0].object).add(object);
          },
          ( xhr ) => {
              // called while loading is progressing
              console.log( `${( xhr.loaded / xhr.total * 100 )}% loaded` );
          },
          ( error ) => {
              // called when loading has errors
              console.error( 'An error happened', error );
          },
        );

        const buttonStrongest = document.createElement( 'button' );
        buttonStrongest.className = 'btn btn-secondary';
        buttonStrongest.id = 'btn-strong';
        buttonStrongest.addEventListener("click", () => {
          // Algoritmo aqui
          let player = this.checkWhichPlayerIs((<THREE.Mesh>this.objectPressed[0].object));
          console.log("Strongest Path for player: " + player?.name + ", with email: " + player?.email + ", with id: " + player?.id);
          this.removeButtons();
          if(!((<THREE.Mesh>this.objectPressed[0].object).position.x == 0 && (<THREE.Mesh>this.objectPressed[0].object).position.y == 0)) {
            this.resetColor();
          }
        })
        buttonStrongest.textContent = "Strongest";
        buttonStrongest.style.color = '0x000';
        const buttonStrongestObject = new CSS2DObject( buttonStrongest );
        buttonStrongestObject.position.setX( -18 );
        buttonStrongestObject.position.setY( +7 );
        buttonStrongestObject.position.setZ( 0 );
        this.buttonAdded.push(buttonStrongestObject);

        const buttonShortest = document.createElement( 'button' );
        buttonShortest.className = 'btn btn-secondary';
        buttonShortest.id = 'btn-short';
        buttonShortest.addEventListener("click", () => {
          // Algoritmo aqui
          let player = this.checkWhichPlayerIs((<THREE.Mesh>this.objectPressed[0].object));
          console.log("Shortest Path for player: " + player?.name + ", with email: " + player?.email + ", with id: " + player?.id);
          this.removeButtons();
          if(!((<THREE.Mesh>this.objectPressed[0].object).position.x == 0 && (<THREE.Mesh>this.objectPressed[0].object).position.y == 0)) {
            this.resetColor();
          }
        })
        buttonShortest.textContent = "Shortest";
        buttonShortest.style.color = '0x000';
        const buttonShortestObject = new CSS2DObject( buttonShortest );
        buttonShortestObject.position.setX( -1 );
        buttonShortestObject.position.setY( +7 );
        buttonShortestObject.position.setZ( 0 );
        this.buttonAdded.push(buttonShortestObject);

        const buttonSafest = document.createElement( 'button' );
        buttonSafest.className = 'btn btn-secondary';
        buttonSafest.id = 'btn-safe';
        buttonSafest.addEventListener("click", () => {
          // Algoritmo aqui
          let player = this.checkWhichPlayerIs((<THREE.Mesh>this.objectPressed[0].object));
          console.log("Safest Path for player: " + player?.name + ", with email: " + player?.email + ", with id: " + player?.id);
          this.removeButtons();
          if(!((<THREE.Mesh>this.objectPressed[0].object).position.x == 0 && (<THREE.Mesh>this.objectPressed[0].object).position.y == 0)) {
            this.resetColor();
          }
        })
        buttonSafest.textContent = "Safest";
        buttonSafest.style.color = '0x000';
        const buttonSafestObject = new CSS2DObject( buttonSafest );
        buttonSafestObject.position.setX( +14 );
        buttonSafestObject.position.setY( +7 );
        buttonSafestObject.position.setZ( 0 );
        this.buttonAdded.push(buttonSafestObject);
        
        (<THREE.Mesh>this.objectPressed[0].object).add(buttonStrongestObject);
        (<THREE.Mesh>this.objectPressed[0].object).add(buttonShortestObject);
        (<THREE.Mesh>this.objectPressed[0].object).add(buttonSafestObject);
      }
    } else {
      if(this.objectPressed.length > 0) {
        if(!((<THREE.Mesh>this.objectPressed[0].object).position.x == 0 && (<THREE.Mesh>this.objectPressed[0].object).position.y == 0)) {
          this.resetColor();
          this.removeButtons();
        }
        this.objectPressed = [];
      }
    }
  }
  checkIntersects() {
    let spheres = [];
      for(let node of this.nodes) {
        spheres.push(node.sphere);
      }
      const intersects = this.raycaster.intersectObjects( spheres );
      for(let n of this.scene.children) {
        if(n.children.length > 1) {
          n.remove(this.currentLabel);
        }
      }  
      if(this.onObject.length > 0 && intersects != this.onObject) {
        for(let obj of this.onObject) {
          if(!this.objectPressed.some(x => x.object.position == obj.object.position)) {
            if(!((<THREE.Mesh>obj.object).position.x == 0 && (<THREE.Mesh>obj.object).position.y == 0)) {
              (<THREE.MeshStandardMaterial>(<THREE.Mesh>obj.object).material).color.set(0x2e86c1);
            }
          }
        }
      }
      this.onObject = intersects;

      for(let inter of intersects) {
        if(!((<THREE.Mesh>inter.object).position.x == 0 && (<THREE.Mesh>inter.object).position.y == 0)) {
          (<THREE.MeshStandardMaterial>(<THREE.Mesh>inter.object).material).color.set(0xff0000);
          
          
          let player = this.checkWhichPlayerIs((<THREE.Mesh>inter.object));

          if(player != null) {
            const divcard = document.createElement( 'div' );
            divcard.className = "card";
            divcard.style.width = "18rem";

            const divcardbody = document.createElement( 'div' );
            divcardbody.className = "card-body";
            
            const row = document.createElement( 'div' );
            row.className = "row";

            //Image stuff
            const colimg = document.createElement( 'div' );
            colimg.className = "col-2";

            const spanuserimage = document.createElement( 'span' );
            spanuserimage.className = "userimage";
            spanuserimage.style.float = "left";
            spanuserimage.style.width = "34px";
            spanuserimage.style.height = "34px";
            spanuserimage.style.borderRadius = "40px";
            spanuserimage.style.overflow = "hidden";
            spanuserimage.style.margin= "-2px 10px -2px 0";
            spanuserimage.style.width= "34px";
            
            const img = document.createElement( 'img' );
            img.src = "https://bootdey.com/img/Content/avatar/avatar3.png";
            img.style.maxWidth = "100%";
            img.style.display = "block";
            
            //Name stuff
            const colname = document.createElement( 'div' );
            colname.className = "col-10";

            const playername = document.createElement( 'h4' );
            playername.textContent = player.name;
            playername.style.marginTop = "3px";

            //Email stuff

            const colemail = document.createElement( 'div' );
            colemail.className = "col-12";

            const playeremail = document.createElement( 'h4' );
            playeremail.textContent = "Email: " + player.email;

            //Birthday stuff

            const colbd = document.createElement( 'div' );
            colbd.className = "col-12";

            const playerbd = document.createElement( 'h4' );
            playerbd.textContent = "Birthday: " + player.dateOfBirth;

            //Phone number

            const colpn = document.createElement( 'div' );
            colpn.className = "col-12";

            const playerpn = document.createElement( 'h4' );
            playerpn.textContent = "Phone number: " + player.phoneNumber;

            //Tags

            const coltag = document.createElement( 'div' );
            coltag.className = "col-12";

            const tagText = document.createElement( 'h4' );
            tagText.textContent = "Tags: ";

            coltag.appendChild(tagText);

            //Tags Content

            const coltagcontent = document.createElement( 'div' );
            coltagcontent.className = "col-12";

            for(let t of player.tags) {
              const playertag = document.createElement( 'a' );
              playertag.className = "badge bg-success";
              playertag.style.textDecoration = "none";
              playertag.textContent = t;

              coltagcontent.appendChild(playertag);
            }

            //Appends

            spanuserimage.appendChild(img);
            colimg.appendChild(spanuserimage);

            colname.appendChild(playername);

            colemail.appendChild(playeremail);

            colbd.appendChild(playerbd);

            colpn.appendChild(playerpn);

            row.appendChild(colimg);
            row.appendChild(colname);
            row.appendChild(colemail);
            row.appendChild(colbd);
            row.appendChild(colpn);
            row.appendChild(coltag);
            row.appendChild(coltagcontent);

            divcardbody.appendChild(row);

            divcard.appendChild(divcardbody);

            const popup = new CSS2DObject( divcard );
            popup.position.setX( 0 );
            popup.position.setY( -30 );
            popup.position.setZ( 10 );
            if((<THREE.Mesh>this.onObject[0].object).children.length === 1 || 
            (<THREE.Mesh>this.onObject[0].object).children.length === 5) {
              (<THREE.Mesh>this.onObject[0].object).add(popup);
              this.currentLabel = popup;
            }
          }
        }
      }
  }



  checkIntersectsConnections() {
    if(this.onObject.length > 0) {
      if(this.onCylinder.length > 0) {
        for(let obj of this.onCylinder) {
              (<THREE.MeshStandardMaterial>(<THREE.Mesh>obj.object).material).color.set(0x80ffff);
              for(let label of this.labelAdded) {
                (<THREE.Mesh>obj.object).remove(label);
              }
        }
      }
      this.onCylinder = [];
      return;
    }
    let cyls = [];
      for(let cylinder of this.cylinders) {
        cyls.push(cylinder.cylinder);
      }
      const intersects = this.raycaster.intersectObjects( cyls );

      if(this.onCylinder.length > 0 && intersects != this.onCylinder) {
        for(let obj of this.onCylinder) {
              (<THREE.MeshStandardMaterial>(<THREE.Mesh>obj.object).material).color.set(0x80ffff);
              for(let label of this.labelAdded) {
                (<THREE.Mesh>obj.object).remove(label);
              }
        }
      }
      this.onCylinder = intersects;

      for(let inter of intersects) {
        (<THREE.MeshStandardMaterial>(<THREE.Mesh>inter.object).material).color.set(0x23afef);

        let con;
        for(let c of this.cylinders) {
          if(c.cylinder.position == (<THREE.Mesh>inter.object).position) {
            con = c;
            break;
          }
        }

        const labelCylinder = document.createElement( 'div' );
        labelCylinder.className = 'badge bg-success text-wrap';
        labelCylinder.style.width = "6rem";
        let tempCon = 50;
        if(con?.strength != undefined) {
          tempCon = con?.strength / 2;
        }
        labelCylinder.textContent = "Strength: " + tempCon;
        const labelCylinderObject = new CSS2DObject( labelCylinder );
        labelCylinderObject.position.setX( 0 );
        labelCylinderObject.position.setY( 0 );
        labelCylinderObject.position.setZ( 4 );
        this.labelAdded.push(labelCylinderObject);
        
        (<THREE.Mesh>inter.object).add(labelCylinderObject);
      }
  }



  checkMouseInMinimap(event: MouseEvent) {
    if( ( event.clientX >= this.miniCamXMin && event.clientX <= this.miniCamXMax ) &&
    ( window.innerHeight - event.clientY >= this.miniCamYMin && window.innerHeight - event.clientY <= this.miniCamYMax ) ) {
     this.controls.enabled = false;
     this.controlsMiniMap.enabled = true;
   }
   else {
     this.controls.enabled = true;
     this.controlsMiniMap.enabled = false;
   }
  }

  checkWhichPlayerIs(mesh: THREE.Mesh): NetworkPlayer | null {
    for(let node of this.nodes) {
      if(node.sphere.position == mesh.position)
        return node;
    }
    return null;
  }

  resetColor() {
    (<THREE.MeshStandardMaterial>(<THREE.Mesh>this.objectPressed[0].object).material).color.set(0x2e86c1);
  }

  removeButtons() {
    for(let button of this.buttonAdded) {
      (<THREE.Mesh>this.objectPressed[0].object).remove(button);
      if((<THREE.Mesh>this.objectPressed[0].object).children.length > 1) {
        for(let i = 1; i < (<THREE.Mesh>this.objectPressed[0].object).children.length; i++) {
          (<THREE.Mesh>this.objectPressed[0].object).remove((<THREE.Mesh>this.objectPressed[0].object).children[i]);
        }
      }
    }
  }

  createEdge(con: NetworkConnection) {
    let indexPlayer = this.nodes.findIndex(x => x.id == con.player.id);
    let indexFriend = this.nodes.findIndex(x => x.id == con.friend.id);

    let position0 = this.nodes[indexPlayer].sphere.position;
    let position1 = this.nodes[indexFriend].sphere.position;

    let edgeGeometry = new THREE.CylinderGeometry(0.5, 0.5, 1.0);
    let edgeMaterial  = new THREE.MeshStandardMaterial({color : 0x80ffff,metalness: 0.2,roughness: 0.55,opacity: 1.0}) ;

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

    for(let connection of this.connections) {
      if(connection == con) {
        con.cylinder = cylinder;
        // Add it to the scene
        this.scene.add(connection.cylinder);
        this.cylinders.push(connection);
        break;
      }
    }
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
    this.labelRenderer.render( this.scene, this.camera );
    this.renderer.render( this.scene, this.camera );
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
