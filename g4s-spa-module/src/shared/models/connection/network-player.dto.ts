import * as THREE from "three";

export class NetworkPlayer {
    id: string;
    email: string;
    name: string;
    sphere: THREE.Mesh;

    constructor(id: string) {
        this.id = id;
    }

    setEmailAndName(email: string, name: string) {
        this.email = email;
        this.name = name;
    } 

    setMesh(geometry: THREE.SphereGeometry, material: THREE.MeshStandardMaterial) {
        this.sphere = new THREE.Mesh(geometry, material);
    }
}
