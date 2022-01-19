import * as THREE from "three";
import { Player } from "../player/player.model";

export class NetworkPlayer {
    id: string;
    email: string;
    name: string;
    dateOfBirth: string;
    phoneNumber: number;
    emotionalStatus: string;
    tags: string[] = [];
    sphere: THREE.Mesh;

    constructor(id: string) {
        this.id = id;
    }

    setInfo(player: Player) {
        this.email = player.email;
        this.name = player.name;
        let date = new Date(player.dateOfBirth);
        this.dateOfBirth = date.getDate().toString().padStart(2, "0") + "-" + (date.getMonth() + 1).toString().padStart(2, "0") + "-" + date.getFullYear();
        this.phoneNumber = player.phoneNumber;
        this.emotionalStatus = player.emotionalStatus;
        this.tags = player.tags;
    } 

    setMesh(geometry: THREE.SphereGeometry, material: THREE.MeshStandardMaterial) {
        this.sphere = new THREE.Mesh(geometry, material);
    }
}
