import * as THREE from "three";
import { NetworkPlayer } from "./network-player.dto";

export class NetworkConnection {
    id: string;
    player: NetworkPlayer;
    friend: NetworkPlayer;
    strength: number;
    cylinder: THREE.Mesh;
    isPath: boolean = false;

    constructor(id:string, playerId:string, friendId:string, strength: number) {
        this.id = id;
        this.player = new NetworkPlayer(playerId);
        this.friend = new NetworkPlayer(friendId);
        this.strength = strength * 2;
        this.isPath = false;
    }

    setPath(value: boolean) {
        this.isPath = value;
    }

    checkIsPath() : boolean{
        return this.isPath;
    }
}
