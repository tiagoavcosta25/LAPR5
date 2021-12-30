import { NetworkPlayer } from "./network-player.dto";

export class NetworkConnection {
    id: string;
    player: NetworkPlayer;
    friend: NetworkPlayer;

    constructor(id:string, playerId:string, friendId:string) {
        this.id = id;
        this.player = new NetworkPlayer(playerId);
        this.friend = new NetworkPlayer(friendId);
    }
}
