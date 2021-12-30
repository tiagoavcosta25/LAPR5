import { NetworkPlayer } from "./network-player.dto";

export class NetworkScope {
    player: NetworkPlayer;
    friends: NetworkPlayer[];
    scope: number;

    constructor(player: NetworkPlayer, scope: number) {
        this.player = player;
        this.friends = [];
        this.scope = scope;
    }
}
