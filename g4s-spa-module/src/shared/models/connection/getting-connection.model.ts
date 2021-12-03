import { Player } from "../player/player.model";

export class GettingConnection {
    id: string;
    player: Player;
    friend: Player;
    connectionStrength: number;
    tags: string[];
}
