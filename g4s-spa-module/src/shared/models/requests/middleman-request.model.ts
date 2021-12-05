import { Player } from "../player/player.model";

export abstract class MiddleManRequest {
    id: string;
    player: Player;
    target: Player;
    playerToMiddleManMessage: string;
}
