import { Player } from "../player/player.model";

export abstract class TargetPendingRequest {
    id: string;
    player: Player;
    target: Player;
    playerToTargetMessage: string;
}
