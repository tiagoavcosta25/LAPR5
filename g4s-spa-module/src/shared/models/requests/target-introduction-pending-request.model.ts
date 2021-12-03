import { Player } from "../player/player.model";
import { TargetPendingRequest } from "./target-pending-request.model";

export class TargetIntroductionPendingRequest extends TargetPendingRequest {
    middleMan: Player;
    middleManToTargetMessage: string;
}
