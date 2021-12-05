import { Player } from "../player/player.model";
import { MiddleManRequest } from "./middleman-request.model";

export class MiddleManIntroductionRequest extends MiddleManRequest {
    middleMan: Player;
    middleManToTargetMessage: string;
}
