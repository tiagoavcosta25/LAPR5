import { ConnectionRequest } from "./connection-request.model";

export class IntroductionRequest extends ConnectionRequest {
    middleMan: string;
    playerToMiddleManMessage: string;
    middleManToTargetMessage: string;
}
