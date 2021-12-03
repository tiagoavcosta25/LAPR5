export class ConnectionRequest {
    id: string;
    player: string;
    target: string;
    playerToTargetMessage: string;
    currentStatus: string;
    strength: number;    
    tags: string[];
    middleMan: string;
    playerToMiddleManMessage: string;
    middleManToTargetMessage: string;
}
