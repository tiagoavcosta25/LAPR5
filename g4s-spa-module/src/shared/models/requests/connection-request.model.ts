export abstract class ConnectionRequest {
    id: string;
    player: string;
    target: string;
    playerToTargetMessage: string;
    currentStatus: string;
    strength: number;    
    tags: string[];
}
