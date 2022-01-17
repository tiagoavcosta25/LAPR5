export class CreateComment {
    postId: string;
    creatorId: string;
    content: string;

    constructor(postId: string, creatorId: string, content: string) {
        this.postId = postId;
        this.creatorId = creatorId;
        this.content = content;
    }
}
