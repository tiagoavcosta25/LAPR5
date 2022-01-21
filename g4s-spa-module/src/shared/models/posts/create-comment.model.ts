export class CreateComment {
    postId: string;
    creatorId: string;
    avatar: string;
    name: string;
    content: string;

    constructor(postId: string, creatorId: string, avatar:string, name: string, content: string) {
        this.postId = postId;
        this.creatorId = creatorId;
        this.avatar = avatar;
        this.content = content;
        this.name = name;
    }
}
