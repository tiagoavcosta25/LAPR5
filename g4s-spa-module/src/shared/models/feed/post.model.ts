import { Comment } from "../posts/comment.model";

export class Post {
    id: string;
    content: string;
    creatorId: string;
    likes: string[] = [];
    dislikes: string[] = [];
    tags: string[] = [];
    comments: Comment[];
    createdAt: Date;
}
