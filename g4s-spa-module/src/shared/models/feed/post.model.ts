import { Comment } from "../posts/comment.model";

export class Post {
    id: string;
    content: string;
    creatorId: string;
    creatorEmail: string;
    avatar: string;
    name: string;
    likes: string[] = [];
    dislikes: string[] = [];
    tags: string[] = [];
    comments: Comment[];
    createdAt: Date;
}
