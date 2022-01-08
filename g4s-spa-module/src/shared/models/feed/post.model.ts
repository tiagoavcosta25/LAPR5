export class Post {
    id: string;
    content: string;
    creatorId: string;
    like: string[] = [];
    dislike: string[] = [];
    tags: string[] = [];
}
