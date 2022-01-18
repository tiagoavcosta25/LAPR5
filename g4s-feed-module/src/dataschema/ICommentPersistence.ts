export interface ICommentPersistence {
	domainId: string;
	postId: string;
	creatorId: string;
	name: string;
    content: string;
	createdAt: Date;
}