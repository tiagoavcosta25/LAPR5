export interface ICommentPersistence {
	domainId: string;
	postId: string;
	creatorId: string;
	avatar: string;
	name: string;
    content: string;
	createdAt: Date;
}