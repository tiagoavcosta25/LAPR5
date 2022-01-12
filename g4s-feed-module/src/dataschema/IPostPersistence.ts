export interface IPostPersistence {
	domainId: string;
	content: string;
	creatorId: string;
	likes: string[];
	dislikes: string[];
	tags: string[];
}