import { ICommentPersistence } from "./ICommentPersistence";

export interface IPostPersistence {
	domainId: string;
	content: string;
	creatorId: string;
	creatorEmail: string;
	avatar: string;
	name: string;
	likes: string[];
	dislikes: string[];
	tags: string[];
	comments: ICommentPersistence[];
	createdAt: Date;
}