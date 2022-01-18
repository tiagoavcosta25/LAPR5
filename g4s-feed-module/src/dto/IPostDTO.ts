import ICommentDTO from "./ICommentDTO";

export default interface IPostDTO {
  id: string;
  content: string;
  creatorId: string;
  name: string;
	likes: string[];
	dislikes: string[];
	tags: string[];
  comments: ICommentDTO[];
  createdAt: Date;
}
