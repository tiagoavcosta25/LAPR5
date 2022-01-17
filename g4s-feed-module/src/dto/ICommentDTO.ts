import { UniqueEntityID } from "../core/domain/UniqueEntityID";

export default interface ICommentDTO {
  id: string;
  postId: string;
  creatorId: string;
  content: string;
  createdAt: Date;
}
