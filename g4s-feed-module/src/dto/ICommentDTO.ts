import { UniqueEntityID } from "../core/domain/UniqueEntityID";

export default interface ICommentDTO {
  id: string;
  postId: string;
  creatorId: string;
  avatar: string;
  name: string;
  content: string;
  createdAt: Date;
}
