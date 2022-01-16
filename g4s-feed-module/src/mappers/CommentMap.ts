import { Mapper } from "../core/infra/Mapper";

import ICommentDTO from "../dto/ICommentDTO";
import { Comment } from "../domain/comment";

export class CommentMap extends Mapper<Comment> {
  
  public static toDTO( comment: Comment, postId:string ): ICommentDTO {
    return {
      id: comment.id.toString(),
      postId: postId,
      creatorId: comment.creatorId,
      content: comment.content.value,
    } as ICommentDTO;
  }

  public static toPersistence (comment: Comment): any {
    return {
      domainId: comment.id.toString(),
      postId: comment.postId.toString(),
      creatorId: comment.creatorId.toString(),
      content: comment.content.value.toString(),
    }
  }
}