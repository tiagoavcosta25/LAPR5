import { Entity } from "../core/domain/Entity";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";

import { Result } from "../core/logic/Result";
import { CommentId } from "./commentId";

import ICommentDTO from "../dto/ICommentDTO";
import { CommentContent } from "./commentContent";

interface CommentProps {
  postId: string;
  content: CommentContent;
  creatorId: string;
}

export class Comment extends Entity<CommentProps> {
  get id (): UniqueEntityID {
    return this._id;
  }

  get commentId (): CommentId {
    return new CommentId(this.commentId.toValue());
  }

  get content (): CommentContent {
    return this.props.content;
  }

  set content ( value: CommentContent) {
    this.props.content = value;
  }

  get creatorId (): string {
    return this.props.creatorId;
  }

  set creatorId ( value: string) {
    this.props.creatorId = value;
  }

  get postId (): string {
    return this.props.postId;
  }

  set postId ( value: string) {
    this.props.postId = value;
  }

  private constructor (props: CommentProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (commentDTO: ICommentDTO, id?: UniqueEntityID): Result<Comment> {
    const postId = commentDTO.postId;
    const content = commentDTO.content;
    const creatorId = commentDTO.creatorId;

    if (!!postId === false || !!content === false || !!creatorId === false || content.length === 0) {
      return Result.fail<Comment>('Must provide comment content, creator and postId')
    } else {
      const resContent = CommentContent.create(content);
      if(resContent.isSuccess){
        const comment = new Comment({ postId: postId, content: resContent.getValue(), creatorId: creatorId }, id);
        return Result.ok<Comment>( comment )
      }
      return Result.fail<Comment>('Comment content error')
    }
  }
}
