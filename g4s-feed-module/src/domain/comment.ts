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
  avatar: string;
  name: string;
  createdAt: Date;
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

  get avatar (): string {
    return this.props.avatar;
  }

  set avatar ( value: string) {
    this.props.avatar = value;
  }

  get name (): string {
    return this.props.name;
  }

  set name ( value: string) {
    this.props.name = value;
  }

  get postId (): string {
    return this.props.postId;
  }

  set postId ( value: string) {
    this.props.postId = value;
  }

  get createdAt (): Date {
    return this.props.createdAt;
  }

  set createdAt ( value: Date) {
    this.props.createdAt = value;
  }

  private constructor (props: CommentProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (commentDTO: ICommentDTO, id?: UniqueEntityID): Result<Comment> {
    const postId = commentDTO.postId;
    const content = commentDTO.content;
    const creatorId = commentDTO.creatorId;
    const avatar = commentDTO.avatar;
    const name = commentDTO.name;
    const createdAt = commentDTO.createdAt;

    if (!!postId === false || !!content === false || !!creatorId === false || !!avatar === false || !!name === false || content.length === 0) {
      return Result.fail<Comment>('Must provide comment content, creator and postId')
    } else {
      const resContent = CommentContent.create(content);
      if(resContent.isSuccess){
        const comment = new Comment({ postId: postId, content: resContent.getValue(), creatorId: creatorId, avatar: avatar, name: name, createdAt: createdAt }, id);
        return Result.ok<Comment>( comment )
      }
      return Result.fail<Comment>('Comment content error')
    }
  }
}
