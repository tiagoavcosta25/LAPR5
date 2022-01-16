import { AggregateRoot } from "../core/domain/AggregateRoot";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";

import { Result } from "../core/logic/Result";
import { PostId } from "./postId";

import IPostDTO from "../dto/IPostDTO";
import { PostContent } from "./postContent";
import { Comment } from "./comment";
import ICommentDTO from "../dto/ICommentDTO";
import { ICommentPersistence } from "../dataschema/ICommentPersistence";
import { IPostPersistence } from "../dataschema/IPostPersistence";

interface PostProps {
  content: PostContent;
  creatorId: string;
  likes: string[];
  dislikes: string[];
  tags: string[];
  comments: Comment[];
}

export class Post extends AggregateRoot<PostProps> {
  get id (): UniqueEntityID {
    return this._id;
  }

  get postId (): PostId {
    return new PostId(this.postId.toValue());
  }

  get content (): PostContent {
    return this.props.content;
  }

  set content ( value: PostContent) {
    this.props.content = value;
  }

  get creatorId (): string {
    return this.props.creatorId;
  }

  set creatorId ( value: string) {
    this.props.creatorId = value;
  }

  get likes (): string[] {
    return this.props.likes;
  }

  set likes ( value: string[]) {
    this.props.likes = value;
  }

  get dislikes (): string[] {
    return this.props.dislikes;
  }

  set dislikes ( value: string[]) {
    this.props.dislikes = value;
  }

  get tags (): string[] {
    return this.props.tags;
  }

  set tags ( value: string[]) {
    this.props.tags = value;
  }

  get comments (): Comment[] {
    return this.props.comments;
  }

  set comments ( value: Comment[]) {
    this.props.comments = value;
  }

  private constructor (props: PostProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (postDTO: IPostDTO | any, id?: UniqueEntityID): Result<Post> {
    const content = postDTO.content;
    const creatorId = postDTO.creatorId;
    let likes = postDTO.likes;
    let dislikes = postDTO.dislikes;
    const tags = postDTO.tags;
    let comments = postDTO.comments;
    let _comments = [];

    if(likes === undefined){
      likes = [];
    }

    if(dislikes === undefined){
      dislikes = [];
    }

    if(comments === undefined) {
      comments = [];
    } else {
      for(let comment of comments) {
        let newCommentProps = {
          postId: comment.postId,
          creatorId: comment.creatorId,
          content: comment.content
        } as ICommentDTO
        let newComment = Comment.create(newCommentProps, new UniqueEntityID(comment.domainId));
        if(newComment.isFailure) {
          return Result.fail<Post>('Invalid Comments on Post');
        }

        _comments.push(newComment.getValue());
      }
    }

    if (!!content === false || !!creatorId === false || content.length === 0) {
      return Result.fail<Post>('Must provide a post content and creator')
    } else {
      const resContent = PostContent.create(content);
      if(resContent.isSuccess){
        const post = new Post({ content: resContent.getValue(), creatorId: creatorId, likes: likes,
           dislikes: dislikes, tags: tags, comments: _comments }, id);
        return Result.ok<Post>( post )
      }
      return Result.fail<Post>('Post content error')
    }
  }
}
