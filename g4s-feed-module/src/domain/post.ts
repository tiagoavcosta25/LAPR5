import { AggregateRoot } from "../core/domain/AggregateRoot";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";

import { Result } from "../core/logic/Result";
import { PostId } from "./postId";

import IPostDTO from "../dto/IPostDTO";
import { PostContent } from "./postContent";

interface PostProps {
  content: PostContent;
  creatorId: string;
  likes: string[];
  dislikes: string[];
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

  private constructor (props: PostProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (postDTO: IPostDTO, id?: UniqueEntityID): Result<Post> {
    const content = postDTO.content;
    const creatorId = postDTO.creatorId;
    const likes = postDTO.likes;
    const dislikes = postDTO.dislikes;

    console.log(postDTO);

    if (!!content === false || !!creatorId === false || content.length === 0) {
      return Result.fail<Post>('Must provide a post content and creator')
    } else {
      const resContent = PostContent.create(content);
      if(resContent.isSuccess){
        const post = new Post({ content: resContent.getValue(), creatorId: creatorId, likes: likes, dislikes: dislikes }, id);
        return Result.ok<Post>( post )
      }
      return Result.fail<Post>('Post content error')
    }
  }
}
