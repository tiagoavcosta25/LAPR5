import { AggregateRoot } from "../core/domain/AggregateRoot";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Result } from "../core/logic/Result";
import { Guard } from "../core/logic/Guard";
import { PostId } from "./postId";
import { PostContent } from "./postContent";


interface PostProps {
  content: PostContent;
}

export class Post extends AggregateRoot<PostProps> {
  get id (): UniqueEntityID {
    return this._id;
  }

  get userId (): PostId {
    return PostId.caller(this.id);
  }

  get content (): PostContent {
    return this.props.content;
  }

  private constructor (props: PostProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (props: PostProps, id?: UniqueEntityID): Result<Post> {

    const guardedProps = [
      { argument: props.content, argumentName: 'content' }
    ];

    const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

    if (!guardResult.succeeded) {
      return Result.fail<Post>(guardResult.message)
    }     
    else {
      const user = new Post({
        ...props
      }, id);

      return Result.ok<Post>(user);
    }
  }
}