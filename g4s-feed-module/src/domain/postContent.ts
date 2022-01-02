
import { ValueObject } from "../core/domain/ValueObject";
import { Result } from "../core/logic/Result";
import { Guard } from "../core/logic/Guard";

interface PostContentProps {
  value: string;
}

export class PostContent extends ValueObject<PostContentProps> {
  get value (): string {
    return this.props.value;
  }
  
  private constructor (props: PostContentProps) {
    super(props);
  }

  public static create (content: string): Result<PostContent> {
    const guardResult = Guard.againstNullOrUndefined(content, 'content');
    if (!guardResult.succeeded) {
      return Result.fail<PostContent>(guardResult.message);
    } else {
      return Result.ok<PostContent>(new PostContent({ value: content }))
    }
  }
}