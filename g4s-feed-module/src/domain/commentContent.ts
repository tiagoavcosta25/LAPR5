
import { ValueObject } from "../core/domain/ValueObject";
import { Result } from "../core/logic/Result";
import { Guard } from "../core/logic/Guard";

interface CommentContentProps {
  value: string;
}

export class CommentContent extends ValueObject<CommentContentProps> {
  get value (): string {
    return this.props.value;
  }
  
  private constructor (props: CommentContentProps) {
    super(props);
  }

  public static create (content: string): Result<CommentContent> {
    const guardResult = Guard.againstNullOrUndefined(content, 'content');
    if (!guardResult.succeeded) {
      return Result.fail<CommentContent>(guardResult.message);
    } else {
      return Result.ok<CommentContent>(new CommentContent({ value: content }))
    }
  }
}