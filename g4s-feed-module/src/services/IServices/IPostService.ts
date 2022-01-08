import { Result } from "../../core/logic/Result";
import IPostDTO from "../../dto/IPostDTO";
import IReactionDTO from "../../dto/IReactionDTO";

export default interface IPostService  {
  createPost(postDTO: IPostDTO): Promise<Result<IPostDTO>>;
  updatePost(postDTO: IPostDTO): Promise<Result<IPostDTO>>;
  likePost(postDTO: IReactionDTO): Promise<Result<IPostDTO>>;
  dislikePost(postDTO: IReactionDTO): Promise<Result<IPostDTO>>;

  getPost (postId: string): Promise<Result<IPostDTO>>;
}
