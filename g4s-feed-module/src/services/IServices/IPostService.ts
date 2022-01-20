import { Result } from "../../core/logic/Result";
import ICommentDTO from "../../dto/ICommentDTO";
import IDeleteCommentDTO from "../../dto/IDeleteCommentDTO";
import IPostDTO from "../../dto/IPostDTO";
import IReactionDTO from "../../dto/IReactionDTO";

export default interface IPostService  {
  createPost(postDTO: IPostDTO): Promise<Result<IPostDTO>>;
  updatePost(postDTO: IPostDTO): Promise<Result<IPostDTO>>;
  likePost(postDTO: IReactionDTO): Promise<Result<IPostDTO>>;
  unlikePost(postDTO: IReactionDTO): Promise<Result<IPostDTO>>;
  dislikePost(postDTO: IReactionDTO): Promise<Result<IPostDTO>>;
  undislikePost(postDTO: IReactionDTO): Promise<Result<IPostDTO>>;
  getPost (postId: string): Promise<Result<IPostDTO>>;
  commentPost(commentDTO: ICommentDTO): Promise<Result<IPostDTO>>;
  deleteComment(deleteCommentDTO: IDeleteCommentDTO): Promise<Result<IPostDTO>>;
  getPostsByUser(creatorId: string): Promise<Result<IPostDTO[]>>;
  getDCalc(emailA: string, emailB:string): Promise<Result<number>>;
}
