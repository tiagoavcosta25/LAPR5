import { Request, Response, NextFunction } from 'express';

export default interface IPostController  {
  createPost(req: Request, res: Response, next: NextFunction);
  updatePost(req: Request, res: Response, next: NextFunction);

  commentPost(req: Request, res: Response, next: NextFunction);
  deleteComment(req: Request, res: Response, next: NextFunction);

  likePost(req: Request, res: Response, next: NextFunction);
  unlikePost(req: Request, res: Response, next: NextFunction);

  dislikePost(req: Request, res: Response, next: NextFunction);
  undislikePost(req: Request, res: Response, next: NextFunction);

  getPostsByUser(req: Request, res: Response, next: NextFunction);
  getDCalc(req: Request, res: Response, next: NextFunction);
  getDCalcId(req: Request, res: Response, next: NextFunction);
}