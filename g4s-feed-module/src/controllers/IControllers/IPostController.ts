import { Request, Response, NextFunction } from 'express';

export default interface IRoleController  {
  createPost(req: Request, res: Response, next: NextFunction);
  updatePost(req: Request, res: Response, next: NextFunction);
}