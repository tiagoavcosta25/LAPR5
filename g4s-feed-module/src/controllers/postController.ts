import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";

import IPostController from "./IControllers/IPostController";
import IPostService from '../services/IServices/IPostService';
import IPostDTO from '../dto/IPostDTO';

import { Result } from "../core/logic/Result";
import IReactionDTO from '../dto/IReactionDTO';
import ICommentDTO from '../dto/ICommentDTO';
import IDeleteCommentDTO from '../dto/IDeleteCommentDTO';

@Service()
export default class PostController implements IPostController /* TODO: extends ../core/infra/BaseController */ {
  constructor(
      @Inject(config.services.post.name) private postServiceInstance : IPostService
  ) {}

  public async createPost(req: Request, res: Response, next: NextFunction) {
    try {
      const postOrError = await this.postServiceInstance.createPost(req.body as IPostDTO) as Result<IPostDTO>;
        
      if (postOrError.isFailure) {
        return res.status(402).send();
      }

      const postDTO = postOrError.getValue();
      return res.json( postDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  };

  public async updatePost(req: Request, res: Response, next: NextFunction) {
    try {
      const postOrError = await this.postServiceInstance.updatePost(req.body as IPostDTO) as Result<IPostDTO>;

      if (postOrError.isFailure) {
        return res.status(404).send();
      }

      const postDTO = postOrError.getValue();
      return res.json( postDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  };

  public async likePost(req: Request, res: Response, next: NextFunction) {
    try {
      const postOrError = await this.postServiceInstance.likePost(req.body as IReactionDTO) as Result<IPostDTO>;

      if (postOrError.isFailure) {
        return res.status(404).send();
      }

      const postDTO = postOrError.getValue();
      return res.json( postDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  };

  public async unlikePost(req: Request, res: Response, next: NextFunction) {
    try {
      const postOrError = await this.postServiceInstance.unlikePost(req.body as IReactionDTO) as Result<IPostDTO>;

      if (postOrError.isFailure) {
        return res.status(404).send();
      }

      const postDTO = postOrError.getValue();
      return res.json( postDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  };

  public async dislikePost(req: Request, res: Response, next: NextFunction) {
    try {
      const postOrError = await this.postServiceInstance.dislikePost(req.body as IReactionDTO) as Result<IPostDTO>;

      if (postOrError.isFailure) {
        return res.status(404).send();
      }

      const postDTO = postOrError.getValue();
      return res.json( postDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  };

  public async undislikePost(req: Request, res: Response, next: NextFunction) {
    try {
      const postOrError = await this.postServiceInstance.undislikePost(req.body as IReactionDTO) as Result<IPostDTO>;

      if (postOrError.isFailure) {
        return res.status(404).send();
      }

      const postDTO = postOrError.getValue();
      return res.json( postDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  };

  public async commentPost(req: Request, res: Response, next: NextFunction) {
    try {
      const postOrError = await this.postServiceInstance.commentPost(req.body as ICommentDTO) as Result<IPostDTO>;

      if (postOrError.isFailure) {
        return res.status(404).send();
      }

      const postDTO = postOrError.getValue();
      return res.json( postDTO ).status(201);
    }
    catch (e) {
      return next(e);
    }
  };

  public async deleteComment(req: Request, res: Response, next: NextFunction) {
    try {
      const postOrError = await this.postServiceInstance.deleteComment(req.body as IDeleteCommentDTO) as Result<IPostDTO>;

      if (postOrError.isFailure) {
        return res.status(404).send();
      }

      const postDTO = postOrError.getValue();
      return res.json( postDTO ).status(200);
    }
    catch (e) {
      return next(e);
    }
  };

  public async getPostsByUser(req: Request, res: Response, next: NextFunction) {
    try {
      const postOrError = await this.postServiceInstance.getPostsByUser(req.params.creatorId) as Result<IPostDTO[]>;
      if (postOrError.isFailure) {
        return res.send().status(404);
      }

      let listResponse = [];

      for(let p of postOrError.getValue()) {
        listResponse.push(p);
      }

      return res.json( listResponse ).status(200);
    }
    catch (e) {
      return next(e);
    }
  }

  public async getDCalc(req: Request, res: Response, next: NextFunction) {
    try {
      const count = await this.postServiceInstance.getDCalc(req.params.idA, req.params.idB) as Result<number>;
      if (count.isFailure) {
        return res.status(404).send();
      }

      return res.status(200).json({ dCalc: count.getValue() });
    }
    catch (e) {
      return next(e);
    }
  }

}