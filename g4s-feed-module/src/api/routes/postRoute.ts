import { Router, Request, Response, NextFunction } from 'express';
import { Container } from 'typedi';

import PostService from '../../services/postService';
import { IPostDTO } from '../../dto/IPostDTO';

import middlewares from '../middlewares';
import { celebrate, Joi } from 'celebrate';
import winston = require('winston');

var post_controller = require('../../controllers/postController');

const route = Router();

export default (app: Router) => {
  app.use('/post', route);

  route.post(
    '/create',
    celebrate({
      body: Joi.object({
        content: Joi.string().required()
      }),
    }),
    async (req: Request, res: Response, next: NextFunction) => {
      const logger = Container.get('logger') as winston.Logger;
      logger.debug('Calling Create Post endpoint with body: %o', req.body )

      try {
        const postServiceInstance = Container.get(PostService);
        const postOrError = await postServiceInstance.createPost(req.body as IPostDTO);

        if (postOrError.isFailure) {
          logger.debug(postOrError.errorValue())
          return res.status(401).send(postOrError.errorValue());
        }
    
        const {postDTO, token} = postOrError.getValue();

        return res.status(201).json({ postDTO, token });
      } catch (e) {
        //logger.error('🔥 error: %o', e);
        return next(e);
      }
    },
  );
};
