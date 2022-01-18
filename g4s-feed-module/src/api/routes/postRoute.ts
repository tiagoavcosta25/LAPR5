import { Router } from 'express';
import { celebrate, Joi } from 'celebrate';

import { Container } from 'typedi';
import IPostController from '../../controllers/IControllers/IPostController'; 

import config from "../../../config";

const route = Router();

export default (app: Router) => {
  
  app.use('/post', route);

  const ctrl = Container.get(config.controllers.post.name) as IPostController;

  route.post('',
    celebrate({
      body: Joi.object({
        content: Joi.string().required(),
        creatorId: Joi.string().required(),
        name: Joi.string().required(),
        tags: Joi.array()
      })
    }),
    (req, res, next) => ctrl.createPost(req, res, next) );

  route.put('',
    celebrate({
      body: Joi.object({
        id: Joi.string().required(),
        content: Joi.string().required(),
        creatorId: Joi.string().required(),
        name: Joi.string().required()
      }),
    }),
    (req, res, next) => ctrl.updatePost(req, res, next) );

  route.patch('/like',
    celebrate({
      body: Joi.object({
        postId: Joi.string().required(),
        playerId: Joi.string().required()
      }),
    }),
    (req, res, next) => ctrl.likePost(req, res, next) );

  route.patch('/dislike',
    celebrate({
      body: Joi.object({
        postId: Joi.string().required(),
        playerId: Joi.string().required()
      }),
    }),
    (req, res, next) => ctrl.dislikePost(req, res, next) );

  route.get('/:creatorId',
    (req, res, next) => ctrl.getPostsByUser(req, res, next) );
};
