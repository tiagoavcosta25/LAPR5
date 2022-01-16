import { Router } from 'express';
import { celebrate, Joi } from 'celebrate';

import { Container } from 'typedi';
import IPostController from '../../controllers/IControllers/IPostController'; 

import config from "../../../config";

const route = Router();

export default (app: Router) => {
  app.use('/post', route);

  const ctrl = Container.get(config.controllers.post.name) as IPostController;

  route.patch('/addcomment',
    celebrate({
      body: Joi.object({
        postId: Joi.string().required(),
        creatorId: Joi.string().required(),
        content: Joi.string().required()
      })
    }),
    (req, res, next) => ctrl.commentPost(req, res, next) );

    route.patch('/deletecomment',
    celebrate({
      body: Joi.object({
        id: Joi.string().required(),
        postId: Joi.string().required()
      })
    }),
    (req, res, next) => ctrl.deleteComment(req, res, next) );
};


