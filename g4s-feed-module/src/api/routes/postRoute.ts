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
        creatorId: Joi.string().required()
      })
    }),
    (req, res, next) => ctrl.createPost(req, res, next) );

  route.put('',
    celebrate({
      body: Joi.object({
        id: Joi.string().required(),
        content: Joi.string().required(),
        creatorId: Joi.string().required()
      }),
    }),
    (req, res, next) => ctrl.updatePost(req, res, next) );
};
