import expressLoader from './express';
import dependencyInjectorLoader from './dependencyInjector';
import mongooseLoader from './mongoose';
import Logger from './logger';

import config from '../../config';

export default async ({ expressApp }) => {
  const mongoConnection = await mongooseLoader();
  Logger.info('✌️ DB loaded and connected!');

  const userSchema = {
    // compare with the approach followed in repos and services
    name: 'userSchema',
    schema: '../persistence/schemas/userSchema',
  };

  const roleSchema = {
    // compare with the approach followed in repos and services
    name: 'roleSchema',
    schema: '../persistence/schemas/roleSchema',
  };

  const roleController = {
    name: config.controllers.role.name,
    path: config.controllers.role.path
  }

  const roleRepo = {
    name: config.repos.role.name,
    path: config.repos.role.path
  }

  const postSchema = {
    // compare with the approach followed in repos and services
    name: 'postSchema',
    schema: '../persistence/schemas/postSchema',
  };

  const postController = {
    name: config.controllers.post.name,
    path: config.controllers.post.path
  }

  const postRepo = {
    name: config.repos.post.name,
    path: config.repos.post.path
  }

  const commentSchema = {
    // compare with the approach followed in repos and services
    name: 'commentSchema',
    schema: '../persistence/schemas/commentSchema',
  };

  const userRepo = {
    name: config.repos.user.name,
    path: config.repos.user.path
  }

  const roleService = {
    name: config.services.role.name,
    path: config.services.role.path
  }

  const postService = {
    name: config.services.post.name,
    path: config.services.post.path
  }

  await dependencyInjectorLoader({
    mongoConnection,
    schemas: [
      userSchema,
      roleSchema,
      postSchema,
      commentSchema
    ],
    controllers: [
      roleController,
      postController
    ],
    repos: [
      roleRepo,
      userRepo,
      postRepo
    ],
    services: [
      roleService,
      postService
    ]
  });
  Logger.info('✌️ Schemas, Controllers, Repositories, Services, etc. loaded');

  await expressLoader({ app: expressApp });
  Logger.info('✌️ Express loaded');
};
