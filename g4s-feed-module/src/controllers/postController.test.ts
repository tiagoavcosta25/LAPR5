import * as sinon from 'sinon';

import { Response, Request, NextFunction } from 'express';

import { Container } from 'typedi';
import config from '../../config';

import { Result } from '../core/logic/Result';

import IPostDTO from '../dto/IPostDTO';
import PostController from './postController';
import IPostService from '../services/IServices/IPostService';
import 'reflect-metadata';
import { stringify } from 'querystring';

describe('post controller', function() {
  beforeEach(function() {
  });

  it('createPost: returns json post content', async function() {
    let body = {    id: 'idPost',
                    content: 'This is a post',
                    creatorId: 'idCreator',
                    creatorEmail: 'emailCreator',
                    avatar: 'avatar',
                    name: 'name',
                    likes: [
                        'idLike'
                    ],
                    dislikes: [
                        'idDislike'
                    ],
                    tags: [
                        'tag'
                    ],
                    comments: [
                        {
                            id: 'idComment',
                            postId: 'idPost',
                            creatorId: 'idCreator',
                            avatar: 'avatar',
                            name: 'name',
                            content: 'content',
                            createdAt: 'createdAt'
                        }
                    ],
                    createdAt: null
                    };
    let req: Partial<Request> = {};
    req.body = body;

    let res: Partial<Response> = {
      json: sinon.spy(),
    };
    let next: Partial<NextFunction> = () => {};

    let postSchemaInstance = require('../persistence/schemas/postSchema').default;
    Container.set('postSchema', postSchemaInstance);

    let postRepoClass = require(config.repos.post.path).default;
    let postRepoInstance = Container.get(postRepoClass);
    Container.set('PostRepo', postRepoInstance);

    let postServiceClass = require(config.services.post.path).default;
    let postServiceInstance = Container.get(postServiceClass);
    Container.set(config.services.post.name, postServiceInstance);
    postServiceInstance = Container.get(config.services.post.name);
    sinon.stub(postServiceInstance, 'createPost').returns(
      Result.ok<IPostDTO>({ id: 'idPost',
      content: 'This is a post',
      creatorId: 'idCreator',
      creatorEmail: 'emailCreator',
      avatar: 'avatar',
      name: 'name',
      likes: [
          'idLike'
      ],
      dislikes: [
          'idDislike'
      ],
      tags: [
          'tag'
      ],
      comments: [
          {
              id: 'idComment',
              postId: 'idPost',
              creatorId: 'idCreator',
              avatar: 'avatar',
              name: 'name',
              content: 'content',
              createdAt: null
          }
      ],
      createdAt: null
    }));

    const ctrl = new PostController(postServiceInstance as IPostService);

    await ctrl.createPost(<Request>req, <Response>res, <NextFunction>next);

    sinon.assert.calledOnce(res.json);
    sinon.assert.calledWith(res.json, sinon.match({ id: 'idPost',
    content: 'This is a post',
    creatorId: 'idCreator',
    creatorEmail: 'emailCreator',
    avatar: 'avatar',
    name: 'name',
    likes: [
        'idLike'
    ],
    dislikes: [
        'idDislike'
    ],
    tags: [
        'tag'
    ],
    comments: [
        {
            id: 'idComment',
            postId: 'idPost',
            creatorId: 'idCreator',
            avatar: 'avatar',
            name: 'name',
            content: 'content',
            createdAt: null
        }
    ],
    createdAt: null  }),
    );
  }
  )

  it('updatePost: returns json post content', async function() {
    let body = {    id: 'idPost',
                    content: 'This is a post',
                    creatorId: 'idCreator',
                    creatorEmail: 'emailCreator',
                    avatar: 'avatar',
                    name: 'name',
                    likes: [
                        'idLike'
                    ],
                    dislikes: [
                        'idDislike'
                    ],
                    tags: [
                        'tag'
                    ],
                    comments: [
                        {
                            id: 'idComment',
                            postId: 'idPost',
                            creatorId: 'idCreator',
                            avatar: 'avatar',
                            name: 'name',
                            content: 'content',
                            createdAt: 'createdAt'
                        }
                    ],
                    createdAt: null
                    };
    let req: Partial<Request> = {};
    req.body = body;

    let res: Partial<Response> = {
      json: sinon.spy(),
    };
    let next: Partial<NextFunction> = () => {};

    let postSchemaInstance = require('../persistence/schemas/postSchema').default;
    Container.set('postSchema', postSchemaInstance);

    let postRepoClass = require(config.repos.post.path).default;
    let postRepoInstance = Container.get(postRepoClass);
    Container.set('PostRepo', postRepoInstance);

    let postServiceClass = require(config.services.post.path).default;
    let postServiceInstance = Container.get(postServiceClass);
    Container.set(config.services.post.name, postServiceInstance);
    postServiceInstance = Container.get(config.services.post.name);
    sinon.stub(postServiceInstance, 'updatePost').returns(
      Result.ok<IPostDTO>({ id: 'idPost',
      content: 'This is a post',
      creatorId: 'idCreator',
      creatorEmail: 'emailCreator',
      avatar: 'avatar',
      name: 'name',
      likes: [
          'idLike'
      ],
      dislikes: [
          'idDislike'
      ],
      tags: [
          'tag'
      ],
      comments: [
          {
              id: 'idComment',
              postId: 'idPost',
              creatorId: 'idCreator',
              avatar: 'avatar',
              name: 'name',
              content: 'content',
              createdAt: null
          }
      ],
      createdAt: null
    }));

    const ctrl = new PostController(postServiceInstance as IPostService);

    await ctrl.updatePost(<Request>req, <Response>res, <NextFunction>next);

    sinon.assert.calledOnce(res.json);
    sinon.assert.calledWith(res.json, sinon.match({ id: 'idPost',
    content: 'This is a post',
    creatorId: 'idCreator',
    creatorEmail: 'emailCreator',
    avatar: 'avatar',
    name: 'name',
    likes: [
        'idLike'
    ],
    dislikes: [
        'idDislike'
    ],
    tags: [
        'tag'
    ],
    comments: [
        {
            id: 'idComment',
            postId: 'idPost',
            creatorId: 'idCreator',
            avatar: 'avatar',
            name: 'name',
            content: 'content',
            createdAt: null
        }
    ],
    createdAt: null  }),
    );
  }
  )

  it('likePost: returns json post content', async function() {
    let body = {    postId: 'postId',
                    playerEmail: 'playerEmail'
                    };
    let req: Partial<Request> = {};
    req.body = body;

    let res: Partial<Response> = {
      json: sinon.spy(),
    };
    let next: Partial<NextFunction> = () => {};

    let postSchemaInstance = require('../persistence/schemas/postSchema').default;
    Container.set('postSchema', postSchemaInstance);

    let postRepoClass = require(config.repos.post.path).default;
    let postRepoInstance = Container.get(postRepoClass);
    Container.set('PostRepo', postRepoInstance);

    let postServiceClass = require(config.services.post.path).default;
    let postServiceInstance = Container.get(postServiceClass);
    Container.set(config.services.post.name, postServiceInstance);
    postServiceInstance = Container.get(config.services.post.name);
    sinon.stub(postServiceInstance, 'likePost').returns(
      Result.ok<IPostDTO>({ id: 'idPost',
      content: 'This is a post',
      creatorId: 'idCreator',
      creatorEmail: 'emailCreator',
      avatar: 'avatar',
      name: 'name',
      likes: [
          'playerEmail'
      ],
      dislikes: [
          'idDislike'
      ],
      tags: [
          'tag'
      ],
      comments: [
          {
              id: 'idComment',
              postId: 'idPost',
              creatorId: 'idCreator',
              avatar: 'avatar',
              name: 'name',
              content: 'content',
              createdAt: null
          }
      ],
      createdAt: null
    }));

    const ctrl = new PostController(postServiceInstance as IPostService);

    await ctrl.likePost(<Request>req, <Response>res, <NextFunction>next);

    sinon.assert.calledOnce(res.json);
    sinon.assert.calledWith(res.json, sinon.match({ id: 'idPost',
    content: 'This is a post',
    creatorId: 'idCreator',
    creatorEmail: 'emailCreator',
    avatar: 'avatar',
    name: 'name',
    likes: [
        'playerEmail'
    ],
    dislikes: [
        'idDislike'
    ],
    tags: [
        'tag'
    ],
    comments: [
        {
            id: 'idComment',
            postId: 'idPost',
            creatorId: 'idCreator',
            avatar: 'avatar',
            name: 'name',
            content: 'content',
            createdAt: null
        }
    ],
    createdAt: null  }),
    );
  }
  )
  
  it('unlikePost: returns json post content', async function() {
    let body = {    postId: 'postId',
                    playerEmail: 'playerEmail'
                    };
    let req: Partial<Request> = {};
    req.body = body;

    let res: Partial<Response> = {
      json: sinon.spy(),
    };
    let next: Partial<NextFunction> = () => {};

    let postSchemaInstance = require('../persistence/schemas/postSchema').default;
    Container.set('postSchema', postSchemaInstance);

    let postRepoClass = require(config.repos.post.path).default;
    let postRepoInstance = Container.get(postRepoClass);
    Container.set('PostRepo', postRepoInstance);

    let postServiceClass = require(config.services.post.path).default;
    let postServiceInstance = Container.get(postServiceClass);
    Container.set(config.services.post.name, postServiceInstance);
    postServiceInstance = Container.get(config.services.post.name);
    sinon.stub(postServiceInstance, 'unlikePost').returns(
      Result.ok<IPostDTO>({ id: 'idPost',
      content: 'This is a post',
      creatorId: 'idCreator',
      creatorEmail: 'emailCreator',
      avatar: 'avatar',
      name: 'name',
      likes: [
          'playerEmail'
      ],
      dislikes: [
          'idDislike'
      ],
      tags: [
          'tag'
      ],
      comments: [
          {
              id: 'idComment',
              postId: 'idPost',
              creatorId: 'idCreator',
              avatar: 'avatar',
              name: 'name',
              content: 'content',
              createdAt: null
          }
      ],
      createdAt: null
    }));

    const ctrl = new PostController(postServiceInstance as IPostService);

    await ctrl.unlikePost(<Request>req, <Response>res, <NextFunction>next);

    sinon.assert.calledOnce(res.json);
    sinon.assert.calledWith(res.json, sinon.match({ id: 'idPost',
    content: 'This is a post',
    creatorId: 'idCreator',
    creatorEmail: 'emailCreator',
    avatar: 'avatar',
    name: 'name',
    likes: [
        'playerEmail'
    ],
    dislikes: [
        'idDislike'
    ],
    tags: [
        'tag'
    ],
    comments: [
        {
            id: 'idComment',
            postId: 'idPost',
            creatorId: 'idCreator',
            avatar: 'avatar',
            name: 'name',
            content: 'content',
            createdAt: null
        }
    ],
    createdAt: null  }),
    );
  }
  )

  it('dislikePost: returns json post content', async function() {
    let body = {    postId: 'postId',
                    playerEmail: 'playerEmail'
                    };
    let req: Partial<Request> = {};
    req.body = body;

    let res: Partial<Response> = {
      json: sinon.spy(),
    };
    let next: Partial<NextFunction> = () => {};

    let postSchemaInstance = require('../persistence/schemas/postSchema').default;
    Container.set('postSchema', postSchemaInstance);

    let postRepoClass = require(config.repos.post.path).default;
    let postRepoInstance = Container.get(postRepoClass);
    Container.set('PostRepo', postRepoInstance);

    let postServiceClass = require(config.services.post.path).default;
    let postServiceInstance = Container.get(postServiceClass);
    Container.set(config.services.post.name, postServiceInstance);
    postServiceInstance = Container.get(config.services.post.name);
    sinon.stub(postServiceInstance, 'dislikePost').returns(
      Result.ok<IPostDTO>({ id: 'idPost',
      content: 'This is a post',
      creatorId: 'idCreator',
      creatorEmail: 'emailCreator',
      avatar: 'avatar',
      name: 'name',
      likes: [
          'playerEmail'
      ],
      dislikes: [
          'idDislike'
      ],
      tags: [
          'tag'
      ],
      comments: [
          {
              id: 'idComment',
              postId: 'idPost',
              creatorId: 'idCreator',
              avatar: 'avatar',
              name: 'name',
              content: 'content',
              createdAt: null
          }
      ],
      createdAt: null
    }));

    const ctrl = new PostController(postServiceInstance as IPostService);

    await ctrl.dislikePost(<Request>req, <Response>res, <NextFunction>next);

    sinon.assert.calledOnce(res.json);
    sinon.assert.calledWith(res.json, sinon.match({ id: 'idPost',
    content: 'This is a post',
    creatorId: 'idCreator',
    creatorEmail: 'emailCreator',
    avatar: 'avatar',
    name: 'name',
    likes: [
        'playerEmail'
    ],
    dislikes: [
        'idDislike'
    ],
    tags: [
        'tag'
    ],
    comments: [
        {
            id: 'idComment',
            postId: 'idPost',
            creatorId: 'idCreator',
            avatar: 'avatar',
            name: 'name',
            content: 'content',
            createdAt: null
        }
    ],
    createdAt: null  }),
    );
  }
  )

  it('undislikePost: returns json post content', async function() {
    let body = {    postId: 'postId',
                    playerEmail: 'playerEmail'
                    };
    let req: Partial<Request> = {};
    req.body = body;

    let res: Partial<Response> = {
      json: sinon.spy(),
    };
    let next: Partial<NextFunction> = () => {};

    let postSchemaInstance = require('../persistence/schemas/postSchema').default;
    Container.set('postSchema', postSchemaInstance);

    let postRepoClass = require(config.repos.post.path).default;
    let postRepoInstance = Container.get(postRepoClass);
    Container.set('PostRepo', postRepoInstance);

    let postServiceClass = require(config.services.post.path).default;
    let postServiceInstance = Container.get(postServiceClass);
    Container.set(config.services.post.name, postServiceInstance);
    postServiceInstance = Container.get(config.services.post.name);
    sinon.stub(postServiceInstance, 'undislikePost').returns(
      Result.ok<IPostDTO>({ id: 'idPost',
      content: 'This is a post',
      creatorId: 'idCreator',
      creatorEmail: 'emailCreator',
      avatar: 'avatar',
      name: 'name',
      likes: [
          'playerEmail'
      ],
      dislikes: [
          'idDislike'
      ],
      tags: [
          'tag'
      ],
      comments: [
          {
              id: 'idComment',
              postId: 'idPost',
              creatorId: 'idCreator',
              avatar: 'avatar',
              name: 'name',
              content: 'content',
              createdAt: null
          }
      ],
      createdAt: null
    }));

    const ctrl = new PostController(postServiceInstance as IPostService);

    await ctrl.undislikePost(<Request>req, <Response>res, <NextFunction>next);

    sinon.assert.calledOnce(res.json);
    sinon.assert.calledWith(res.json, sinon.match({ id: 'idPost',
    content: 'This is a post',
    creatorId: 'idCreator',
    creatorEmail: 'emailCreator',
    avatar: 'avatar',
    name: 'name',
    likes: [
        'playerEmail'
    ],
    dislikes: [
        'idDislike'
    ],
    tags: [
        'tag'
    ],
    comments: [
        {
            id: 'idComment',
            postId: 'idPost',
            creatorId: 'idCreator',
            avatar: 'avatar',
            name: 'name',
            content: 'content',
            createdAt: null
        }
    ],
    createdAt: null  }),
    );
  }
  )

  it('commentPost: returns json post content', async function() {
    let body = {    id: 'commentId',
                    postId: 'postId',
                    creatorId: 'creatorId',
                    avatar: 'avatar',
                    name: 'name',
                    content: 'content',
                    createdAt: null
                    };
    let req: Partial<Request> = {};
    req.body = body;

    let res: Partial<Response> = {
      json: sinon.spy(),
    };
    let next: Partial<NextFunction> = () => {};

    let postSchemaInstance = require('../persistence/schemas/postSchema').default;
    Container.set('postSchema', postSchemaInstance);

    let postRepoClass = require(config.repos.post.path).default;
    let postRepoInstance = Container.get(postRepoClass);
    Container.set('PostRepo', postRepoInstance);

    let postServiceClass = require(config.services.post.path).default;
    let postServiceInstance = Container.get(postServiceClass);
    Container.set(config.services.post.name, postServiceInstance);
    postServiceInstance = Container.get(config.services.post.name);
    sinon.stub(postServiceInstance, 'commentPost').returns(
      Result.ok<IPostDTO>({ id: 'idPost',
      content: 'This is a post',
      creatorId: 'idCreator',
      creatorEmail: 'emailCreator',
      avatar: 'avatar',
      name: 'name',
      likes: [
          'playerEmail'
      ],
      dislikes: [
          'idDislike'
      ],
      tags: [
          'tag'
      ],
      comments: [
          {
              id: 'idComment',
              postId: 'idPost',
              creatorId: 'idCreator',
              avatar: 'avatar',
              name: 'name',
              content: 'content',
              createdAt: null
          }
      ],
      createdAt: null
    }));

    const ctrl = new PostController(postServiceInstance as IPostService);

    await ctrl.commentPost(<Request>req, <Response>res, <NextFunction>next);

    sinon.assert.calledOnce(res.json);
    sinon.assert.calledWith(res.json, sinon.match({ id: 'idPost',
    content: 'This is a post',
    creatorId: 'idCreator',
    creatorEmail: 'emailCreator',
    avatar: 'avatar',
    name: 'name',
    likes: [
        'playerEmail'
    ],
    dislikes: [
        'idDislike'
    ],
    tags: [
        'tag'
    ],
    comments: [
        {
            id: 'idComment',
            postId: 'idPost',
            creatorId: 'idCreator',
            avatar: 'avatar',
            name: 'name',
            content: 'content',
            createdAt: null
        }
    ],
    createdAt: null  }),
    );
  }
  )

  it('deleteComment: returns json post content', async function() {
    let body = {    id: 'commentId',
                    postId: 'postId'
                    };
    let req: Partial<Request> = {};
    req.body = body;

    let res: Partial<Response> = {
      json: sinon.spy(),
    };
    let next: Partial<NextFunction> = () => {};

    let postSchemaInstance = require('../persistence/schemas/postSchema').default;
    Container.set('postSchema', postSchemaInstance);

    let postRepoClass = require(config.repos.post.path).default;
    let postRepoInstance = Container.get(postRepoClass);
    Container.set('PostRepo', postRepoInstance);

    let postServiceClass = require(config.services.post.path).default;
    let postServiceInstance = Container.get(postServiceClass);
    Container.set(config.services.post.name, postServiceInstance);
    postServiceInstance = Container.get(config.services.post.name);
    sinon.stub(postServiceInstance, 'deleteComment').returns(
      Result.ok<IPostDTO>({ id: 'idPost',
      content: 'This is a post',
      creatorId: 'idCreator',
      creatorEmail: 'emailCreator',
      avatar: 'avatar',
      name: 'name',
      likes: [
          'playerEmail'
      ],
      dislikes: [
          'idDislike'
      ],
      tags: [
          'tag'
      ],
      comments: [
          {
              id: 'idComment',
              postId: 'idPost',
              creatorId: 'idCreator',
              avatar: 'avatar',
              name: 'name',
              content: 'content',
              createdAt: null
          }
      ],
      createdAt: null
    }));

    const ctrl = new PostController(postServiceInstance as IPostService);

    await ctrl.deleteComment(<Request>req, <Response>res, <NextFunction>next);

    sinon.assert.calledOnce(res.json);
    sinon.assert.calledWith(res.json, sinon.match({ id: 'idPost',
    content: 'This is a post',
    creatorId: 'idCreator',
    creatorEmail: 'emailCreator',
    avatar: 'avatar',
    name: 'name',
    likes: [
        'playerEmail'
    ],
    dislikes: [
        'idDislike'
    ],
    tags: [
        'tag'
    ],
    comments: [
        {
            id: 'idComment',
            postId: 'idPost',
            creatorId: 'idCreator',
            avatar: 'avatar',
            name: 'name',
            content: 'content',
            createdAt: null
        }
    ],
    createdAt: null  }),
    );
  }
  )

  



  ;
});