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
  })

  it('createPost partial argument test2: returns json post content', async function() {
    let body = {    id: 'idPost2',
                    content: 'This is a post2',
                    creatorId: 'idCreator2',
                    creatorEmail: 'emailCreator2',
                    avatar: 'avatar2',
                    name: 'name2',
                    likes: [
                    ],
                    dislikes: [
                    ],
                    tags: [
                    ],
                    comments: [
                        {
                            id: 'nocomment',
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
  })

  it('createPost partial argument test3: returns json post content', async function() {
    let body = {    id: 'idPost3',
                    content: 'This is a post3',
                    creatorId: 'idCreator3',
                    creatorEmail: 'emailCreator3',
                    avatar: 'avatar3',
                    name: 'name3',
                    likes: [
                    ],
                    dislikes: [
                    ],
                    tags: [
                    ],
                    comments: [
                        {
                            id: 'nocomment',
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
  })

  it('createPost partial argument test4: returns json post content', async function() {
    let body = {    id: 'idPost4',
                    content: 'This is a post4',
                    creatorId: 'idCreator4',
                    creatorEmail: 'emailCreator4',
                    avatar: 'avatar4',
                    name: 'name4',
                    likes: [
                    ],
                    dislikes: [
                    ],
                    tags: [
                    ],
                    comments: [
                        {
                            id: 'nocomment',
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
  })

  it('createPost partial argument test5: returns json post content', async function() {
    let body = {    id: 'idPost5',
                    content: 'This is a post5',
                    creatorId: 'idCreator5',
                    creatorEmail: 'emailCreator5',
                    avatar: 'avatar5',
                    name: 'name5',
                    likes: [
                    ],
                    dislikes: [
                    ],
                    tags: [
                    ],
                    comments: [
                        {
                            id: 'nocomment',
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
  })

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

  it('updatePost partial arguments test 2: returns json post content', async function() {
    let body = {    id: 'idPost2',
                    content: 'This is a post2',
                    creatorId: 'idCreator2',
                    creatorEmail: 'emailCreator2',
                    avatar: 'avatar2',
                    name: 'name2',
                    likes: [
                    ],
                    dislikes: [
                    ],
                    tags: [
                    ],
                    comments: [
                        {
                            id: 'nocomments',
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

  it('updatePost partial arguments test 3: returns json post content', async function() {
    let body = {    id: 'idPost3',
                    content: 'This is a post3',
                    creatorId: 'idCreator3',
                    creatorEmail: 'emailCreator3',
                    avatar: 'avatar3',
                    name: 'name3',
                    likes: [
                    ],
                    dislikes: [
                    ],
                    tags: [
                    ],
                    comments: [
                        {
                            id: 'nocomments',
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

  it('updatePost partial arguments test 4: returns json post content', async function() {
    let body = {    id: 'idPost4',
                    content: 'This is a post4',
                    creatorId: 'idCreator4',
                    creatorEmail: 'emailCreator4',
                    avatar: 'avatar4',
                    name: 'name4',
                    likes: [
                    ],
                    dislikes: [
                    ],
                    tags: [
                    ],
                    comments: [
                        {
                            id: 'nocomments',
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

  it('updatePost partial arguments test 5: returns json post content', async function() {
    let body = {    id: 'idPost5',
                    content: 'This is a post5',
                    creatorId: 'idCreator5',
                    creatorEmail: 'emailCreator5',
                    avatar: 'avatar5',
                    name: 'name5',
                    likes: [
                    ],
                    dislikes: [
                    ],
                    tags: [
                    ],
                    comments: [
                        {
                            id: 'nocomments',
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

  it('likePost partial arguments test2: returns json post content', async function() {
    let body = {    postId: 'postId2',
                    playerEmail: 'playerEmail2'
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

  it('likePost partial arguments test3: returns json post content', async function() {
    let body = {    postId: 'postId3',
                    playerEmail: 'playerEmail3'
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

  it('unlikePost partial argument test 2: returns json post content', async function() {
    let body = {    postId: 'postId2',
                    playerEmail: 'playerEmail2'
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

  it('unlikePost partial argument test 3: returns json post content', async function() {
    let body = {    postId: 'postId3',
                    playerEmail: 'playerEmail3'
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

  it('dislikePost partial argument test 2: returns json post content', async function() {
    let body = {    postId: 'postId2',
                    playerEmail: 'playerEmail2'
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

  it('dislikePost partial argument test 3: returns json post content', async function() {
    let body = {    postId: 'postId3',
                    playerEmail: 'playerEmail3'
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

  it('undislikePost partial argument test 2: returns json post content', async function() {
    let body = {    postId: 'postId2',
                    playerEmail: 'playerEmail2'
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

  it('undislikePost partial argument test 3: returns json post content', async function() {
    let body = {    postId: 'postId3',
                    playerEmail: 'playerEmail3'
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

  it('commentPost partial argument test2: returns json post content', async function() {
    let body = {    id: 'commentId2',
                    postId: 'postId2',
                    creatorId: 'creatorId2',
                    avatar: 'avatar2',
                    name: 'name2',
                    content: 'content2',
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

  it('commentPost partial argument test3: returns json post content', async function() {
    let body = {    id: 'commentId3',
                    postId: 'postId3',
                    creatorId: 'creatorId3',
                    avatar: 'avatar3',
                    name: 'name3',
                    content: 'content3',
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

  it('commentPost partial argument test4: returns json post content', async function() {
    let body = {    id: 'commentId4',
                    postId: 'postId4',
                    creatorId: 'creatorId4',
                    avatar: 'avatar4',
                    name: 'name4',
                    content: 'content4',
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

  it('commentPost partial argument test5: returns json post content', async function() {
    let body = {    id: 'commentId5',
                    postId: 'postId5',
                    creatorId: 'creatorId5',
                    avatar: 'avatar5',
                    name: 'name5',
                    content: 'content5',
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

  it('deleteComment partial arguments test2: returns json post content', async function() {
    let body = {    id: 'commentId2',
                    postId: 'postId2'
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

  
  it('deleteComment partial arguments test3: returns json post content', async function() {
    let body = {    id: 'commentId3',
                    postId: 'postId3'
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