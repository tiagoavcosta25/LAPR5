import * as sinon from 'sinon';

import { Response, Request, NextFunction } from 'express';

import { Container } from 'typedi';
import config from '../../config';

import { Result } from '../core/logic/Result';

import IRoleService from '../services/IServices/IRoleService';
import RoleController from './roleController';
import IRoleDTO from '../dto/IRoleDTO';
import 'reflect-metadata';

describe('role controller', function() {
  beforeEach(function() {});

  it('createRole: returns json with id+name values', async function() {
    let body = { name: 'role12' };
    let req: Partial<Request> = {};
    req.body = body;

    let res: Partial<Response> = {
      json: sinon.spy(),
    };
    let next: Partial<NextFunction> = () => {};

    let roleSchemaInstance = require('../persistence/schemas/roleSchema').default;
    Container.set('roleSchema', roleSchemaInstance);

    let roleRepoClass = require(config.repos.role.path).default;
    let roleRepoInstance = Container.get(roleRepoClass);
    Container.set('RoleRepo', roleRepoInstance);

    let roleServiceClass = require(config.services.role.path).default;
    let roleServiceInstance = Container.get(roleServiceClass);
    Container.set(config.services.role.name, roleServiceInstance);
    roleServiceInstance = Container.get(config.services.role.name);
    sinon.stub(roleServiceInstance, 'createRole').returns(
      Result.ok<IRoleDTO>({ id: '123', name: req.body.name }),
    );

    const ctrl = new RoleController(roleServiceInstance as IRoleService);

    await ctrl.createRole(<Request>req, <Response>res, <NextFunction>next);

    sinon.assert.calledOnce(res.json);
    sinon.assert.calledWith(res.json, sinon.match({ id: '123', name: req.body.name }));
  })
  
  it('createRole2: returns json with id+name values', async function() {
    let body = { name: 'role12' };
    let req: Partial<Request> = {};
    req.body = body;

    let res: Partial<Response> = {
      json: sinon.spy(),
    };
    let next: Partial<NextFunction> = () => {};

    let roleSchemaInstance = require('../persistence/schemas/roleSchema').default;
    Container.set('roleSchema', roleSchemaInstance);

    let roleRepoClass = require(config.repos.role.path).default;
    let roleRepoInstance = Container.get(roleRepoClass);
    Container.set('RoleRepo', roleRepoInstance);

    let roleServiceClass = require(config.services.role.path).default;
    let roleServiceInstance = Container.get(roleServiceClass);
    Container.set(config.services.role.name, roleServiceInstance);
    roleServiceInstance = Container.get(config.services.role.name);

    const ctrl = new RoleController(roleServiceInstance as IRoleService);

    await ctrl.createRole(<Request>req, <Response>res, <NextFunction>next);

    sinon.assert.calledOnce(res.json);
    sinon.assert.calledWith(res.json, sinon.match({ id: '123', name: req.body.name }));
  })

  it('createRole3: returns json with id+name values', async function() {
    let body = { name: 'role12' };
    let req: Partial<Request> = {};
    req.body = body;

    let res: Partial<Response> = {
      json: sinon.spy(),
    };
    let next: Partial<NextFunction> = () => {};

    let roleSchemaInstance = require('../persistence/schemas/roleSchema').default;
    Container.set('roleSchema', roleSchemaInstance);

    let roleRepoClass = require(config.repos.role.path).default;
    let roleRepoInstance = Container.get(roleRepoClass);
    Container.set('RoleRepo', roleRepoInstance);

    let roleServiceClass = require(config.services.role.path).default;
    let roleServiceInstance = Container.get(roleServiceClass);
    Container.set(config.services.role.name, roleServiceInstance);
    roleServiceInstance = Container.get(config.services.role.name);

    const ctrl = new RoleController(roleServiceInstance as IRoleService);

    await ctrl.createRole(<Request>req, <Response>res, <NextFunction>next);

    sinon.assert.calledOnce(res.json);
    sinon.assert.calledWith(res.json, sinon.match({ id: '123', name: req.body.name }));
  })
  ;
});