import { Response, Request } from 'express';

import { Container} from 'typedi';

import config from '../../config';

import IPostRepo from '../services/IRepos/IPostRepo';

import { PostMap } from "../mappers/PostMap";
import { IPostDTO } from '../dto/IPostDTO';


exports.getMe = async function(req, res: Response) {
  
    // NB: a arquitetura ONION não está a ser seguida aqui

    const postRepo = Container.get(config.repos.post.name) as IPostRepo

    if( !req.token || req.token == undefined )
        return res.json( new Error("Inexistent or invalid Token")).status(401);

    const post = await postRepo.findById( req.token.id );
    if (!post)
        return res.json( new Error("Post not registered")).status(401);

    const postDTO = PostMap.toDTO( post ) as IPostDTO;
    return res.json( postDTO ).status(200);
}
