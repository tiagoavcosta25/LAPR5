import { Service, Inject } from 'typedi';
import config from "../../config";
import IPostDTO from '../dto/IPostDTO';
import { Post } from "../domain/post";
import IPostRepo from '../services/IRepos/IPostRepo';
import IPostService from './IServices/IPostService';
import { Result } from "../core/logic/Result";
import { PostMap } from "../mappers/PostMap";
import { PostContent } from '../domain/postContent';
import IReactionDTO from '../dto/IReactionDTO';
import ICommentDTO from '../dto/ICommentDTO';
import { Comment } from '../domain/comment';
import { UniqueEntityID } from '../core/domain/UniqueEntityID';
import IDeleteCommentDTO from '../dto/IDeleteCommentDTO';
import mongoose from '../loaders/mongoose';

@Service()
export default class PostService implements IPostService {
  constructor(
      @Inject(config.repos.post.name) private postRepo : IPostRepo
  ) {}

  public async getPost( postId: string): Promise<Result<IPostDTO>> {
    try {
      const post = await this.postRepo.findByDomainId(postId);

      if (post === null) {
        return Result.fail<IPostDTO>("Post not found");
      }
      else {
        const postDTOResult = PostMap.toDTO( post ) as IPostDTO;
        return Result.ok<IPostDTO>( postDTOResult )
        }
    } catch (e) {
      throw e;
    }
  }


  public async createPost(postDTO: IPostDTO): Promise<Result<IPostDTO>> {
    try {

      const postOrError = await Post.create( postDTO );

      if (postOrError.isFailure) {
        return Result.fail<IPostDTO>(postOrError.errorValue());
      }

      const postResult = postOrError.getValue();

      await this.postRepo.save(postResult);

      const postDTOResult = PostMap.toDTO( postResult ) as IPostDTO;

      return Result.ok<IPostDTO>( postDTOResult )
    } catch (e) {
      throw e;
    }
  }

  public async updatePost(postDTO: IPostDTO): Promise<Result<IPostDTO>> {
    try {
      const post = await this.postRepo.findByDomainId(postDTO.id);

      if (post === null) {
        return Result.fail<IPostDTO>("Post not found");
      }
      else {
        post.content = PostContent.create(postDTO.content).getValue();
        post.creatorId = postDTO.creatorId;
        post.likes = postDTO.likes;
        post.dislikes = postDTO.dislikes;
        await this.postRepo.save(post);

        const postDTOResult = PostMap.toDTO( post ) as IPostDTO;
        return Result.ok<IPostDTO>( postDTOResult )
        }
    } catch (e) {
      throw e;
    }
  }

  public async likePost(postDTO: IReactionDTO): Promise<Result<IPostDTO>> {
    try {
      const post = await this.postRepo.findByDomainId(postDTO.postId);

      if (post === null) {
        return Result.fail<IPostDTO>("Post not found");
      }
      else {
        if(post.dislikes.includes(postDTO.playerId)){
          var i = post.dislikes.indexOf(postDTO.playerId);
          post.dislikes.splice(i, 1);
          
        }
        if(!post.likes.includes(postDTO.playerId)){
          post.likes.push(postDTO.playerId);
        }

        await this.postRepo.save(post);

        const postDTOResult = PostMap.toDTO( post ) as IPostDTO;
        return Result.ok<IPostDTO>( postDTOResult )
        }
    } catch (e) {
      throw e;
    }
  }

  public async dislikePost(postDTO: IReactionDTO): Promise<Result<IPostDTO>> {
    try {
      const post = await this.postRepo.findByDomainId(postDTO.postId);

      if (post === null) {
        return Result.fail<IPostDTO>("Post not found");
      }
      else {
        if(post.likes.includes(postDTO.playerId)){
          var i = post.likes.indexOf(postDTO.playerId);
          post.likes.splice(i, 1);
          
        }
        if(!post.dislikes.includes(postDTO.playerId)){
          post.dislikes.push(postDTO.playerId);
        }

        await this.postRepo.save(post);

        const postDTOResult = PostMap.toDTO( post ) as IPostDTO;
        return Result.ok<IPostDTO>( postDTOResult )
        }
    } catch (e) {
      throw e;
    }
  }

  public async commentPost(commentDTO: ICommentDTO): Promise<Result<IPostDTO>> {
    try {
      const post = await this.postRepo.findByDomainId(commentDTO.postId);

      if (post === null) {
        return Result.fail<IPostDTO>("Post not found");
      }
      else {
        let newCommentProps = {
          postId: commentDTO.postId,
          creatorId: commentDTO.creatorId,
          content: commentDTO.content,
          createdAt: commentDTO.createdAt
        } as ICommentDTO

        let comment = Comment.create(newCommentProps, new UniqueEntityID(commentDTO.id)).getValue();
        post.comments.push(comment);
        await this.postRepo.save(post);

        const postFinished = await this.postRepo.findByDomainId(commentDTO.postId);

        const postDTOResult = PostMap.toDTO( postFinished ) as IPostDTO;
        return Result.ok<IPostDTO>( postDTOResult )
        }
    } catch (e) {
      throw e;
    }
  }

  public async deleteComment(deleteCommentDTO: IDeleteCommentDTO): Promise<Result<IPostDTO>> {
    try {
      const post = await this.postRepo.findByDomainId(deleteCommentDTO.postId);

      if (post === null) {
        return Result.fail<IPostDTO>("Post not found");
      }
      else {

        let index = post.comments.findIndex( c => c.id.toString() == deleteCommentDTO.id);

        if (index != -1) {
          post.comments.splice(index, 1);
        } else {
          return Result.fail<IPostDTO>("Comment not found");
        }

        await this.postRepo.save(post);

        const postDTOResult = PostMap.toDTO( post ) as IPostDTO;
        return Result.ok<IPostDTO>( postDTOResult )
        }
    } catch (e) {
      throw e;
    }
  }

  public async getPostsByUser(creatorId: string): Promise<Result<IPostDTO[]>> {
    try {
      const posts = await this.postRepo.getAllByCreatorId(creatorId);

      if (posts.length == 0) {
        return Result.fail<IPostDTO[]>("There are no posts");
      }
      else {
        let postList: IPostDTO[] = [];
        for(let p of posts) {
          postList.push(PostMap.toDTO( p ) as IPostDTO);
        }
        return Result.ok<IPostDTO[]>( postList )
      }
    } catch (e) {
      throw e;
    }
  }

}
