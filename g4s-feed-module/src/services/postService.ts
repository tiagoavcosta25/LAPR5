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
        post.creatorEmail = postDTO.creatorEmail;
        post.avatar = postDTO.avatar;
        post.name = postDTO.name;
        post.likes = postDTO.likes;
        post.dislikes = postDTO.dislikes;
        for(let c of postDTO.comments) {
          let newCommentProps = {
            postId: c.postId,
            creatorId: c.creatorId,
            avatar: c.avatar,
            name: c.name,
            content: c.content,
            createdAt: c.createdAt
          } as ICommentDTO
          let newComment = Comment.create(newCommentProps, new UniqueEntityID((<any>c).domainId));
          post.comments.push(newComment.getValue());
        }
        post.comments.sort((b,a) => (a.createdAt > b.createdAt) ? 1 : ((b.createdAt > a.createdAt) ? -1 : 0))
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
        if(post.dislikes.includes(postDTO.playerEmail)){
          var i = post.dislikes.indexOf(postDTO.playerEmail);
          post.dislikes.splice(i, 1);
          
        }
        if(!post.likes.includes(postDTO.playerEmail)){
          post.likes.push(postDTO.playerEmail);
        }

        await this.postRepo.save(post);

        const postDTOResult = PostMap.toDTO( post ) as IPostDTO;
        return Result.ok<IPostDTO>( postDTOResult )
        }
    } catch (e) {
      throw e;
    }
  }

  public async unlikePost(postDTO: IReactionDTO): Promise<Result<IPostDTO>> {
    try {
      const post = await this.postRepo.findByDomainId(postDTO.postId);

      if (post === null) {
        return Result.fail<IPostDTO>("Post not found");
      }
      else {
        if(post.likes.includes(postDTO.playerEmail)){
          var i = post.likes.indexOf(postDTO.playerEmail);
          post.likes.splice(i, 1);
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
        if(post.likes.includes(postDTO.playerEmail)){
          var i = post.likes.indexOf(postDTO.playerEmail);
          post.likes.splice(i, 1);
          
        }
        if(!post.dislikes.includes(postDTO.playerEmail)){
          post.dislikes.push(postDTO.playerEmail);
        }

        await this.postRepo.save(post);

        const postDTOResult = PostMap.toDTO( post ) as IPostDTO;
        return Result.ok<IPostDTO>( postDTOResult )
        }
    } catch (e) {
      throw e;
    }
  }

  public async undislikePost(postDTO: IReactionDTO): Promise<Result<IPostDTO>> {
    try {
      const post = await this.postRepo.findByDomainId(postDTO.postId);

      if (post === null) {
        return Result.fail<IPostDTO>("Post not found");
      }
      else {
        if(post.dislikes.includes(postDTO.playerEmail)){
          var i = post.dislikes.indexOf(postDTO.playerEmail);
          post.dislikes.splice(i, 1);
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
          avatar: commentDTO.avatar,
          name: commentDTO.name,
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
        return Result.ok<IPostDTO[]>([]);
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

  public async getDCalc(idA: string, idB:string): Promise<Result<number>> {
    try {
      let likes = await this.postRepo.countALikesOnBPosts(idA, idB);
      let dislikes = await this.postRepo.countADislikesOnBPosts(idA, idB);

      let count = likes - dislikes;

      return Result.ok<number>(count);
    } catch (e) {
      throw e;
    }
  }

}
