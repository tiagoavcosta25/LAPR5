import { Service, Inject } from 'typedi';
import config from "../../config";
import IPostDTO from '../dto/IPostDTO';
import { Post } from "../domain/post";
import IPostRepo from '../services/IRepos/IPostRepo';
import IPostService from './IServices/IPostService';
import { Result } from "../core/logic/Result";
import { PostMap } from "../mappers/PostMap";
import { PostContent } from '../domain/postContent';

@Service()
export default class PostService implements IPostService {
  constructor(
      @Inject(config.repos.post.name) private postRepo : IPostRepo
  ) {}

  public async getPost( postId: string): Promise<Result<IPostDTO>> {
    try {
      const post = await this.postRepo.findById(postId);

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
      const post = await this.postRepo.findById(postDTO.id);

      if (post === null) {
        return Result.fail<IPostDTO>("Post not found");
      }
      else {
        post.content = PostContent.create(postDTO.content).getValue();
        post.creatorId = postDTO.creatorId;
        await this.postRepo.save(post);

        const postDTOResult = PostMap.toDTO( post ) as IPostDTO;
        return Result.ok<IPostDTO>( postDTOResult )
        }
    } catch (e) {
      throw e;
    }
  }

}
