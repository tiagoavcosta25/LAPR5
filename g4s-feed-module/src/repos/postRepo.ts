import { Service, Inject } from 'typedi';

import IPostRepo from "../services/IRepos/IPostRepo";
import { Post } from "../domain/post";
import { PostId } from "../domain/postId";
import { PostMap } from "../mappers/PostMap";

import { Document, FilterQuery, Model } from 'mongoose';
import { IPostPersistence } from '../dataschema/IPostPersistence';

@Service()
export default class PostRepo implements IPostRepo {
  private models: any;

  constructor(
    @Inject('postSchema') private postSchema : Model<IPostPersistence & Document>,
  ) {}

  private createBaseQuery (): any {
    return {
      where: {},
    }
  }

  public async exists(post: Post): Promise<boolean> {
    
    const idX = post.id instanceof PostId ? (<PostId>post.id).toValue() : post.id;

    const query = { domainId: idX}; 
    const postDocument = await this.postSchema.findOne( query as FilterQuery<IPostPersistence & Document>);

    return !!postDocument === true;
  }

  public async save (post: Post): Promise<Post> {
    const query = { domainId: post.id.toString()}; 

    const postDocument = await this.postSchema.findOne( query );

    try {
      if (postDocument === null ) {
        const rawPost: any = PostMap.toPersistence(post);

        const postCreated = await this.postSchema.create(rawPost);

        return PostMap.toDomain(postCreated);
      } else {
        postDocument.content = post.content.value;
        postDocument.creatorId = post.creatorId;
        postDocument.likes = post.likes;
        postDocument.dislikes = post.dislikes;
        await postDocument.save();

        return post;
      }
    } catch (err) {
      throw err;
    }
  }

  public async findByCreatorId (creatorId: string | string): Promise<Post> {
    const query = { creatorId: creatorId.toString() };
    const postRecord = await this.postSchema.findOne( query );

    if( postRecord != null) {
      return PostMap.toDomain(postRecord);
    }
    else
      return null;
  }

  public async findById (postId: PostId | string): Promise<Post> {
    const query = { domainId: postId};
    const postRecord = await this.postSchema.findOne( query as FilterQuery<IPostPersistence & Document> );

    if( postRecord != null) {
      return PostMap.toDomain(postRecord);
    }
    else
      return null;
  }
}