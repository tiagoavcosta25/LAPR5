import { Service, Inject } from 'typedi';

import { Document, Model } from 'mongoose';
import { IPostPersistence } from '../dataschema/IPostPersistence';

import IPostRepo from "../services/IRepos/IPostRepo";
import { Post } from "../domain/post";
import { PostId } from "../domain/postId";
import { PostContent } from "../domain/postContent";
import { PostMap } from "../mappers/PostMap";

@Service()
export default class PostRepo implements IPostRepo {
  private models: any;

  constructor(
    @Inject('postSchema') private postSchema : Model<IPostPersistence & Document>,
    @Inject('logger') private logger
  ) { }

  private createBaseQuery (): any {
    return {
      where: {},
    }
  }

  public async exists (postId: PostId | string): Promise<boolean> {

    const idX = postId instanceof PostId ? (<PostId>postId).id.toValue() : postId;

    const query = { domainId: idX}; 
    const postDocument = await this.postSchema.findOne( query );

    return !!postDocument === true;
  }

  public async save (post: Post): Promise<Post> {
    const query = { domainId: post.id.toString() }; 

    const postDocument = await this.postSchema.findOne( query );

    try {
      if (postDocument === null ) {
        const rawPost: any = PostMap.toPersistence(post);

        const postCreated = await this.postSchema.create(rawPost);

        return PostMap.toDomain(postCreated);
      } else {
        postDocument.content = post.content;
        await postDocument.save();

        return post;
      }
    } catch (err) {
      throw err;
    }
  }

  public async findByContent (content: PostContent | string): Promise<Post> {
    const query = { content: content.toString() };
    const postRecord = await this.postSchema.findOne( query );

    if( postRecord != null) {
      return PostMap.toDomain(postRecord);
    }
    else
      return null;
  }

  public async findById (postId: PostId | string): Promise<Post> {

    const idX = postId instanceof PostId ? (<PostId>postId).id.toValue() : postId;

    const query = { domainId: idX }; 
    const postRecord = await this.postSchema.findOne( query );

    if( postRecord != null) {
      return PostMap.toDomain(postRecord);
    }
    else
      return null;
  }
}