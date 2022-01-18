import { Mapper } from "../core/infra/Mapper";

import { Document, Model } from 'mongoose';
import { IPostPersistence } from '../dataschema/IPostPersistence';

import IPostDTO from "../dto/IPostDTO";
import { Post } from "../domain/post";

import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { CommentMap } from "./CommentMap";

export class PostMap extends Mapper<Post> {
  
  public static toDTO( post: Post): IPostDTO {
    return {
      id: post.id.toString(),
      content: post.content.value,
      creatorId: post.creatorId,
      name: post.name,
      likes: post.likes,
      dislikes: post.dislikes,
      tags: post.tags,
      comments: post.comments.map(comment => CommentMap.toDTO(comment, post.id.toValue() as string)),
      createdAt: post.createdAt
    } as IPostDTO;
  }

  public static toDomain (post: any | Model<IPostPersistence & Document> ): Post {
    const postOrError = Post.create(
      post,
      new UniqueEntityID(post.domainId)
    );

    postOrError.isFailure ? console.log(postOrError.error) : '';

    return postOrError.isSuccess ? postOrError.getValue() : null;
  }

  public static toPersistence (post: Post): any {
    return {
      domainId: post.id.toString(),
      content: post.content.value.toString(),
      creatorId: post.creatorId.toString(),
      name: post.name.toString(),
      likes: post.likes,
      dislikes: post.dislikes,
      tags: post.tags,
      comments: post.comments.map(comment => CommentMap.toPersistence(comment)),
      createdAt: post.createdAt
    }
  }
}