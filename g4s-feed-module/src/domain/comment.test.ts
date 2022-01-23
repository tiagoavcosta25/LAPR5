import 'reflect-metadata';
import { Post } from './post';
import IPostDTO from '../dto/IPostDTO';
import ICommentDTO from '../dto/ICommentDTO';
import { Comment } from './comment';

describe('post unit test', function() {
    var expect = require('expect');
    let id : string = "commentId";
    let postId: string = "postId";
    let creatorId: string = "creatorId";
    let avatar: string = "avatar";
    let name: string = "name";
    let content: string = "content";
    let commentDto : ICommentDTO = {
        id: id,
        postId: postId,
        creatorId: creatorId,
        avatar: avatar,
        name: name,
        content: content,
        createdAt: null
    }

    const resetId = () => commentDto.id = id;
    const resetPostId = () => commentDto.postId = postId;
    const resetCreatorId = () => commentDto.creatorId = creatorId;
    const resetAvatar = () => commentDto.avatar = avatar;
    const resetName = () => commentDto.name = name;
    const resetContent = () => commentDto.content = content;

    it('create valid comment', () => {
        const comment = Comment.create(commentDto);
        expect(comment.isSuccess).toEqual(true);
        expect(comment.getValue().postId.toString()).toEqual(postId);
        expect(comment.getValue().creatorId.toString()).toEqual(creatorId);
        expect(comment.getValue().avatar.toString()).toEqual(avatar);
        expect(comment.getValue().name.toString()).toEqual(name);
        expect(comment.getValue().content.value.toString()).toEqual(content);    
    })

    it('fail to create comment with empty postId', () => {
        commentDto.postId = "";
        const dto = Post.create(commentDto);
        expect(dto.isFailure).toEqual(true);
        resetPostId();
    })

    it('fail to create comment with undefined postId', () => {
        commentDto.postId = undefined;
        const dto = Post.create(commentDto);
        expect(dto.isFailure).toEqual(true);
        resetPostId();
    })

    it('fail to create comment with null postId', () => {
        commentDto.content = null;
        const dto = Post.create(commentDto);
        expect(dto.isFailure).toEqual(true);
        resetPostId();
    })

    it('fail to create comment with empty creatorId', () => {
        commentDto.creatorId = "";
        const dto = Post.create(commentDto);
        expect(dto.isFailure).toEqual(true);
        resetCreatorId();
    })

    it('fail to create comment with undefined creatorId', () => {
        commentDto.creatorId = undefined;
        const dto = Post.create(commentDto);
        expect(dto.isFailure).toEqual(true);
        resetCreatorId();
    })

    it('fail to create comment with null creatorId', () => {
        commentDto.creatorId = null;
        const dto = Post.create(commentDto);
        expect(dto.isFailure).toEqual(true);
        resetCreatorId();
    })

    it('fail to create comment with empty avatar', () => {
        commentDto.avatar = "";
        const dto = Post.create(commentDto);
        expect(dto.isFailure).toEqual(true);
        resetAvatar();
    })

    it('fail to create comment with undefined avatar', () => {
        commentDto.avatar = undefined;
        const dto = Post.create(commentDto);
        expect(dto.isFailure).toEqual(true);
        resetAvatar();
    })

    it('fail to create comment with null avatar', () => {
        commentDto.avatar = null;
        const dto = Post.create(commentDto);
        expect(dto.isFailure).toEqual(true);
        resetAvatar();
    })

    it('fail to create comment with empty name', () => {
        commentDto.name = "";
        const dto = Post.create(commentDto);
        expect(dto.isFailure).toEqual(true);
        resetName();
    })

    it('fail to create comment with undefined name', () => {
        commentDto.name = undefined;
        const dto = Post.create(commentDto);
        expect(dto.isFailure).toEqual(true);
        resetName();
    })

    it('fail to create comment with null name', () => {
        commentDto.name = null;
        const dto = Post.create(commentDto);
        expect(dto.isFailure).toEqual(true);
        resetName();
    })

    it('fail to create comment with empty content', () => {
        commentDto.content = "";
        const dto = Post.create(commentDto);
        expect(dto.isFailure).toEqual(true);
        resetContent();
    })

    it('fail to create comment with undefined content', () => {
        commentDto.content = undefined;
        const dto = Post.create(commentDto);
        expect(dto.isFailure).toEqual(true);
        resetContent();
    })

    it('fail to create comment with null content', () => {
        commentDto.content = null;
        const dto = Post.create(commentDto);
        expect(dto.isFailure).toEqual(true);
        resetContent();
    })

  ;
});