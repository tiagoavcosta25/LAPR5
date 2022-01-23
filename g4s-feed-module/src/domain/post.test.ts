import 'reflect-metadata';
import { Post } from './post';
import IPostDTO from '../dto/IPostDTO';
import ICommentDTO from '../dto/ICommentDTO';

describe('post unit test', function() {
    var expect = require('expect');
    let content : string = "content";
    let creatorId : string = "creatorId";
    let creatorEmail : string = "creatorEmail";
    let avatar : string = "avatar";
    let name : string = "name";
    let likes : string[] = ["like"];
    let dislikes : string[] = ["dislike"];
    let tags : string[] = ["tag"];
    let comments = [];
    let createdAt : null;
    let postDto : IPostDTO = {
        id:null,
        content:content,
        creatorId:creatorId,
        creatorEmail:creatorEmail,
        avatar:avatar,
        name:name,
        likes:likes,
        dislikes:dislikes,
        tags:tags,
        comments:comments,
        createdAt:createdAt
    }

    const resetContent = () => postDto.content = content;
    const resetCreatorId = () => postDto.creatorId = creatorId;
    const resetCreatorEmail = () => postDto.creatorEmail = creatorEmail;
    const resetAvatar = () => postDto.avatar = avatar;
    const resetName = () => postDto.name = name;
    const resetLikes = () => postDto.likes = likes;
    const resetDislikes = () => postDto.dislikes = dislikes;
    const resetTags = () => postDto.tags = tags;
    const resetComments = () => postDto.comments = comments;
    const resetCreatedAt = () => postDto.createdAt = createdAt;

    it('create valid post', () => {
        const post = Post.create(postDto);
        expect(post.isSuccess).toEqual(true);
        expect(post.getValue().content.value.toString()).toEqual(content);
        expect(post.getValue().creatorId.toString()).toEqual(creatorId);
        expect(post.getValue().creatorEmail.toString()).toEqual(creatorEmail);
        expect(post.getValue().avatar.toString()).toEqual(avatar);
        expect(post.getValue().name.toString()).toEqual(name);
        expect(post.getValue().likes).toEqual(likes);
        expect(post.getValue().dislikes).toEqual(dislikes);
        expect(post.getValue().tags).toEqual(tags);
        expect(post.getValue().comments).toEqual(comments);
        expect(post.getValue().createdAt).toEqual(createdAt);
        
    })

    it('fail to create post with empty content', () => {
        postDto.content = "";
        const dto = Post.create(postDto);
        expect(dto.isFailure).toEqual(true);
        resetContent();
    })

    it('fail to create post with undefined content', () => {
        postDto.content = undefined;
        const dto = Post.create(postDto);
        expect(dto.isFailure).toEqual(true);
        resetContent();
    })

    it('fail to create post with null content', () => {
        postDto.content = null;
        const dto = Post.create(postDto);
        expect(dto.isFailure).toEqual(true);
        resetContent();
    })

    it('fail to create post with empty creatorId', () => {
        postDto.creatorId = "";
        const dto = Post.create(postDto);
        expect(dto.isFailure).toEqual(true);
        resetCreatorId();
    })

    it('fail to create post with undefined creatorId', () => {
        postDto.creatorId = undefined;
        const dto = Post.create(postDto);
        expect(dto.isFailure).toEqual(true);
        resetCreatorId();
    })

    it('fail to create post with null creatorId', () => {
        postDto.creatorId = null;
        const dto = Post.create(postDto);
        expect(dto.isFailure).toEqual(true);
        resetCreatorId();
    })

    it('fail to create post with empty creatorEmail', () => {
        postDto.creatorEmail = "";
        const dto = Post.create(postDto);
        expect(dto.isFailure).toEqual(true);
        resetCreatorEmail();
    })

    it('fail to create post with undefined creatorEmail', () => {
        postDto.creatorEmail = undefined;
        const dto = Post.create(postDto);
        expect(dto.isFailure).toEqual(true);
        resetCreatorEmail();
    })

    it('fail to create post with null avatar', () => {
        postDto.avatar = null;
        const dto = Post.create(postDto);
        expect(dto.isFailure).toEqual(true);
        resetAvatar();
    })

    it('fail to create post with empty name', () => {
        postDto.name = "";
        const dto = Post.create(postDto);
        expect(dto.isFailure).toEqual(true);
        resetName();
    })

    it('fail to create post with undefined name', () => {
        postDto.name = undefined;
        const dto = Post.create(postDto);
        expect(dto.isFailure).toEqual(true);
        resetName();
    })

    it('fail to create post with null name', () => {
        postDto.name = null;
        const dto = Post.create(postDto);
        expect(dto.isFailure).toEqual(true);
        resetName();
    })

    it('success creating post with likes', () => {
        postDto.likes.push("like1");
        const dto = Post.create(postDto);
        expect(dto.isSuccess).toEqual(true);
        resetLikes(); 
    })

    it('success creating post with undefined likes (goes to [])', () => {
        postDto.likes = undefined;
        const dto = Post.create(postDto);
        expect(dto.isSuccess).toEqual(true);
        resetLikes(); 
    })

    it('success creating post with null likes (goes to [])', () => {
        postDto.likes = null;
        const dto = Post.create(postDto);
        expect(dto.isSuccess).toEqual(true);
        resetComments(); 
    })

    it('success creating post with dislikes', () => {
        postDto.dislikes.push("dislike1");
        const dto = Post.create(postDto);
        expect(dto.isSuccess).toEqual(true);
        resetDislikes(); 
    })

    it('success creating post with undefined dislikes (goes to [])', () => {
        postDto.dislikes = undefined;
        const dto = Post.create(postDto);
        expect(dto.isSuccess).toEqual(true);
        resetDislikes(); 
    })

    it('success creating post with null dislikes (goes to [])', () => {
        postDto.dislikes = null;
        const dto = Post.create(postDto);
        expect(dto.isSuccess).toEqual(true);
        resetDislikes(); 
    })

    it('success creating post with comments', () => {
        let commentDto : ICommentDTO = {
            id: "commentId",
            postId: "postId",
            creatorId: "creatorId",
            avatar: "avatar",
            name: "name",
            content: "content",
            createdAt: null
        }
        postDto.comments.push(commentDto);
        const dto = Post.create(postDto);
        expect(dto.isSuccess).toEqual(true);
        resetComments(); 
    })

    it('success creating post with undefined comments (goes to [])', () => {
        postDto.comments = undefined;
        const dto = Post.create(postDto);
        expect(dto.isSuccess).toEqual(true);
        resetComments(); 
    })

    it('success creating post with null comments (goes to [])', () => {
        postDto.comments = null;
        const dto = Post.create(postDto);
        expect(dto.isSuccess).toEqual(true);
        resetComments(); 
    })
  ;
});