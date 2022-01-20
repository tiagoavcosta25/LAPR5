import { Repo } from "../../core/infra/Repo";
import { Post } from "../../domain/post";
import { PostId } from "../../domain/postId";

export default interface IPostRepo extends Repo<Post> {
	save(post: Post): Promise<Post>;
	findByDomainId (roleId: PostId | string): Promise<Post>;
	findByCreatorId (creatorId: string): Promise<Post>;
	getAllByCreatorId (creatorId: string): Promise<Post[]>;
	countALikesOnBPosts (emailA: string, emailB: string): Promise<number>;
}
  