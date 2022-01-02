import { Repo } from "../../core/infra/Repo";
import { Post } from "../../domain/post";
import { PostContent } from "../../domain/postContent";

export default interface IUserRepo extends Repo<Post> {
	save(post: Post): Promise<Post>;
	findByContent (content: PostContent | string): Promise<Post>;
	findById (id: string): Promise<Post>;
}
  