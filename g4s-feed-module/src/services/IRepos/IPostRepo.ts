import { Repo } from "../../core/infra/Repo";
import { Post } from "../../domain/post";

export default interface IPostRepo extends Repo<Post> {
	save(post: Post): Promise<Post>;
	findByCreatorId (creatorId: string): Promise<Post>;
	findById (id: string): Promise<Post>;
}
  