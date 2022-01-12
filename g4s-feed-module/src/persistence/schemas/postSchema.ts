import { IPostPersistence } from '../../dataschema/IPostPersistence';
import mongoose from 'mongoose';

const PostSchema = new mongoose.Schema(
  {
    domainId: { type: String, unique: true },
    content: { type: String}, 
    creatorId: { type: String},
    likes: { type: [String] },
    dislikes: { type: [String]},
    tags: { type: [String]}
  },
  {
    timestamps: true
  }
);

export default mongoose.model<IPostPersistence & mongoose.Document>('Post', PostSchema);
