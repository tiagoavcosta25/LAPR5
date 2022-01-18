import { IPostPersistence } from '../../dataschema/IPostPersistence';
import mongoose from 'mongoose';
import { CommentSchema } from './commentSchema';

const PostSchema = new mongoose.Schema(
  {
    domainId: { type: String, unique: true },
    content: { type: String}, 
    creatorId: { type: String},
    name: { type: String },
    likes: { type: [String] },
    dislikes: { type: [String]},
    tags: { type: [String]},
    comments: [CommentSchema]
  },
  {
    timestamps: true
  }
);

export default mongoose.model<IPostPersistence & mongoose.Document>('Post', PostSchema);
