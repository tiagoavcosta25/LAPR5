import { IPostPersistence } from '../../dataschema/IPostPersistence';
import mongoose from 'mongoose';

const PostSchema = new mongoose.Schema(
  {
    id: { type: String, unique: true },
    content: { type: String, unique: true },
    creatorId: { type: String, unique: true }
  },
  {
    timestamps: true
  }
);

export default mongoose.model<IPostPersistence & mongoose.Document>('Post', PostSchema);
