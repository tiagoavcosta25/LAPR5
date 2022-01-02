import { IRolePersistence } from '../../dataschema/IRolePersistence';
import mongoose from 'mongoose';

const PostSchema = new mongoose.Schema(
  {
    domainId: { type: String, unique: true },
    content: { type: String, unique: true }
  },
  {
    timestamps: true
  }
);

export default mongoose.model<IRolePersistence & mongoose.Document>('Post', PostSchema);
