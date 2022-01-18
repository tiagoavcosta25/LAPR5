import mongoose from 'mongoose';

export const CommentSchema = new mongoose.Schema({
    domainId: { type: String },
    postId: { type: String },
    creatorId: { type: String},
    name: { type: String },
    content: { type: String}, 
  },
  {
    timestamps: true
  });