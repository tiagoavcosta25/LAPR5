import dotenv from 'dotenv';

// Set the NODE_ENV to 'development' by default
process.env.NODE_ENV = process.env.NODE_ENV || 'development';

const envFound = dotenv.config();
if (!envFound) {
  // This error should crash whole process

  throw new Error("⚠️  Couldn't find .env file  ⚠️");
}

export default {
  /**
   * Your favorite port
   */
  port: parseInt(process.env.PORT, 10) || 3000,

  /**
   * That long string from mlab
   */
  databaseURL: process.env.MONGODB_URI || "mongodb://socialnetworknosql51db:qvgH1prW5uFZ1q9TjGevNEeYZlgGNM9khrC2V4XiNC3ek9pRKd6CCD3qzxpf1zOr0MRWANWVBcdfOX8ghF4w2w==@socialnetworknosql51db.mongo.cosmos.azure.com:10255/socialNetworkNoSqlDB51?ssl=true&retrywrites=false&maxIdleTimeMS=120000&appName=@socialnetworknosql51db@",

  /**
   * Your secret sauce
   */
  jwtSecret: process.env.JWT_SECRET || "my sakdfho2390asjod$%jl)!sdjas0i secret",

  /**
   * Used by winston logger
   */
  logs: {
    level: process.env.LOG_LEVEL || 'info',
  },

  /**
   * API configs
   */
  api: {
    prefix: '/api',
  },

  controllers: {
    role: {
      name: "RoleController",
      path: "../controllers/roleController"
    },
    post: {
      name: "PostController",
      path: "../controllers/postController"
    }
  },

  repos: {
    role: {
      name: "RoleRepo",
      path: "../repos/roleRepo"
    },
    user: {
      name: "UserRepo",
      path: "../repos/userRepo"
    },
    post: {
      name: "PostRepo",
      path: "../repos/postRepo"
    }
  },

  services: {
    role: {
      name: "RoleService",
      path: "../services/roleService"
    },
    post: {
      name: "PostService",
      path: "../services/postService"
    }
  },
};
