import { Router } from 'express';
import auth from './routes/userRoute';
import user from './routes/userRoute';
import role from './routes/roleRoute';
import post from './routes/postRoute';
import comment from './routes/commentRoute';

export default () => {
	const app = Router();

	post(app);
	comment(app);
	auth(app);
	user(app);
	role(app);
	
	return app
}