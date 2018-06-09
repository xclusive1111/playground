import './main.css';
import { Main } from './user_input/Signup.elm';
import registerServiceWorker from './registerServiceWorker';

Main.embed(document.getElementById('root'));

registerServiceWorker();
