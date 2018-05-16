import './Styles/style.css';
import { App } from './App.elm';
import registerServiceWorker from './registerServiceWorker';

App.embed(document.getElementById('root'));

registerServiceWorker();
