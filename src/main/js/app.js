import Elm from '../elm/App.elm';
import $ from 'jquery';

import 'bootstrap';

import 'bootstrap/dist/css/bootstrap.css'
import '../stylesheets/style.scss';

const container = document.getElementById('main');
const todo = Elm.App.embed(container)

todo.ports.focus.subscribe(focus)

function focus(id){
  setTimeout(() => $(`#${id}`).focus(), 50)
}
