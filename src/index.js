import "./main.css";
import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

const storedState = localStorage.getItem("model");

const startingState = storedState ? JSON.parse(storedState) : null;

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: startingState
});

app.ports.setStorage.subscribe(state => {
  localStorage.setItem("model", JSON.stringify(state));
});

app.ports.removeStorage.subscribe(() => {
  localStorage.removeItem("model");
});

registerServiceWorker();
