import { Elm } from "./Main.elm";
import "./Styles/_index.scss";

// Initialize our Elm app
const app = Elm.Main.init();

// Get Scroll Position
window.addEventListener(
  "scroll",
  function () {
    var offset = { x: window.pageXOffset, y: window.pageYOffset };
    app.ports.onScroll.send(offset);
  },
  { passive: true }
);
