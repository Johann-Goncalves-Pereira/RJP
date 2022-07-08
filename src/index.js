import { Elm } from "./Main.elm";
import "./Styles/_index.scss";

// Initialize our Elm app
const app = Elm.Main.init({
  flags: JSON.parse(localStorage.getItem("storage")),
});

// Get Scroll Position
window.addEventListener(
  "scroll",
  function () {
    var offset = { x: window.pageXOffset, y: window.pageYOffset };
    app.ports.onScroll.send(offset);
  },
  { passive: true }
);

app.ports.save.subscribe((storage) => {
  localStorage.setItem("storage", JSON.stringify(storage));
  app.ports.load.send(storage);
});
