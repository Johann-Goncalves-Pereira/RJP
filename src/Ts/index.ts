// & Elm
import { Elm } from "../Main.elm";

// & Ports
import { Storage } from "./Ports/Storage";
import { Scroll } from "./Ports/Scroll";
import { Dialog } from "./Ports/Dialog";

// & Style
import "../Styles/_index.scss";

// * APP
const rootNode = document.querySelector("#root");
const locaStorage = JSON.parse(localStorage.getItem("storage") as string);

const app = Elm.Main.init({
  flags: locaStorage,
  node: rootNode,
});

// : Calls
Storage(app);
Scroll(app);
Dialog(app);
