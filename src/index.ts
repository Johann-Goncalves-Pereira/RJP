// @ts-ignore
import { Elm } from "./Main.elm";
import "./Styles/_index.scss";

//? Initialize our Elm app
const app = Elm.Main.init({
  // @ts-ignore
  flags: JSON.parse(localStorage.getItem("storage")),
});

//? Get Scroll Position
window.addEventListener(
  "scroll",
  function () {
    var offset = { x: window.pageXOffset, y: window.pageYOffset };
    app.ports.onScroll.send(offset);
  },
  { passive: true }
);

//? Port To get Storage
app.ports.save.subscribe((storage: JSON) => {
  localStorage.setItem("storage", JSON.stringify(storage));
  app.ports.load.send(storage);
});

//? Port To get Dialog State
app.ports.toggleDialog.subscribe((id: string) => {
  const dialog: any = document.querySelector(`#${id}`);

  if (dialog.close) {
    dialog.close;
    console.log(dialog.close);
  } else {
    dialog.showModal;
    console.log(dialog.showModal);
  }
});
