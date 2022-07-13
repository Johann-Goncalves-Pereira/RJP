export function Scroll(app: any) {
  window.addEventListener(
    "scroll",
    function () {
      var offset = { x: window.pageXOffset, y: window.pageYOffset };
      app.ports.onScroll.send(offset);
    },
    { passive: true }
  );
}
