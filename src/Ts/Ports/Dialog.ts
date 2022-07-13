export function Dialog(app: any) {
  app.ports.toggleDialog.subscribe((id: string) => {
    const dialog = document.querySelector(`#${id}`) as HTMLDialogElement;

    if (dialog.open) {
      dialog.close?.();
    } else {
      dialog.showModal?.();
    }
  });
}
