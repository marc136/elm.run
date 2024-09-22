// Adapted from https://discourse.elm-lang.org/t/native-html-element-dialog-modal-opening-closing/9232

export class ModalDialog extends HTMLElement {
  static observedAttributes = ["open"];

  connectedCallback() {
    this.refresh();
  }

  attributeChangedCallback(name: string, oldValue: string, newValue: string) {
    switch (name) {
      case "open":
        console.warn("changed open attribute", this);
    }
    if (this.isConnected) this.refresh();
  }

  private refresh() {
    const isOpen = this.getAttribute("open") !== null;
    const dialog = this.querySelector("dialog");
    if (dialog) {
      if (isOpen) {
        dialog.onclick = (event) => {
          // closing only if clicked on scrim, not on the filler
          if (event.currentTarget === event.target) dialog.close();
        };
        dialog.showModal();
      } else {
        dialog.close();
      }
    }
  }
}
