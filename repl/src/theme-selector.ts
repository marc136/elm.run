/**
 * This is a light or dark color scheme selector custom element.
 * Initially, it defaults to auto-detecting the user's setting.
 * But they user may select either light or dark mode (and store that decision in LocalStorage).
 * 
 * Inspired by https://whitep4nth3r.com/blog/best-light-dark-mode-theme-toggle-javascript/
 */

export type Scheme = "light" | "dark";
type Option = 'auto' | Scheme
export const prefer = "prefers-color-scheme";

function parseOption(string: string | undefined | null | FormDataEntryValue): Option {
  switch (string) {
    case 'light':
    case 'dark':
      return string;
    default:
      return 'auto';
  }
}

export function detectPreferredColorScheme(): Scheme {
  const parsed = parseOption(localStorage.getItem(prefer));
  if (parsed !== 'auto') return parsed;

  try {
    return window.matchMedia("(prefers-color-scheme: dark)").matches ? 'dark' : 'light';
  } catch {
    return 'light';
  }
}

export class ColorSchemeSelector extends HTMLElement {
  private scheme: Option;
  private matchMedia: MediaQueryList | null = null;
  private form: HTMLFormElement;

  constructor() {
    super();
    this.form = this.appendChild(document.createElement("form"));
    this.form.appendChild(this.mkInput("auto", "auto"));
    this.form.appendChild(this.mkInput("light", "light"));
    this.form.appendChild(this.mkInput("dark", "dark"));
  }

  connectedCallback() {
    //@ts-ignore TODO remove after initial debugging
    this.addEventListener<CustomEvent>(prefer, (ev: CustomEvent) =>
      console.debug("Got event", prefer, ev.detail)
    );

    this.matchMedia = window.matchMedia("(prefers-color-scheme: dark)");

    this.scheme = parseOption(localStorage.getItem(prefer));
    if (this.scheme === 'auto') {
      this.emitAndWatchMatchMedia();
    } else {
      this.emit(this.scheme)
    }
    this.form.querySelector<HTMLInputElement>(`input#${this.scheme}`)?.click()

    this.form.onchange = () => {
      this.scheme = parseOption(new FormData(this.form).get(prefer));
      console.warn("form prefers", this.scheme);
      localStorage.setItem(prefer, this.scheme);
      switch (this.scheme) {
        case "auto":
          this.emitAndWatchMatchMedia();
          localStorage.removeItem(prefer);
          break;
        default:
          localStorage.setItem(prefer, this.scheme);
          if (this.matchMedia) {
            this.matchMedia.onchange = null;
          }
          this.emit(this.scheme);
      }
    };
  }

  private mkInput(caption: string, scheme: Option) {
    const node = document.createElement("label");
    node.appendChild(document.createTextNode(caption));
    const input = document.createElement("input");
    input.type = "radio";
    input.name = prefer;
    input.id = scheme;
    input.value = scheme;
    input.ariaDescription = `Select ${caption} color scheme`;
    node.appendChild(input);

    if (this.scheme === scheme) {
      input.checked = true;
    }

    return node;
  }

  disconnectedCallback() {
    this.form.onchanged = null;
    if (this.matchMedia) {
      this.matchMedia.onchange = null;
    }
    this.matchMedia = null;
  }

  private emit(value: Scheme) {
    console.info("dispatching", prefer, { value });
    this.dispatchEvent(
      new CustomEvent(prefer, { bubbles: true, detail: value })
    );
    document.documentElement.dataset.theme = value;
  }

  private emitAndWatchMatchMedia() {
    this.emitMatchMedia()
    if (!this.matchMedia) return;
    this.matchMedia.onchange = this.emitMatchMedia.bind(this);
  }

  private emitMatchMedia() {
    if (!this.matchMedia) return;
    this.emit(this.matchMedia.matches ? "dark" : "light");
  }
}
