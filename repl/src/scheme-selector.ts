/**
 * This is a light or dark color scheme selector custom element.
 * Initially, it defaults to auto-detecting the user's setting.
 * But they user may select either light or dark mode (and store that decision in LocalStorage).
 *
 * Inspired by https://whitep4nth3r.com/blog/best-light-dark-mode-theme-toggle-javascript/
 */

export type Scheme = "light" | "dark";
type Option = "auto" | Scheme;
export const prefer = "prefers-color-scheme";

function parseOption(
  string: string | undefined | null | FormDataEntryValue,
): Option {
  switch (string) {
    case "light":
    case "dark":
      return string;
    default:
      return "auto";
  }
}

const prefersDarkQuery = "(prefers-color-scheme: dark)";

export function detectPreferredColorScheme(): Scheme {
  const parsed = parseOption(localStorage.getItem(prefer));
  if (parsed !== "auto") return parsed;

  try {
    return window.matchMedia(prefersDarkQuery).matches ? "dark" : "light";
  } catch {
    return "light";
  }
}

export class ColorSchemeSelector extends HTMLElement {
  private scheme: Option;
  private matchMedia: MediaQueryList | null = null;
  private form: HTMLFormElement;

  constructor() {
    super();
    this.form = this.appendChild(document.createElement("form"));
    this.form.appendChild(this.mkInput("light", "light"));
    this.form.appendChild(this.mkInput("a", "auto"));
    this.form.appendChild(this.mkInput("dark", "dark"));
  }

  connectedCallback() {
    //@ts-ignore TODO remove after initial debugging
    this.addEventListener<CustomEvent>(prefer, (ev: CustomEvent) =>
      console.debug("Got event", prefer, ev.detail),
    );

    this.matchMedia = window.matchMedia(prefersDarkQuery);

    this.scheme = parseOption(localStorage.getItem(prefer));
    if (this.scheme === "auto") {
      this.emitAndWatchMatchMedia();
    } else {
      this.emit(this.scheme);
    }
    this.form.querySelector<HTMLInputElement>(`input#${this.scheme}`)?.click();

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
    // Icons from https://github.com/tailwindlabs/heroicons/blob/master/LICENSE
    switch (scheme) {
      case "light":
        node.innerHTML =
          '<svg fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" d="M12 3v2.25m6.364.386-1.591 1.591M21 12h-2.25m-.386 6.364-1.591-1.591M12 18.75V21m-4.773-4.227-1.591 1.591M5.25 12H3m4.227-4.773L5.636 5.636M15.75 12a3.75 3.75 0 1 1-7.5 0 3.75 3.75 0 0 1 7.5 0Z"></path></svg>';
        break;
      case "dark":
        node.innerHTML =
          '<svg fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" d="M21.752 15.002A9.72 9.72 0 0 1 18 15.75c-5.385 0-9.75-4.365-9.75-9.75 0-1.33.266-2.597.748-3.752A9.753 9.753 0 0 0 3 11.25C3 16.635 7.365 21 12.75 21a9.753 9.753 0 0 0 9.002-5.998Z"></path></svg>';
        break;
      case "auto":
        node.innerHTML =
          '<svg fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor"><path stroke-linecap="round" stroke-linejoin="round" d="M7.864 4.243A7.5 7.5 0 0 1 19.5 10.5c0 2.92-.556 5.709-1.568 8.268M5.742 6.364A7.465 7.465 0 0 0 4.5 10.5a7.464 7.464 0 0 1-1.15 3.993m1.989 3.559A11.209 11.209 0 0 0 8.25 10.5a3.75 3.75 0 1 1 7.5 0c0 .527-.021 1.049-.064 1.565M12 10.5a14.94 14.94 0 0 1-3.6 9.75m6.633-4.596a18.666 18.666 0 0 1-2.485 5.33"></path></svg>';
        break;
      default:
        node.textContent = caption;
    }

    const input = document.createElement("input");
    input.type = "radio";
    input.name = prefer;
    input.id = scheme;
    input.value = scheme;
    input.ariaDescription = `Select ${caption} color scheme`;
    node.insertBefore(input, node.firstChild);
    // node.appendChild(input);

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
      new CustomEvent(prefer, { bubbles: true, detail: value }),
    );
    document.documentElement.dataset.theme = value;
  }

  private emitAndWatchMatchMedia() {
    this.emitMatchMedia();
    if (!this.matchMedia) return;
    this.matchMedia.onchange = this.emitMatchMedia.bind(this);
  }

  private emitMatchMedia() {
    if (!this.matchMedia) return;
    this.emit(this.matchMedia.matches ? "dark" : "light");
  }
}
