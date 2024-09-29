# Draft for the index page

icon elm.run

How can I run Elm code?

I'm trying to gather different options

Most developers will want to download the Elm compiler and run it locally on their machine.
For them following https://guide.elm-lang.org/install/

This page is mostly about running Elm code in your browser, though.

Many options are available

## REPL

https://elm-lang.org/try is the same editor as on https://social.elm.studio created by Evan Czaplicki and Teresa Sokol
Source code is available as part of the [elm-lang.org website repository](https://github.com/elm/elm-lang.org/tree/8193eb7552e26989d444167d6006eb50616ff554/editor).

With it, you can edit one Elm file on the left side of the view, and can compile and interact with the application on the right side.
It allows you to install Elm packages.

It connects to the [Elm compiler worker server](https://github.com/elm/compiler/tree/2f6dd29258e880dbb7effd57a829a0470d8da48b/worker) to install packages and to compile the Elm code.

It uses a [custom (smaller) codemirror 5](https://github.com/elm/elm-lang.org/blob/8193eb7552e26989d444167d6006eb50616ff554/editor/cm/version.txt) as the code editor.

---

The REPL in the [official Elm guide](https://guide.elm-lang.org/core_language) uses the same backand as the elm-lang.org/try editor.

It seems to be closed source itself.

With it you can use and import other modules from the Elm core package, but cannot pick other ones.

You enter one line at a time and send it to the server. It will either allow you to enter more code, or you can force a compilation attempt by pressing enter twice.

---

https://elmrepl.de
On https://elmrepl.de you see two panes: In the top one, you can freely create one Main.elm file and in the bottom one you have a REPL that imports your Main.elm file and can help you to explore the code.

The source code is available on https://github.com/tomkarp/elmrepl

When you use it, a docker container is started and you are connected via [xterm.js](https://xtermjs.org/) to your own private container.
The top pane uses the [Monaco editor](https://microsoft.github.io/monaco-editor/), which also powers [VS Code](https://github.com/microsoft/vscode).

---

https://elm-notebook.org allows you to create notebooks consisting of cells, they can be either text or Elm source code that can be evaluated.
You can create an account and create notebooks to store them, can change them to be publicly available.

Source is available https://github.com/jxxcarlson/elm-notebook-v2

It uses https://repl.lamdera.com as a backend to run the Elm compiler there.

---

https://github.com/axelerator/a-tour-of-elm

https://a-tour-of-elm.axelerator.de starts first with explaining html and css, but starting with https://a-tour-of-elm.axelerator.de/#ElmLang you get to change one Elm and one Html file and can compile and see it

As the name implies, it is a guided tour of Elm. And as a tutorial it does not support saving the content that you entered.

---

https://ellie-app.com/

Consists of two editor views and one preview section. You can edit one Main.elm file and the html file that will then load it.
That way you can also work with ports and add javascript or use css.
It allows to install packages and to store the progress on the server.
Can also run elm-format on the server to format the code.

Source code at https://github.com/ellie-app/ellie

---

https://elm-editor.com/

Source code at https://github.com/pine-vm/pine/tree/main/implement/example-apps/elm-editor

You can work with multiple Elm files, but cannot use css, html or js files.
You can use arbitrary packages, but it does not provide an easy installer interface. Instead you need to edit the elm.json file manually.

You can import your workspace from a git repository or zip archive and can also download it again as a zip archive.
It can also create links that contain the differences to the loaded git repository for easy sharing.

The initial main workflow for elm-editor was for teaching in a classroom: The teacher would prepare a git repository that everyone could load, and then they could easily share their changes (as diff).

It uses the [Monaco editor](https://microsoft.github.io/monaco-editor/), which also powers [VS Code](https://github.com/microsoft/vscode).

Can also run elm-format on the server to format the code.

---

miniBill's elm-interpreter UI
https://elm.run/minibill-elm-interpreter

Source code at https://github.com/miniBill/elm-interpreter
https://github.com/miniBill/elm-interpreter/blob/main/src/UI.elm

Is hard to describe, but you can enter an arbitrary Elm expression and explore what happens in each step of the way.
Gives buttons to view each sub-expression, shows the available state at that time and also the result.
