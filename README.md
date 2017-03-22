# HLP_ARM_Simulator
HLP Project  - Team 4 - ARM simulator in F#

# GUI

This is the starting point for the GUI, it contains

* Electron to turn a FABLE project into a desktop app

* Gulp to automate project build

* CodeMirror editor component in an app

* FSharp to write syntax-highlight code that integrates with Codemirror editor

* MVC pattern according to ELM structure using Fable-Arch

## Installing the project

Make sure that you are in the `HLP_ARM_Simulator` folder. *All* of the
following commands assume that you are at the root of the project: they will
not work if you are in a sub-folder!

Make sure that you have [`node`](https://nodejs.org/) v6.9 and
[`yarn`](https://yarnpkg.com/) installed. 

You can get [`yarn`](https://yarnpkg.com/) from
[its website](https://yarnpkg.com/en/docs/install), or alternatively you can
use the following command:

```
npm install --global yarn
```


Now you must use [`yarn install`](https://yarnpkg.com/en/docs/cli/install) or simply `yarn`,
which will download all of the necessary dependencies for the project.

After the dependencies are downloaded, it will then automatically compile the
project.

Now you can either start it using **Electron** via the command `yarn go` or open `index.html` in the `dist` directory with a browser of your choice (except probably ie ;))

You can instead use [`yarn run watch`](https://yarnpkg.com/en/docs/cli/run),
which will compile your project (just like
[`yarn`](https://yarnpkg.com/en/docs/cli/install)), but it will also
automatically recompile your project if you make any changes to your project's
files.

If you want to stop watch mode, just hit the `Enter` or `Return` key.

Watch mode is very convenient, and it's also **much** faster, because it only
recompiles the files that actually changed, rather than recompiling your
entire project from scratch.
