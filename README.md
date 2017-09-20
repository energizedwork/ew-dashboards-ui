# EW Dashboards UI


Elm front end for the [energizedwork/ew-dashboards](https://github.com/energizedwork/ew-dashboards) app.

## Local setup

Assuming you have a [Node](https://nodejs.org/en/download/) install.

If you don't already have `elm` and `elm-live`:

``` $ npm install -g elm elm-live ```

Then, to build everything:

``` $ elm-live --output=elm.js src/Main.elm --pushstate --open --debug --warn ```

(Leave off the `--debug` if you don't want the time-traveling debugger.)


## Develop / Test

[elm-live](https://github.com/tomekwi/elm-live) has you covered for auto compile on Elm src & live reload of changes.

### TDD

TODO: bolt in elm-test and run in elm-live

### Formatting

Elm is whitespace sensitive so elm-live uses [elm-format](https://atom.io/packages/elm-format) for consistency. Please follow this convention.



## Deploy
``` $ npm install -g firebase-tools ```

``` $ firebase deploy ```

``` $ open https://dashboards-ui.firebaseapp.com ```
