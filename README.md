# EW Dashboards UI


Elm front end for the [energizedwork/ew-dashboards](https://github.com/energizedwork/ew-dashboards) app.

## Local setup

Assuming you have a [Node](https://nodejs.org/en/download/) install.

```$ npm install```

(Leave off the `--debug` in package.json if you don't want the time-traveling debugger.)


## Develop / Test

[elm-live](https://github.com/tomekwi/elm-live) has you covered for auto compilation of Elm src & live reload of changes. It's wrapped via NPM to allow easy install and to allow tests to run concurrently:


```bash
# run the dev server + local CI
$ npm run start:test:watch

# run the tests only
$ npm run test:watch
```


### Formatting

Elm is whitespace sensitive so elm-live uses [elm-format](https://atom.io/packages/elm-format) for consistency. Please follow this convention.


## Deploy
``` $ npm install -g firebase-tools ```

``` $ firebase deploy ```

``` $ open https://dashboards-ui.firebaseapp.com ```
