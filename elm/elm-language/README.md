## Elm experiment
Sample code for learning purpose.
This project is bootstrapped with [Create Elm App](https://github.com/halfzebra/create-elm-app).

## Installing Elm packages

```sh
elm-app install <package-name>
```

Or install all packages in `elm-package.json`

```sh
elm-app install
```

Other `elm-package` commands are also [available.](#package)

## Installing JavaScript packages

To use JavaScript packages from npm, you'll need to add a `package.json`, install the dependencies, and you're ready to go.

```sh
npm init -y # Add package.json
npm install --save-dev pouchdb-browser # Install library from npm
```

```js
// Use in your JS code
import PouchDB from 'pouchdb-browser';
const db = new PouchDB('mydb');
```

## Starting application

```sh
elm-app start
```
