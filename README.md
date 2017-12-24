This project is a small example of [servant-server](http://haskell-servant.readthedocs.io/), [persistent](https://www.stackage.org/package/persistent) and [pux](http://purescript-pux.org/).

# Server Side

## run server

You can build and run the project with [stack](http://haskellstack.org/), e.g.:

``` bash
stack docker pull
stack build
stack --docker-run-args='--net=bridge --publish=3000:3000' exec example-servant-persistent
```

Then you can query the server from a separate shell:

``` bash
curl -H 'Content-type: application/json' localhost:3000/user/add --data '{"name": "Alice", "age": 42}'
curl -H 'Content-type: application/json' localhost:3000/user/get/Alice
```

## purescript model & api query function generate

``` bash
stack exec psGenerator
```

## api document generate

``` bash
stack exec example-servant-persistent-doc
```

# Client Side

``` bash
cd client
npm install
npm run build
```

Then you can browse top page at http://localhost:3000
