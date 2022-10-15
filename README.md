# Parallel Type Inference
Basis for a parallel by construction type inference


## Build
```
stack build --docker
```
Unfortunately, it does not work at the moment

At the moment, the only way is to connect to a [devcontainer](#dev-container) and build from there:
```sh
docker ps # find container's ID
docker exec -ti [ID] /bin/bash
$ cd /workspaces/[workspace name]
$ stack build
```

## Development
This project relies on `accelerate` library which has a number of its own dependencies. Check accelerate's documentationfor the details.

## Dev Container
There is a docker image at `nihon/compiler-dev` with all the dependencies installed. You can use it with VS Code's `Dev Containers` plugin.

*Note*: Accelerate takes up some time to build on the first run, as well as takes up up to 6 GB RAM.

*Note*: For an unknown reason, sometimes "haskell.serverExecutablePath" option is not set to "haskell-language-server-8.10.7", although it is specified in devcontainer.json. If you start getting gradle errors, try setting it up manually.

## Make changes to the language
First make sure that you have *bnfc*, *happy* and *alex* installed
```
stack install BNFC
stack install happy
stack install alex
```

Change the grammar file, `Lang.cf`, and then generate lexer and parser for the updated language with
```
make generate
```

## Adding new languages
Add a Language.cf file with LBNF of your language to the *languages* folder and run
```
bnfc -d -o src languages/Language.cf

# You can also make a subfolder
bnfc -d -o src -p Frontend languages/Language.cf
```
