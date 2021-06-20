# Contributing

Contributions are welcomed and done by creating a pull request to be landed on
the `main` branch.

See [open issues](https://github.com/jobjo/popper/issues) for some list of items.

If you have any questions, comments or suggestions open a [new
issue](https://github.com/jobjo/popper/issues/new) or get in touch over email.

## Building

Clone the repository and install its dependencies:

```sh
git clone https://github.com/jobjo/popper.git
cd popper
opam install -t --deps-only .
```

There are a few make-targets for convenience:

- `make test` — build project and execute tests.
- `make examples` — build and run examples.
- `make clean` — clean.
- `make format` — runs `ocamlformat` on all files.
- `make api-docs` - build api documentation using `dune build @doc`
- `make serve-docs` - serve non-api documentation from localhost (requires [MkDocs](https://www.mkdocs.org/).
- `make publish-docs` - commits latest api and *mkdocs* documentation on the `gh-pages` branch.

In order to build and test the documentation you need to install
[MkDocs](https://www.mkdocs.org/).

You also need the [Material for
MkDocs](https://github.com/squidfunk/mkdocs-material) plugin, which is installed
by:

```
pip install mkdocs-material
```

To generate and serve the docs, run `make serve-docs` and view at
[http://localhost:8000/](http://localhost:8000/).

### Testing

The `test` folder contains three sub-folders:

 - `expect` with expectation tests for the output.
 - `unit` for unit-testing the library.
 - `util` utilities for generating dune rules and cleaning test output.

 To add a new unit-test:

 - Add to one of the modules in `test/unit` or create a new one
 - If you're adding a new suite, include it in `test/unit/run.ml`.

 To add a new expectation test:

 - Add a new `.ml` file in `test/expect/`.
 - Add a corresponding blank `.expected`  in `test/expected`.
 - Run `make test`.
 - If you're happy with the diff, run `make promote` to promote the change.
 - Confirm that the test pass with `make test` again.

## Project structure

The project is organized as follows:

- `src/lib` contains the `popper` library.
- `src/ppx` defines the ppx `ppx_deriving_popper`.
- `test` contains tests.
- `examples` contains examples.
- `docs` consists for markdown files and are built with `mkdocs`.