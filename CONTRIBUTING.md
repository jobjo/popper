# Contributing

Contributions are welcomed and done by creating a Pull Request to be landed on
the `main` branch.

See  [open issues](https://github.com/jobjo/popper/issues). The ones labeled
with `good first issue` may be best if you're new to the project.

## Building

Clone the repository and install its dependencies:

```sh
git clone https://github.com/jobjo/popper.git
cd popper
opam install -t --deps-only .
```

There are a few Make targets for convenience:

- `make test` — build project and execute tests.
- `make examples` — build and run examples.
- `make clean` — clean.
- `make doc` - generate docs on the `gh-pages` branch and requires having `MkDocs` installed.

In order to build and test the documentation you need to install
[MkDocs](https://www.mkdocs.org/).

The documentation is published on the `gh-pages` 

## Project structure

The project is organized as follows:

- `src/lib` contains the `popper` library.
- `src/ppx` defines the ppx `ppx_deriving_popper`.
- `test` contains tests.
- `examples` contains examples.
- `docs` consists for markdown files and are built with `mkdocs`.

