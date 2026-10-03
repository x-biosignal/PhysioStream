# Contributing

Thank you for your interest in contributing to the **x-biosignal** ecosystem
(`PhysioCore`, `PhysioIO`, `PhysioEEG`, and the other `Physio*` packages).
Contributions of all kinds are welcome: bug reports, feature requests,
documentation fixes, and code.

## How development is organised

Each `Physio*` package is published as its own repository under the
[`x-biosignal`](https://github.com/x-biosignal) organisation, and all packages
are available through the r-universe registry at
<https://x-biosignal.r-universe.dev>.

These public repositories are **release mirrors**: the canonical source is a
single upstream development tree, and each public repository is refreshed as a
snapshot at release time. As a practical consequence, a pull request merged
directly into a public mirror would be overwritten by the next release
snapshot. To make sure your contribution is preserved, please follow the
process below.

## Reporting bugs and requesting features

Open an issue on the **Issues** tab of the specific package repository that is
affected (for example, an EEG-loading problem belongs on `x-biosignal/PhysioEEG`;
a core data-model problem belongs on `x-biosignal/PhysioCore`). If you are
unsure which package is responsible, open the issue on
[`x-biosignal/PhysioCore`](https://github.com/x-biosignal/PhysioCore/issues)
and it will be redirected.

A good bug report includes:

- A minimal reproducible example.
- The output of `sessionInfo()` (or at least R version, OS, and installed
  `Physio*` package versions).
- What you expected to happen versus what actually happened.

## Contributing code

1. Open an issue first describing the change, so it can be discussed before you
   invest time.
2. Fork the relevant package repository and prepare your change as a pull
   request against `main`. Even though the mirror is snapshot-based, the pull
   request is how your patch and its discussion are reviewed; accepted changes
   are integrated upstream by the maintainer and appear in a subsequent release
   snapshot (with attribution preserved).
3. Please keep pull requests focused on a single topic.

### Development setup

```bash
# Install the package and its dependencies from r-universe
install.packages(
  "PhysioCore",
  repos = c("https://x-biosignal.r-universe.dev", "https://cloud.r-project.org")
)

# For a source checkout of a single package:
Rscript -e 'devtools::load_all()'   # load for development
Rscript -e 'devtools::test()'       # run the test suite
R CMD check .                        # full package check
```

### Coding conventions

- **S4 classes**: use `setClass()`, `setGeneric()`, `setMethod()`.
- **Function naming**: `verbNoun()` — e.g. `filterSignals()`, `readEDF()`.
- **Documentation**: roxygen2 with `@param`, `@return`, `@export`, `@examples`;
  regenerate with `roxygen2::roxygenise()`.
- **Tests**: `testthat` tests under `tests/testthat/`.
- **Commits**: [Conventional Commits](https://www.conventionalcommits.org/)
  (`feat:`, `fix:`, `docs:`, `test:`, `refactor:`).

## Code of Conduct

By participating in this project you agree to abide by the
[Code of Conduct](CODE_OF_CONDUCT.md).

## Maintainer note

This ecosystem is maintained by a single maintainer
(Yusuke Matsui, Nagoya University). Reviews and responses are handled as time
permits — thank you for your patience.
