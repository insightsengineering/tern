
-   [tern](#tern)
-   [Installation](#installation)
    -   [Stable Version](#stable-version)
    -   [Development Version](#development-version)
-   [Development](#development)
    -   [Issues](#issues)
    -   [Collaboration](#collaboration)
        -   [Creating Pull Requests](#creating-pull-requests)

<!-- README.md is generated from README.Rmd. Please edit that file -->
tern
====

The `tern` R package contains analysis functions to create tables, listings and graphs (TLGs) as used in clinical trials. We also provide [teal](https://github.roche.com/Rpackages/teal) modules for outputs in `tern` in the [teal.tern](https://github.roche.com/Rpackages/teal.oncology) R package.

-   Note that currently **tern may not be used for regulatory submissions**

-   However you can use `tern` for exploratory analysis and for quality control

Installation
============

Stable Version
--------------

``` r
devtools::install_github(
  repo = "Rpackages/random.cdisc.data",
  ref = "v0.1.0", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE
)

devtools::install_github("Roche/rtables", ref = "v0.1.0", upgrade_dependencies = FALSE)

devtools::install_github(
  repo = "Rpackages/tern",
  ref = "v0.5.0", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE
)
```

Development Version
-------------------

To install the current development version of `tern` run:

``` r
devtools::install_github(
  repo = "Rpackages/random.cdisc.data",
  ref = "master", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE
)

devtools::install_github("Roche/rtables", ref = "devel", upgrade_dependencies = FALSE)

devtools::install_github(
  repo = "Rpackages/tern",
  ref = "master", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE
)
```

Development
===========

Issues
------

We hope that `tern` is useful for your everyday work. If you would like more TLGs or you need more configuration options for the existing functions then please fill out a [github issue](https://github.roche.com/Rpackages/tern/issues) with your request and the reason/context for your request.

-   Please do not submit issues with ideas for useful features for current `tern` functions if you do not necessarily need them for your project. We are aware that many of `tern`'s functions would benefit from additional arguments in order to control the outputs in more detail. However, we would like to avoid argument explosion and one way to do that is to get more experience with `tern` *in the field* and then slowly grow the flexibility based on need.

-   Please do not paste outputs with sensitive information into the issues. If possible make a miniamal working example with `random.cdisc.data` data.

Collaboration
-------------

We would like `tern` to be a collaborative effort and your contributions are welcome! However please read the following points before you make any contributions:

-   talk to the repsective function authors before you start working on changes other than bug fixes or documentation examples (this includes argument changes)

-   see in our [planned TLGs project page](https://github.roche.com/Rpackages/tern/projects/3) whether the TLG you need has been requested or is already under development. Otherwise talk to [Adrian Waddell](mailto:adrian.waddell@roche.com) if you urgently need a TLG or if you would like to contribute a new output to `tern`

    -   please have a look at the [R code](https://github.roche.com/Rpackages/tern/tree/master/R) of a couple of functions in `tern`. If you are new to `R` or do not want to spend the effort to create similarly general and robust functions then please consider contributing to the [osprey](https://github.roche.com/Rpackages/osprey) R package.

### Creating Pull Requests

-   Please make pull requests to the `master` branch (i.e. base is master)

-   We then make new tagged releases on the master branch once the `master` branch has been thoroughly tested
