 
-   [tern](#tern)
-   [Installation](#installation)
    -   [Stable Version](#stable-version)
    -   [Development Version](#development-version)
-   [Development](#development)
    -   [Issues](#issues)
    -   [Collaboration](#collaboration)
        -   [Creating Pull Requests](#creating-pull-requests)
-   [Presentations](#presentations)

<!-- README.md is generated from README.Rmd. Please edit that file -->
tern 
====

The `tern` R package contains analysis functions to create tables, listings and graphs (TLGs) as used in clinical trials. We also provide [teal](https://github.roche.com/NEST/teal) modules for outputs in `tern` in the [teal.modules.clinical](https://github.roche.com/NEST/teal.modules.clinical) R package.

-   Note that currently **tern may not be used for regulatory submissions**

-   However you can use `tern` for exploratory analysis and for quality control

For the documentation please refer to the official [documentation webpage](https://pages.github.roche.com/NEST/docs/).

Installation
============

Stable Version
--------------

``` r
devtools::install_github(
  repo = "NEST/random.cdisc.data",
  ref = "master", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE
)

devtools::install_github("Roche/rtables")

devtools::install_github(
  repo = "NEST/tern",
  ref = "master", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE
)
```

Development Version
-------------------

To install the current development version of `tern` run:

``` r
devtools::install_github(
  repo = "NEST/random.cdisc.data",
  ref = "devel", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE
)

devtools::install_github("Roche/rtables", ref = "devel", upgrade_dependencies = FALSE)

devtools::install_github(
  repo = "NEST/tern",
  ref = "devel", 
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE
)
```

Development
===========

Issues
------

We hope that `tern` is useful for your everyday work. If you would like more TLGs or you need more configuration options for the existing functions then please fill out a [`github` issue](https://github.roche.com/NEST/tern/issues) with your request and the reason/context for your request.

-   Please do not submit issues with ideas for useful features for current `tern` functions if you do not necessarily need them for your project. We are aware that many of `tern`'s functions would benefit from additional arguments in order to control the outputs in more detail. However, we would like to avoid argument explosion and one way to do that is to get more experience with `tern` *in the field* and then slowly grow the flexibility based on need.

-   Please do not paste outputs with sensitive information into the issues. If possible make a minimal working example with `random.cdisc.data` data.

Collaboration
-------------

We would like `tern` to be a collaborative effort and your contributions are welcome! However please read the following points before you make any contributions:

-   talk to the respective function authors before you start working on changes other than bug fixes or documentation examples (this includes argument changes)

    -   please have a look at the [R code](https://github.roche.com/NEST/tern/tree/master/R) of a couple of functions in `tern`. If you are new to `R` or do not want to spend the effort to create similarly general and robust functions then please consider contributing to the [osprey](https://github.roche.com/Rpackages/osprey) R package.

### Creating Pull Requests

-   Please make pull requests to the `master` branch (i.e. base is master)

-   We then make new tagged releases on the master branch once the `master` branch has been thoroughly tested

Presentations
=============

-   [Stats Software Initiative - R Series. June 18, 2018](https://docs.google.com/presentation/d/1OB7MMt3YKzfMJ-gXcGpcRqM8tjbMZWqeEki164L38i4/edit?usp=sharing)
