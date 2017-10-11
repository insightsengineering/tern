
[Project Website][ghs]

# teal.oncology

This R package contains teal modules for analysing oncology clinical trials 
data. The package is in its building phase and under active development. Hence
do not use these modules for production work.


# Installation

You need to install the `teal` version that is currently located on the `beta`
branch.

``` r
devtools::install_git(
  url = "http://github.roche.com/Rpackages/teal.git",
  build_vignettes = TRUE,
  upgrade_dependencies = FALSE,
  branch = "beta"
)
```

You can then install `teal.oncology` with

``` r
devtools::install_git(
  url = "http://github.roche.com/Rpackages/teal.oncology.git",
  build_vignettes = TRUE,
  upgrade_dependencies = FALSE
)
```

# Getting Started

Each teal module in `teal.oncology` will be explained in a separate vignette and
is accessile via the articles tab on the [project site][ghs].


[ghs]: http://pages.github.roche.com/Rpackages/teal.oncology