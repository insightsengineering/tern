# tern <a href='https://github.com/insightsengineering/tern'><img src="man/figures/tern.png" align="right" height="139" style="max-width: 100%;"/></a>

[![Check 🛠](https://github.com/insightsengineering/tern/actions/workflows/check.yaml/badge.svg)](https://github.com/insightsengineering/tern/actions/workflows/check.yaml)
[![Docs 📚](https://github.com/insightsengineering/tern/actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io/tern/)
[![Release 🎈](https://github.com/insightsengineering/tern/actions/workflows/release.yaml/badge.svg)](https://github.com/insightsengineering/tern/releases)

The `tern` R package contains analysis functions to create tables and graphs used for clinical trial reporting.

The package provides a large range of functionality, for example:

<!-- markdownlint-disable MD007 MD030 -->
-   data visualizations:
    -   forest plots
    -   line plots
    -   Kaplan-Meier plots
    -   ...
-   statistical model fits:
    -   logistic regression
    -   Cox regression
    -   ...
-   summary tables:
    -   unique patients
    -   exposure across patients
    -   change from baseline for parameters
    -   ...

<!-- markdownlint-enable MD007 MD030 -->

Many of these outputs are available to be added into `teal` applications for interactive exploration of data. These `teal` modules are available in the [`teal.modules.clinical`](https://insightsengineering.github.io/teal.modules.clinical) package.

## Installation

For releases from August 2022 it is recommended that you [create and use a Github PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("insightsengineering/tern@*release")
```

A stable release of all  `NEST` packages from June 2022 is also available [here](https://github.com/insightsengineering/depository#readme).

In order to run many of the examples you will also need to install the [`scda`](https://insightsengineering.github.io/scda) package.

See package vignettes `browseVignettes(package = "tern")` for usage of this package.

## Acknowledgment

This package is a result of a joint efforts by many developers and stakeholders. We would like to thank everyone who contributed so far!
