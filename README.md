# tern <a href='https://github.com/insightsengineering/tern'><img src="man/figures/logo.png" align="right" height="200" width="200"/></a>

<!-- start badges -->
[![Check 🛠](https://github.com/insightsengineering/tern/actions/workflows/check.yaml/badge.svg)](https://insightsengineering.github.io/tern/main/unit-test-report/)
[![Docs 📚](https://github.com/insightsengineering/tern/actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io/tern/)
[![Code Coverage 📔](https://raw.githubusercontent.com/insightsengineering/tern/_xml_coverage_reports/data/main/badge.svg)](https://insightsengineering.github.io/tern/main/coverage-report/)

![GitHub forks](https://img.shields.io/github/forks/insightsengineering/tern?style=social)
![GitHub repo stars](https://img.shields.io/github/stars/insightsengineering/tern?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/insightsengineering/tern)
![GitHub contributors](https://img.shields.io/github/contributors/insightsengineering/tern)
![GitHub last commit](https://img.shields.io/github/last-commit/insightsengineering/tern)
![GitHub pull requests](https://img.shields.io/github/issues-pr/insightsengineering/tern)
![GitHub repo size](https://img.shields.io/github/repo-size/insightsengineering/tern)
![GitHub language count](https://img.shields.io/github/languages/count/insightsengineering/tern)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/insightsengineering/tern/main?color=purple\&label=package%20version)](https://github.com/insightsengineering/tern/tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/insightsengineering/tern?color=red\&label=open%20issues)](https://github.com/insightsengineering/tern/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->

The `tern` R package contains analysis functions to create tables and graphs used for clinical trial reporting.

The package provides a large range of functionality, such as:

<!-- markdownlint-disable MD007 MD030 -->

Data visualizations:

-   Line plots ([`g_lineplot`](https://insightsengineering.github.io/tern/latest-tag/reference/g_lineplot.html))
-   Kaplan-Meier plots ([`g_km`](https://insightsengineering.github.io/tern/latest-tag/reference/g_km.html))
-   Forest plots ([`g_forest`](https://insightsengineering.github.io/tern/latest-tag/reference/g_forest.html))
-   STEP graphs ([`g_step`](https://insightsengineering.github.io/tern/latest-tag/reference/g_step.html))
-   Individual patient plots ([`g_ipp`](https://insightsengineering.github.io/tern/latest-tag/reference/individual_patient_plot.html))
-   Waterfall plots ([`g_waterfall`](https://insightsengineering.github.io/tern/latest-tag/reference/g_waterfall.html))

Statistical model fit summaries:

-   Logistic regression ([`summarize_logistic`](https://insightsengineering.github.io/tern/latest-tag/reference/logistic_regression.html))
-   Cox regression ([`summarize_coxreg`](https://insightsengineering.github.io/tern/latest-tag/reference/cox_regression.html))

Analysis tables:

-   See a list of all available analyze functions [here](https://insightsengineering.github.io/tern/latest-tag/reference/analyze_functions.html)
-   See a list of all available summarize functions [here](https://insightsengineering.github.io/tern/latest-tag/reference/summarize_functions.html)
-   See a list of all available column-wise analysis functions [here](https://insightsengineering.github.io/tern/latest-tag/reference/analyze_colvars_functions.html)

<!-- markdownlint-enable MD007 MD030 -->

Many of these outputs are available to be added into [`teal`](https://insightsengineering.github.io/teal/) shiny applications for interactive exploration of data. These `teal` modules are available in the [`teal.modules.clinical`](https://insightsengineering.github.io/teal.modules.clinical/) package.

See the [TLG Catalog](https://insightsengineering.github.io/tlg-catalog/) for an extensive catalog of example clinical trial tables, listings, and graphs created using `tern` functionality.

## Installation

`tern` is available on CRAN and you can install the latest released version with:

```r
install.packages("tern")
```

or you can install the latest development version directly from GitHub by running the following:

```r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("insightsengineering/tern")
```

Note that it is recommended you [create and use a `GITHUB_PAT`](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token) if installing from GitHub.

See package vignettes `browseVignettes(package = "tern")` for usage of this package.

## Related

- [`rtables`](https://insightsengineering.github.io/rtables/) - table engine used
- [`tlg-catalog`](https://insightsengineering.github.io/tlg-catalog/) - website showcasing many examples of clinical trial tables, listings, and graphs
- [`teal.modules.clinical`](https://insightsengineering.github.io/teal.modules.clinical/) - `teal` modules for interactive data analysis

## Acknowledgment

This package is a result of a joint efforts by many developers and stakeholders. We would like to thank everyone who has contributed so far!

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/tern.svg)](https://starchart.cc/insightsengineering/tern)

### Stargazers

[![Stargazers repo roster for tern](https://reporoster.com/stars/insightsengineering/tern)](https://github.com/insightsengineering/tern/stargazers)

[![Forkers repo roster for tern](https://reporoster.com/forks/insightsengineering/tern)](https://github.com/insightsengineering/tern/network/members)
