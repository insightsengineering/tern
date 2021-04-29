# tern

The `tern` R package contains analysis functions to create tables,
listings and graphs (TLGs) as used in clinical trials. We also provide
the `teal` modules for outputs in `tern` in the `teal.modules.clinical` R package.
For more details see [here](https://go.roche.com/agile-R).

  - Note that tern can be used to create output for regulatory submissions when used in the enableR system. 
  
  - Use of tern in other contexts/systems is for exploratory analysis, quality control, etc.

# Installation

Please refer to the quick start section in agile-R [here](https://go.roche.com/agile-R).

The latest version of `tern` can be installed locally with:

```
devtools::install_github(
  repo = "NEST/tern",
  host = "https://github.roche.com/api/v3",
  upgrade_dependencies = FALSE
)
```

# Presentations

  - [Stats Software Initiative - R Series.
    June 18, 2018](https://docs.google.com/presentation/d/1OB7MMt3YKzfMJ-gXcGpcRqM8tjbMZWqeEki164L38i4/edit?usp=sharing)

# Acknowledgments

There are a number of people who have previously been actively working on `tern` including: Maximilian Mordig, Jennifer Li, Chendi Liao, Yuyao Song, Edgar Manukyan, Carolyn Zhang, Mark Rothe, Xiao Yu Mo.
