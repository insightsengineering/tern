# Labels for adverse event baskets

**\[stable\]**

## Usage

``` r
aesi_label(aesi, scope = NULL)
```

## Arguments

- aesi:

  (`character`)  
  vector with standardized MedDRA query name (e.g. `SMQxxNAM`) or
  customized query name (e.g. `CQxxNAM`).

- scope:

  (`character`)  
  vector with scope of query (e.g. `SMQxxSC`).

## Value

A `string` with the standard label for the AE basket.

## Examples

``` r
adae <- tern_ex_adae

# Standardized query label includes scope.
aesi_label(adae$SMQ01NAM, scope = adae$SMQ01SC)
#> [1] "C.1.1.1.3/B.2.2.3.1 aesi (BROAD)"

# Customized query label.
aesi_label(adae$CQ01NAM)
#> [1] "D.2.1.5.3/A.1.1.1.1 aesi"
```
