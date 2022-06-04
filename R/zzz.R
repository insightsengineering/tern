color_palette_core <- function(palette = "nest", n = 10) {
  checkmate::assert_string(palette)
  match.arg(palette, c("nest", "stream", "viridis"))
  checkmate::assert_number(n)

  set.seed(124)
  colors <- if (palette == "viridis") {
    rep(c(
      scales::col_bin(
        c(
          "#0D0887FF", "#1C068DFF", "#270592FF", "#310597FF", "#3B049AFF", "#44039EFF", "#4C02A1FF",
          "#5502A4FF", "#5D01A6FF", "#6600A7FF", "#6E00A8FF", "#7601A8FF", "#7E03A8FF", "#8606A6FF",
          "#8E0BA5FF", "#9511A1FF", "#9C179EFF", "#A21D9AFF", "#A92395FF", "#B02991FF", "#B52F8CFF",
          "#BC3488FF", "#C13B82FF", "#C6417DFF", "#CC4678FF", "#D04D73FF", "#D5536FFF", "#DA596AFF",
          "#DE5F65FF", "#E26561FF", "#E56B5DFF", "#E97257FF", "#ED7953FF", "#F07F4FFF", "#F3864AFF",
          "#F68D45FF", "#F89441FF", "#FA9B3CFF", "#FCA338FF", "#FDAB33FF", "#FDB32FFF", "#FEBB2BFF",
          "#FDC328FF", "#FDCC26FF", "#FBD424FF", "#F9DD25FF", "#F6E726FF", "#F3EF27FF", "#F0F921FF"
        ),
        1:49
      )(sample(1:49, 49)),
      ceiling(n / 49)
    ))
  } else if (palette == "stream") {
    rep(
      c(
        "#343cff", "#ff484b", "#232323", "#329032", "#ffa41c", "#750375", "#1d831c", "#767676",
        "#9b2525", "#008080", "#ff1f8e", "#bdb771", "#ffd92d", "#000bff", "#ff0004", "#090909",
        "#379336", "#ffa722", "#750375", "#2f9999", "#ff2c95", "#bfb976", "#ffdd41", "#3438ff",
        "#ff0004", "#000000", "#047503", "#ff9a05", "#7a0d7a", "#a94343", "#008080", "#ffa825",
        "#ff1388", "#bdb771", "#ffe258", "#141dff", "#ff0004", "#000000"
      ),
      ceiling(n / 38)
    )
  } else if (palette == "nest") {
    rep(
      c(
        "#ff2951ff", "#2995ffff", "#81832Bff", "#AC1CF8", "#1CF853", "#F8761C", "#F8341C",
        "#F81CC5", "#2D1CF8", "#1CF8DC", "#6EF81C", "#F8B81C", "#F81C25", "#F81C85", "#EC1CF8",
        "#6D1CF8", "#1C4BF8", "#1CD0F8", "#1CF898", "#2AF81C", "#B3F81C", "#F8D91C", "#F8971C",
        "#F8551C", "#F8241C", "#F81C35", "#F81C65", "#F81CA5", "#F81CE5", "#CC1CF8", "#8D1CF8",
        "#4D1CF8", "#1C2BF8", "#1C6BF8", "#1CAEF8", "#1CF2F8", "#1CF8BA", "#1CF876", "#1CF831",
        "#4CF81C", "#91F81C", "#D5F81C", "#F8E91C", "#F8C91C", "#F8A81C", "#F8871C", "#F8661C",
        "#F8451C", "#F81C1D", "#F81C2D", "#F81C3D", "#F81C55", "#F81C75", "#F81C95", "#F81CB5",
        "#F81CD5", "#F81CF5", "#DC1CF8", "#BC1CF8", "#9D1CF8", "#7D1CF8", "#5D1CF8", "#3D1CF8",
        "#1D1CF8", "#1C3BF8", "#1C5BF8", "#1C7BF8", "#1C9DF8", "#1CBFF8", "#1CE1F8", "#1CF8ED",
        "#1CF8CB", "#1CF8A9", "#1CF887", "#1CF865", "#1CF842", "#1CF820", "#3BF81C", "#5DF81C",
        "#80F81C", "#A2F81C", "#C4F81C", "#E6F81C", "#F8F11C", "#F8E11C", "#F8D11C", "#F8C11C",
        "#F8B01C", "#F8A01C", "#F88F1C", "#F87F1C", "#F86E1C", "#F85E1C", "#F84D1C", "#F83D1C",
        "#F82C1C"
      )[seq_len(96)],
      ceiling(n / 96)
    )
  }

  return(colors[seq_len(n)])
}


.onLoad <- function(libname, pkgname) { # nolint
  # adapted from https://github.com/r-lib/devtools/blob/master/R/zzz.R
  tern_default_options <- list(
    "tern.color" <- color_palette_core("stream")
  )
  op <- options()
  toset <- !(names(tern_default_options) %in% names(op))
  if (any(toset)) options(tern_default_options[toset])

  invisible()
}
