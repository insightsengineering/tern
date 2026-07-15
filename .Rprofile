if (Sys.getenv("GITHUB_ACTIONS") == "true") {
  local({
    # Patch pkgdown's cran_link() to handle network failures gracefully.
    # In restricted CI environments, cloud.r-project.org may be unreachable,
    # causing pkgdown::build_site() to fail with a connection error. Since
    # pkgdown 2.x removed the pkgdown.internet option, this wraps cran_link()
    # in a tryCatch so that connection errors return NULL (no CRAN link shown
    # in the sidebar) rather than halting the build.
    if (requireNamespace("pkgdown", quietly = TRUE)) {
      .orig_cran_link <- utils::getFromNamespace("cran_link", "pkgdown")
      utils::assignInNamespace(
        "cran_link",
        function(pkg) tryCatch(.orig_cran_link(pkg), error = function(e) NULL),
        ns = "pkgdown"
      )
    }
  })
}
