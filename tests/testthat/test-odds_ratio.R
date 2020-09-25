
test_that("`or_glm` estimates right OR and CI", {
  data <- data.frame(
    rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1, 0, 0)),
    grp = letters[c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3)]
  )

  data_ab <- subset(data, grp %in% c("a", "b"))
  data_ab$grp <- droplevels(data_ab$grp)

  result <- or_glm(data_ab, conf_level = 0.95)[[1]]

  expected <- c(or = 1 / 2 / 2 / 1, or_lcl = 0.0083, or_ucl = 7.4518)
  expect_equal(result, expected, tolerance = 1e-4)

  # Because `rtables` works by column (compared to the reference), we verified
  # that the model fitted on the complete dataset (grp: a, b, c) provides equal
  # estimations to the model fitted to the subset group and reference (grp: a, b).
  model_fit <- glm(rsp ~ grp, data, family = stats::binomial(link = "logit"))
  expected <- c(
    exp(stats::coef(model_fit)[-1])["grpb"],
    exp(stats::confint.default(model_fit, level = 0.95)["grpb", ])
    )
  expect_equal(result, expected, tolerance = 1e-4, check.attributes = FALSE)
})


test_that("`s_odds_ratio` estimates right OR and CI", {

  data <- data.frame(
    rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1, 0, 0)),
    grp = letters[c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3)]
  )

  result <- s_odds_ratio(
      df = subset(data, grp == "b"),
      .var = "rsp",
      .ref_group = subset(data, grp == "a"),
      .in_ref_col = FALSE
    )

  expected <- list(
    or_ci = with_label(
      c(or = 1 / 2 / 2 / 1, or_lcl = 0.0083, or_ucl = 7.4518),
      "Odds Ratio (95%CI)"
    )
  )
  expect_equal(result, expected, tolerance = 1e-4)

})
