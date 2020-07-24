context("test t_abn_shift")

library(dplyr)

test_that("Abnormality shift table", {
  asl <- read.table(
    header = TRUE,
    sep = ";",
    stringsAsFactors = FALSE,
    strip.white = TRUE,
    text = '
      "USUBJID";"ARM"
      "AB12345-JPN-3-id-109";"B: Placebo"
      "AB12345-PAK-1-id-112";"B: Placebo"
      "AB12345-BRA-1-id-105";"A: Drug X"
      "AB12345-USA-11-id-100";"C: Combination"
      "AB12345-RUS-16-id-4";"B: Placebo"
      "AB12345-NGA-12-id-264";"B: Placebo"
      "AB12345-USA-11-id-157";"B: Placebo"
      "AB12345-USA-3-id-282";"A: Drug X"
      "AB12345-CHN-14-id-143";"C: Combination"
      "AB12345-CAN-1-id-18";"A: Drug X"
    '
  )
  alb <- read.table(
    header = TRUE,
    sep = ";",
    stringsAsFactors = FALSE,
    strip.white = TRUE,
    text = '
    "USUBJID";"ARM";"ANRIND";"BNRIND";"AVAL";"BASE";"AVISIT";"PARAMCD"
    "AB12345-BRA-1-id-105";"A: Drug X";"NORMAL";"NORMAL";32.6665524521703;55.356705471316;"WEEK 2 DAY 15";"ALT"
    "AB12345-BRA-1-id-105";"A: Drug X";"NORMAL";"NORMAL";41.7819668169786;55.356705471316;"WEEK 4 DAY 29";"ALT"
    "AB12345-BRA-1-id-105";"A: Drug X";"NORMAL";"NORMAL";58.5454464941575;57.3423114504851;"WEEK 2 DAY 15";"CRP"
    "AB12345-BRA-1-id-105";"A: Drug X";"HIGH";"NORMAL";50.1958370424952;57.3423114504851;"WEEK 4 DAY 29";"CRP"
    "AB12345-CAN-1-id-18";"A: Drug X";"NORMAL";"NORMAL";37.0726402321071;45.765200675786;"WEEK 2 DAY 15";"ALT"
    "AB12345-CAN-1-id-18";"A: Drug X";"HIGH";"NORMAL";74.2402814373015;45.765200675786;"WEEK 4 DAY 29";"ALT"
    "AB12345-CAN-1-id-18";"A: Drug X";"NORMAL";"NORMAL";51.5956434600758;42.8430339139095;"WEEK 2 DAY 15";"CRP"
    "AB12345-CAN-1-id-18";"A: Drug X";"NORMAL";"NORMAL";57.937329965051;42.8430339139095;"WEEK 4 DAY 29";"CRP"
    "AB12345-CHN-14-id-143";"C: Combination";"NORMAL";"NORMAL";44.0041539565271;58.7615019827223;"WEEK 2 DAY 15";"ALT"
    "AB12345-CHN-14-id-143";"C: Combination";"NORMAL";"NORMAL";63.5930284733831;58.7615019827223;"WEEK 4 DAY 29";"ALT"
    "AB12345-CHN-14-id-143";"C: Combination";"NORMAL";"NORMAL";48.6118829405467;42.9123978871911;"WEEK 2 DAY 15";"CRP"
    "AB12345-CHN-14-id-143";"C: Combination";"NORMAL";"NORMAL";51.9425413013236;42.9123978871911;"WEEK 4 DAY 29";"CRP"
    "AB12345-JPN-3-id-109";"B: Placebo";"HIGH";"NORMAL";28.4375627160164;44.8927628241263;"WEEK 2 DAY 15";"ALT"
    "AB12345-JPN-3-id-109";"B: Placebo";"NORMAL";"NORMAL";42.5211179739778;44.8927628241263;"WEEK 4 DAY 29";"ALT"
    "AB12345-JPN-3-id-109";"B: Placebo";"HIGH";"NORMAL";53.474059724986;36.6433568028555;"WEEK 2 DAY 15";"CRP"
    "AB12345-JPN-3-id-109";"B: Placebo";"NORMAL";"NORMAL";51.0605916296897;36.6433568028555;"WEEK 4 DAY 29";"CRP"
    "AB12345-NGA-12-id-264";"B: Placebo";"NORMAL";"NORMAL";71.0730509300219;49.3633183719403;"WEEK 2 DAY 15";"ALT"
    "AB12345-NGA-12-id-264";"B: Placebo";"NORMAL";"NORMAL";59.9071470114137;49.3633183719403;"WEEK 4 DAY 29";"ALT"
    "AB12345-NGA-12-id-264";"B: Placebo";"HIGH";"NORMAL";50.8793043682198;46.7737080467617;"WEEK 2 DAY 15";"CRP"
    "AB12345-NGA-12-id-264";"B: Placebo";"LOW";"NORMAL";41.6240494253267;46.7737080467617;"WEEK 4 DAY 29";"CRP"
    "AB12345-PAK-1-id-112";"B: Placebo";"NORMAL";"NORMAL";49.0849739581824;39.5506523279266;"WEEK 2 DAY 15";"ALT"
    "AB12345-PAK-1-id-112";"B: Placebo";"NORMAL";"NORMAL";44.6946798986498;39.5506523279266;"WEEK 4 DAY 29";"ALT"
    "AB12345-PAK-1-id-112";"B: Placebo";"NORMAL";"NORMAL";52.6651694234886;56.3281981107019;"WEEK 2 DAY 15";"CRP"
    "AB12345-PAK-1-id-112";"B: Placebo";"HIGH";"NORMAL";51.9986746343666;56.3281981107019;"WEEK 4 DAY 29";"CRP"
    "AB12345-RUS-16-id-4";"B: Placebo";"NORMAL";"NORMAL";36.7321798910633;52.1999271188109;"WEEK 2 DAY 15";"ALT"
    "AB12345-RUS-16-id-4";"B: Placebo";"NORMAL";"NORMAL";41.6977005480585;52.1999271188109;"WEEK 4 DAY 29";"ALT"
    "AB12345-RUS-16-id-4";"B: Placebo";"NORMAL";"HIGH";54.0131832646212;41.9342499346905;"WEEK 2 DAY 15";"CRP"
    "AB12345-RUS-16-id-4";"B: Placebo";"NORMAL";"HIGH";49.9159827073483;41.9342499346905;"WEEK 4 DAY 29";"CRP"
    "AB12345-USA-11-id-100";"C: Combination";"NORMAL";"NORMAL";47.0546090446394;58.7101969004239;"WEEK 2 DAY 15";"ALT"
    "AB12345-USA-11-id-100";"C: Combination";"NORMAL";"NORMAL";45.1646631268346;58.7101969004239;"WEEK 4 DAY 29";"ALT"
    "AB12345-USA-11-id-100";"C: Combination";"NORMAL";"NORMAL";55.3214028747172;51.3944523707207;"WEEK 2 DAY 15";"CRP"
    "AB12345-USA-11-id-100";"C: Combination";"NORMAL";"NORMAL";44.7647020691003;51.3944523707207;"WEEK 4 DAY 29";"CRP"
    "AB12345-USA-11-id-157";"B: Placebo";"NORMAL";"NORMAL";64.8975603545437;57.6057015486742;"WEEK 2 DAY 15";"ALT"
    "AB12345-USA-11-id-157";"B: Placebo";"NORMAL";"NORMAL";41.6168328261839;57.6057015486742;"WEEK 4 DAY 29";"ALT"
    "AB12345-USA-11-id-157";"B: Placebo";"NORMAL";"NORMAL";60.3106676217289;48.353178958272;"WEEK 2 DAY 15";"CRP"
    "AB12345-USA-11-id-157";"B: Placebo";"NORMAL";"NORMAL";60.5607289335823;48.353178958272;"WEEK 4 DAY 29";"CRP"
    "AB12345-USA-3-id-282";"A: Drug X";"NORMAL";"NORMAL";47.9331543841011;50.6578635909217;"WEEK 2 DAY 15";"ALT"
    "AB12345-USA-3-id-282";"A: Drug X";"NORMAL";"NORMAL";42.2639792525317;50.6578635909217;"WEEK 4 DAY 29";"ALT"
    "AB12345-USA-3-id-282";"A: Drug X";"NORMAL";"NORMAL";51.7595855254374;34.3419529888524;"WEEK 2 DAY 15";"CRP"
    "AB12345-USA-3-id-282";"A: Drug X";"NORMAL";"NORMAL";40.8733413053303;34.3419529888524;"WEEK 4 DAY 29";"CRP"
    '
  )
  asl$ARM <- factor(asl$ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination")) # nolint
  alb$ARM <- factor(alb$ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination")) # nolint
  alb$PARAMCD <- factor(alb$PARAMCD) # nolint
  alb$AVISIT <- factor(alb$AVISIT, levels = sort(unique(alb$AVISIT))) # nolint
  tbl_tern <- t_abn_shift(
    rri_ana = alb$ANRIND,
    rri_base = alb$BNRIND,
    value_ana = alb$AVAL,
    value_base = alb$BASE,
    abnormal = c("LOW", "HIGH"),
    id = alb$USUBJID,
    row_by = alb[, c("PARAMCD", "AVISIT")],
    col_by = alb$ARM,
    col_N = table(asl$ARM),
    table_tree = FALSE
  )
  test_header <- rheader(
    rrow("", "", "B: Placebo", "A: Drug X", "C: Combination"),
    rrow("", "Baseline Status", "(N=5)", "(N=3)", "(N=2)")
  )
  test_format <- function(x, output) {
    if (x[1] > 0 & x[2] > 0) {
      paste0(x[1], "/", x[2], " (", round(x[3], 3) * 100, "%)")
    } else if (x[1] == 0) {
      paste0(x[1], "/", x[2])
    }
  }
  tbl_test <- rtable(
    header = test_header,
    rrow("ALT"),
    rrow("WEEK2 DAY 15", indent = 1),
    rrow(
      "LOW",
      "NOT LOW",
      rcell(c(0, 5, 0 / 5), format = test_format),
      rcell(c(0, 3, 0 / 3), format = test_format),
      rcell(c(0, 2, 0 / 2), format = test_format),
      indent = 2
    ),
    rrow(
      "",
      "Total",
      rcell(c(0, 5, 0 / 5), format = test_format),
      rcell(c(0, 3, 0 / 3), format = test_format),
      rcell(c(0, 2, 0 / 2), format = test_format),
      indent = 2
    ),
    rrow(
      "HIGH", "NOT HIGH",
      rcell(c(1, 5, 1 / 5), format = test_format),
      rcell(c(0, 3, 0 / 3), format = test_format),
      rcell(c(0, 2, 0 / 2), format = test_format),
      indent = 2
    ),
    rrow(
      "", "Total",
      rcell(c(1, 5, 1 / 5), format = test_format),
      rcell(c(0, 3, 0 / 3), format = test_format),
      rcell(c(0, 2, 0 / 2), format = test_format),
      indent = 2
    ),
    rrow(),
    rrow("WEEK4 DAY 29", indent = 1),
    rrow(
      "LOW", "NOT LOW",
      rcell(c(0, 5, 0 / 5), format = test_format),
      rcell(c(0, 3, 0 / 3), format = test_format),
      rcell(c(0, 2, 0 / 2), format = test_format),
      indent = 2
    ),
    rrow(
      "", "Total",
      rcell(c(0, 5, 0 / 5), format = test_format),
      rcell(c(0, 3, 0 / 3), format = test_format),
      rcell(c(0, 2, 0 / 2), format = test_format),
      indent = 2
    ),
    rrow(
      "HIGH", "NOT HIGH",
      rcell(c(0, 5, 0 / 5), format = test_format),
      rcell(c(1, 3, 1 / 3), format = test_format),
      rcell(c(0, 2, 0 / 2), format = test_format),
      indent = 2
    ),
    rrow(
      "", "Total",
      rcell(c(0, 5, 0 / 5), format = test_format),
      rcell(c(1, 3, 1 / 3), format = test_format),
      rcell(c(0, 2, 0 / 2), format = test_format),
      indent = 2
    ),
    rrow(),
    rrow("CRP"),
    rrow("WEEK2 DAY 15", indent = 1),
    rrow(
      "LOW", "NOT LOW",
      rcell(c(0, 5, 0 / 5), format = test_format),
      rcell(c(0, 3, 0 / 3), format = test_format),
      rcell(c(0, 2, 0 / 2), format = test_format),
      indent = 2
    ),
    rrow(
      "", "Total",
      rcell(c(0, 5, 0 / 5), format = test_format),
      rcell(c(0, 3, 0 / 3), format = test_format),
      rcell(c(0, 2, 0 / 2), format = test_format),
      indent = 2
    ),
    rrow(
      "HIGH", "NOT HIGH",
      rcell(c(2, 4, 2 / 4), format = test_format),
      rcell(c(0, 3, 0 / 3), format = test_format),
      rcell(c(0, 2, 0 / 2), format = test_format),
      indent = 2
    ),
    rrow(
      "", "HIGH",
      rcell(c(0, 1, 0 / 1), format = test_format),
      rcell(c(0, 0, 0 / 0), format = test_format),
      rcell(c(0, 0, 0 / 0), format = test_format),
      indent = 2
    ),
    rrow(
      "", "Total",
      rcell(c(2, 5, 2 / 5), format = test_format),
      rcell(c(0, 3, 0 / 3), format = test_format),
      rcell(c(0, 2, 0 / 2), format = test_format),
      indent = 2
    ),
    rrow(),
    rrow("WEEK4 DAY 29", indent = 1),
    rrow(
      "LOW", "NOT LOW",
      rcell(c(1, 5, 1 / 5), format = test_format),
      rcell(c(0, 3, 0 / 3), format = test_format),
      rcell(c(0, 2, 0 / 2), format = test_format),
      indent = 2
    ),
    rrow(
      "", "Total",
      rcell(c(1, 5, 1 / 5), format = test_format),
      rcell(c(0, 3, 0 / 3), format = test_format),
      rcell(c(0, 2, 0 / 2), format = test_format),
      indent = 2
    ),
    rrow(
      "HIGH", "NOT HIGH",
      rcell(c(1, 4, 1 / 4), format = test_format),
      rcell(c(1, 3, 1 / 3), format = test_format),
      rcell(c(0, 2, 0 / 2), format = test_format),
      indent = 2
    ),
    rrow(
      "", "HIGH",
      rcell(c(0, 1, 0 / 1), format = test_format),
      rcell(c(0, 0, 0 / 0), format = test_format),
      rcell(c(0, 0, 0 / 0), format = test_format),
      indent = 2
    ),
    rrow(
      "", "Total",
      rcell(c(1, 5, 1 / 5), format = test_format),
      rcell(c(1, 3, 1 / 3), format = test_format),
      rcell(c(0, 2, 0 / 2), format = test_format),
      indent = 2
    )
  )
  comp <- compare_rtables(tbl_tern, tbl_test, comp.attr = FALSE)

  expect_true(all(comp == "."), "t_abn_shift does not provide the expected results")


})
