context("test t_tte")

library(dplyr)

test_that("Time to event table", {

  asl <- read.table(
    header = TRUE,
    sep = ";",
    stringsAsFactors = FALSE,
    text = '
      "USUBJID";"ARM"
      "AB12345-GBR-1-id-63";"A: Drug X"
      "AB12345-CHN-4-id-68";"B: Placebo"
      "AB12345-CHN-11-id-26";"A: Drug X"
      "AB12345-CHN-14-id-40";"C: Combination"
      "AB12345-CHN-1-id-2";"C: Combination"
      "AB12345-CHN-2-id-10";"B: Placebo"
      "AB12345-RUS-14-id-95";"C: Combination"
      "AB12345-CHN-1-id-4";"A: Drug X"
      "AB12345-BRA-15-id-75";"C: Combination"
      "AB12345-CHN-11-id-66";"C: Combination"
      "AB12345-NGA-8-id-7";"B: Placebo"
      "AB12345-JPN-6-id-32";"A: Drug X"
      "AB12345-NGA-1-id-79";"B: Placebo"
      "AB12345-USA-1-id-21";"A: Drug X"
      "AB12345-CHN-11-id-13";"A: Drug X"
      "AB12345-CHN-12-id-98";"A: Drug X"
      "AB12345-CHN-11-id-88";"A: Drug X"
      "AB12345-CHN-11-id-65";"B: Placebo"
      "AB12345-CHN-11-id-99";"B: Placebo"
      "AB12345-RUS-11-id-42";"B: Placebo"
      "AB12345-CHN-15-id-56";"A: Drug X"
      "AB12345-USA-12-id-55";"A: Drug X"
      "AB12345-CHN-1-id-81";"A: Drug X"
      "AB12345-PAK-9-id-27";"B: Placebo"
      "AB12345-CHN-4-id-46";"B: Placebo"
      "AB12345-CHN-4-id-33";"C: Combination"
      "AB12345-CHN-13-id-8";"A: Drug X"
      "AB12345-NGA-16-id-44";"C: Combination"
      "AB12345-PAK-18-id-15";"B: Placebo"
      "AB12345-PAK-1-id-22";"B: Placebo"
      "AB12345-JPN-1-id-34";"A: Drug X"
      "AB12345-RUS-9-id-91";"A: Drug X"
      "AB12345-CHN-1-id-25";"A: Drug X"
      "AB12345-PAK-15-id-100";"B: Placebo"
      "AB12345-CHN-11-id-30";"A: Drug X"
      "AB12345-USA-2-id-29";"C: Combination"
      "AB12345-PAK-14-id-78";"A: Drug X"
      "AB12345-CHN-2-id-51";"B: Placebo"
      "AB12345-CHN-11-id-14";"C: Combination"
      "AB12345-CHN-1-id-37";"C: Combination"
      "AB12345-BRA-11-id-64";"C: Combination"
      "AB12345-CHN-13-id-6";"A: Drug X"
      "AB12345-CHN-3-id-69";"A: Drug X"
      "AB12345-BRA-12-id-35";"B: Placebo"
      "AB12345-BRA-11-id-16";"A: Drug X"
      "AB12345-RUS-11-id-48";"A: Drug X"
      "AB12345-CHN-11-id-54";"C: Combination"
      "AB12345-CHN-1-id-72";"C: Combination"
      "AB12345-USA-12-id-73";"B: Placebo"
      "AB12345-USA-11-id-97";"A: Drug X"
      "AB12345-PAK-5-id-90";"C: Combination"
      "AB12345-USA-1-id-76";"A: Drug X"
      "AB12345-GBR-13-id-84";"C: Combination"
      "AB12345-USA-11-id-45";"A: Drug X"
      "AB12345-CAN-1-id-39";"A: Drug X"
      "AB12345-CHN-1-id-89";"B: Placebo"
      "AB12345-BRA-11-id-38";"C: Combination"
      "AB12345-USA-5-id-11";"B: Placebo"
      "AB12345-CHN-15-id-61";"B: Placebo"
      "AB12345-CHN-1-id-1";"C: Combination"
      "AB12345-PAK-17-id-47";"A: Drug X"
      "AB12345-CHN-11-id-85";"A: Drug X"
      "AB12345-BRA-5-id-70";"A: Drug X"
      "AB12345-RUS-15-id-12";"A: Drug X"
      "AB12345-NGA-18-id-77";"A: Drug X"
      "AB12345-CHN-12-id-80";"B: Placebo"
      "AB12345-CHN-11-id-67";"A: Drug X"
      "AB12345-CHN-11-id-50";"A: Drug X"
      "AB12345-RUS-17-id-28";"B: Placebo"
      "AB12345-CHN-13-id-60";"A: Drug X"
      "AB12345-CHN-7-id-52";"C: Combination"
      "AB12345-CHN-1-id-96";"C: Combination"
      "AB12345-GBR-5-id-74";"C: Combination"
      "AB12345-USA-11-id-9";"C: Combination"
      "AB12345-CHN-4-id-18";"B: Placebo"
      "AB12345-USA-11-id-59";"B: Placebo"
      "AB12345-CHN-11-id-93";"B: Placebo"
      "AB12345-RUS-11-id-36";"B: Placebo"
      "AB12345-USA-1-id-86";"B: Placebo"
      "AB12345-BRA-5-id-20";"C: Combination"
      "AB12345-USA-11-id-3";"B: Placebo"
      "AB12345-USA-4-id-82";"A: Drug X"
      "AB12345-CHN-12-id-5";"A: Drug X"
      "AB12345-CHN-5-id-57";"A: Drug X"
      "AB12345-CHN-1-id-71";"B: Placebo"
      "AB12345-CHN-7-id-17";"B: Placebo"
      "AB12345-CHN-15-id-58";"C: Combination"
      "AB12345-CHN-1-id-92";"C: Combination"
      "AB12345-CHN-11-id-19";"B: Placebo"
      "AB12345-PAK-11-id-87";"C: Combination"
      "AB12345-CHN-11-id-49";"B: Placebo"
      "AB12345-BRA-12-id-62";"C: Combination"
      "AB12345-CHN-11-id-53";"A: Drug X"
      "AB12345-CHN-11-id-31";"C: Combination"
      "AB12345-PAK-11-id-43";"C: Combination"
      "AB12345-CHN-13-id-41";"B: Placebo"
      "AB12345-CHN-18-id-94";"B: Placebo"
      "AB12345-USA-1-id-23";"A: Drug X"
      "AB12345-JPN-7-id-83";"C: Combination"
      "AB12345-PAK-12-id-24";"C: Combination"'
  )

  anl <- read.table(
    header = TRUE,
    sep = ";",
    stringsAsFactors = FALSE,
    text = '
      "USUBJID";"ARM";"AVAL";"CNSR";"EVNTDESC";"SEX"
      "AB12345-BRA-11-id-16";"A: Drug X";68.3094024624596;0;"Last Tumor Assessment";"F"
      "AB12345-BRA-11-id-64";"C: Combination";2.6772028952837;1;"Last Date Known To Be Alive";"M"
      "AB12345-BRA-12-id-35";"B: Placebo";109.489035675713;0;"Last Tumor Assessment";"U"
      "AB12345-BRA-12-id-62";"C: Combination";106.998369763221;1;"Adverse Event";"F"
      "AB12345-BRA-5-id-70";"A: Drug X";249.138429151444;0;"Last Date Known To Be Alive";"M"
      "AB12345-CAN-1-id-39";"A: Drug X";8.46791854812801;1;"Adverse Event";"M"
      "AB12345-CHN-1-id-2";"C: Combination";22.2099259961396;0;"Adverse Event";"F"
      "AB12345-CHN-1-id-25";"A: Drug X";15.322454944253;0;"Adverse Event";"M"
      "AB12345-CHN-1-id-4";"A: Drug X";98.1531814452181;0;"Death";"M"
      "AB12345-CHN-1-id-71";"B: Placebo";23.8232357427478;0;"Last Date Known To Be Alive";"UNDIFFERENTIATED"
      "AB12345-CHN-11-id-13";"A: Drug X";79.72784243188;1;"Adverse Event";"F"
      "AB12345-CHN-11-id-14";"C: Combination";14.1581074137727;0;"Last Tumor Assessment";"M"
      "AB12345-CHN-11-id-26";"A: Drug X";189.857071472971;0;"Last Date Known To Be Alive";"F"
      "AB12345-CHN-11-id-30";"A: Drug X";36.5989270433784;0;"Last Tumor Assessment";"F"
      "AB12345-CHN-11-id-50";"A: Drug X";22.3420063406229;0;"Adverse Event";"M"
      "AB12345-CHN-11-id-53";"A: Drug X";48.114412240684;0;"Last Date Known To Be Alive";"M"
      "AB12345-CHN-11-id-65";"B: Placebo";17.8156808484346;1;"Adverse Event";"F"
      "AB12345-CHN-12-id-5";"A: Drug X";36.3564044609666;0;"Adverse Event";"F"
      "AB12345-CHN-13-id-41";"B: Placebo";14.5256093237549;0;"Last Tumor Assessment";"M"
      "AB12345-CHN-13-id-8";"A: Drug X";17.6291636449832;1;"Last Date Known To Be Alive";"M"
      "AB12345-CHN-15-id-58";"C: Combination";43.1390392941363;0;"Last Tumor Assessment";"M"
      "AB12345-CHN-18-id-94";"B: Placebo";190.565616070273;0;"Death";"F"
      "AB12345-CHN-4-id-33";"C: Combination";74.6619143684335;0;"Adverse Event";"M"
      "AB12345-CHN-4-id-68";"B: Placebo";65.8353789709508;1;"Last Date Known To Be Alive";"F"
      "AB12345-CHN-5-id-57";"A: Drug X";37.4325002729893;1;"Adverse Event";"M"
      "AB12345-GBR-5-id-74";"C: Combination";38.6351487133652;0;"Last Tumor Assessment";"M"
      "AB12345-JPN-1-id-34";"A: Drug X";41.0516650686559;0;"Last Tumor Assessment";"M"
      "AB12345-PAK-1-id-22";"B: Placebo";122.59291562763;0;"Death";"M"
      "AB12345-PAK-11-id-43";"C: Combination";24.236196661368;0;"Death";"F"
      "AB12345-PAK-11-id-87";"C: Combination";157.568879678734;0;"Disease Progression";"M"
      "AB12345-PAK-12-id-24";"C: Combination";85.302336545642;0;"Last Tumor Assessment";"F"
      "AB12345-PAK-15-id-100";"B: Placebo";40.6058651860803;0;"Adverse Event";"F"
      "AB12345-PAK-18-id-15";"B: Placebo";197.177252351286;1;"Adverse Event";"F"
      "AB12345-PAK-5-id-90";"C: Combination";57.9750506323644;1;"Adverse Event";"F"
      "AB12345-RUS-14-id-95";"C: Combination";33.1628113891929;1;"Last Tumor Assessment";"F"
      "AB12345-RUS-17-id-28";"B: Placebo";202.103709048572;0;"Disease Progression";"M"
      "AB12345-USA-1-id-21";"A: Drug X";13.2616448029876;1;"Adverse Event";"M"
      "AB12345-USA-11-id-3";"B: Placebo";138.315447136284;0;"Disease Progression";"F"
      "AB12345-USA-11-id-59";"B: Placebo";21.4286278001964;0;"Adverse Event";"M"
      "AB12345-USA-11-id-9";"C: Combination";51.7576559167177;0;"Death";"M"
      "AB12345-USA-11-id-97";"A: Drug X";33.6216260492802;0;"Last Date Known To Be Alive";"M"
      "AB12345-USA-12-id-55";"A: Drug X";43.6603357642889;1;"Last Tumor Assessment";"F"
      "AB12345-USA-12-id-73";"B: Placebo";368.503150881684;0;"Adverse Event";"M"
      "AB12345-USA-2-id-29";"C: Combination";17.974254693836;1;"Last Tumor Assessment";"M"'
  )

  asl$ARM <- factor(asl$ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination")) # nolint
  anl$ARM <- factor(anl$ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination")) # nolint

  #nolint start
  tbl <- rtable(
    header = NULL,
    rrowl("Patients with event (%)", list(c(10, 0.7692), c(11, 0.6471), c(9, 0.6429)), format = "xx (xx.xx%)"),
    rrow("Earliest Contributing Event", indent = 1),
    rrow("Adverse Event", rcell(3), rcell(3), rcell(2), indent = 2),
    rrow("Death", rcell(2), rcell(1), rcell(2), indent = 2),
    rrow("Disease Progression", rcell(2), rcell(0), rcell(1), indent = 2),
    rrow("Last Date Known To Be Alive", rcell(1), rcell(4), rcell(0), indent = 2),
    rrow("Last Tumor Assessment", rcell(2), rcell(3), rcell(4), indent = 2),
    rrowl("Patients without event (%)", list(c(3, 0.2308), c(6, 0.3529), c(5, 0.3571)), format = "xx (xx.xx%)"),
    rrow(""),
    rrow("Time to Event (months)"),
    rrow("Median", rcell(122.59), rcell(48.11), rcell(51.76), indent = 1),
    rrowl("95% CI", list(c(40.6, 202.1), c(36.4, 189.9), c(38.6, NA)), format = "(xx.x, xx.x)", indent = 2),
    rrowl("25% and 75%-ile", list(c(40.6, 202.1), c(36.4, 189.9), c(38.6, 85.3)), format = "xx.x, xx.x", indent = 1),
    rrowl("Range (censored)", list(c(17.8, 197.2), c(8.5, 79.7), c(2.7, 107)), format = "xx.x to xx.x", indent = 1),
    rrowl("Range (event)", list(c(14.5, 368.5), c(15.3, 249.1), c(14.2, 157.6)), format = "xx.x to xx.x", indent = 1),
    rrow(""),
    rrow("Unstratified Analysis"),
    rrowl("p-value (Log-rank)", c(list(NULL), 0.2693, 0.123), format = "xx.xxxx", indent = 1),
    rrow(""),
    rrowl("Hazard Ratio", c(list(NULL), 1.6655, 2.2421), format = "xx.xxxx", indent = 1),
    rrowl("95% CI", c(list(NULL), list(c(0.6684, 4.1505)), list(c(0.7868, 6.3897))), format = "(xx.xxxx, xx.xxxx)", indent = 2),
    rrow(""),
    rrow("Stratified Analysis"),
    rrowl("p-value (Log-rank)", c(list(NULL), 0.1998, 0.1092), format = "xx.xxxx", indent = 1),
    rrow(""),
    rrowl("Hazard Ratio", c(list(NULL), 1.9508, 2.5961), format = "xx.xxxx", indent = 1),
    rrowl("95% CI", c(list(NULL), list(c(0.692, 5.4993)), list(c(0.7774, 8.6698))),
          format = "(xx.xxxx, xx.xxxx)", indent = 2)
  )
  # nolint end

  header(tbl) <- rheader(
    rrow("", "B: Placebo", "A: Drug X", "C: Combination"),
    rrow("", "(N=32)", "(N=38)", "(N=30)"))

  tbl_tern <- t_tte(
    formula = Surv(AVAL, !CNSR) ~ arm(ARM) + strata(SEX),
    data = anl,
    col_N = table(asl$ARM),
    event_descr = factor(anl$EVNTDESC)
  )

  comp <- compare_rtables(tbl_tern, tbl, comp.attr = FALSE)
  expect_true(all(comp == "."), "t_tte does not provide the same results as SAS")

})
