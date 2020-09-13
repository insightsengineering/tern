context("test t_events_patyear")

library(dplyr)

test_that("Event by adjusted patient-years table", {
  asl <- read.table(
    header = TRUE,
    sep = ";",
    stringsAsFactors = FALSE,
    text = '
      "USUBJID";"ARM"
      "AB12345-CHN-15-id-245";"C: Combination"
      "AB12345-RUS-13-id-70";"A: Drug X"
      "AB12345-USA-1-id-242";"C: Combination"
      "AB12345-CHN-7-id-126";"B: Placebo"
      "AB12345-USA-17-id-142";"A: Drug X"
      "AB12345-NGA-1-id-388";"A: Drug X"
      "AB12345-CHN-11-id-38";"B: Placebo"
      "AB12345-BRA-5-id-234";"C: Combination"
      "AB12345-CHN-1-id-329";"A: Drug X"
      "AB12345-CHN-4-id-310";"B: Placebo"
      "AB12345-CHN-5-id-338";"B: Placebo"
      "AB12345-USA-1-id-295";"C: Combination"
      "AB12345-NGA-1-id-325";"B: Placebo"
      "AB12345-PAK-12-id-328";"A: Drug X"
      "AB12345-NGA-17-id-72";"A: Drug X"
      "AB12345-CHN-1-id-376";"C: Combination"
      "AB12345-BRA-1-id-265";"C: Combination"
      "AB12345-CHN-13-id-19";"B: Placebo"
      "AB12345-CHN-15-id-399";"A: Drug X"
      "AB12345-JPN-3-id-109";"B: Placebo"
      "AB12345-USA-11-id-339";"A: Drug X"
      "AB12345-CHN-1-id-53";"A: Drug X"
      "AB12345-PAK-1-id-169";"B: Placebo"
      "AB12345-CHN-11-id-27";"B: Placebo"
      "AB12345-CHN-6-id-385";"B: Placebo"
      "AB12345-PAK-1-id-79";"A: Drug X"
      "AB12345-CHN-1-id-287";"A: Drug X"
      "AB12345-CHN-1-id-346";"B: Placebo"
      "AB12345-CHN-1-id-288";"B: Placebo"
      "AB12345-PAK-11-id-187";"A: Drug X"
      "AB12345-CHN-11-id-90";"B: Placebo"
      "AB12345-CHN-3-id-303";"C: Combination"
      "AB12345-CHN-2-id-149";"B: Placebo"
      "AB12345-PAK-2-id-213";"A: Drug X"
      "AB12345-CHN-1-id-74";"C: Combination"
      "AB12345-USA-15-id-372";"C: Combination"
      "AB12345-USA-12-id-226";"B: Placebo"
      "AB12345-CHN-11-id-354";"A: Drug X"
      "AB12345-CHN-11-id-132";"A: Drug X"
      "AB12345-CHN-12-id-258";"A: Drug X"
      "AB12345-NGA-2-id-353";"A: Drug X"
      "AB12345-RUS-4-id-150";"B: Placebo"
      "AB12345-CHN-11-id-75";"C: Combination"
      "AB12345-PAK-13-id-251";"A: Drug X"
      "AB12345-CHN-14-id-375";"C: Combination"
      "AB12345-CHN-11-id-298";"C: Combination"
      "AB12345-USA-6-id-151";"B: Placebo"
      "AB12345-RUS-2-id-176";"B: Placebo"
      "AB12345-CHN-4-id-335";"A: Drug X"
      "AB12345-CHN-2-id-342";"C: Combination"
      "AB12345-CHN-10-id-380";"B: Placebo"
      "AB12345-GBR-11-id-340";"B: Placebo"
      "AB12345-CHN-3-id-155";"C: Combination"
      "AB12345-CHN-1-id-197";"B: Placebo"
      "AB12345-USA-11-id-157";"B: Placebo"
      "AB12345-CHN-1-id-26";"A: Drug X"
      "AB12345-CHN-11-id-244";"C: Combination"
      "AB12345-BRA-1-id-141";"C: Combination"
      "AB12345-CHN-1-id-208";"C: Combination"
      "AB12345-CHN-3-id-271";"A: Drug X"
      "AB12345-USA-12-id-152";"B: Placebo"
      "AB12345-BRA-11-id-345";"A: Drug X"
      "AB12345-JPN-11-id-373";"C: Combination"
      "AB12345-BRA-13-id-177";"A: Drug X"
      "AB12345-CHN-2-id-284";"A: Drug X"
      "AB12345-USA-1-id-254";"C: Combination"
      "AB12345-CHN-1-id-347";"C: Combination"
      "AB12345-BRA-1-id-134";"A: Drug X"
      "AB12345-USA-11-id-224";"A: Drug X"
      "AB12345-JPN-1-id-21";"B: Placebo"
      "AB12345-CHN-1-id-62";"A: Drug X"
      "AB12345-CHN-11-id-358";"B: Placebo"
      "AB12345-NGA-5-id-99";"C: Combination"
      "AB12345-CHN-5-id-231";"B: Placebo"'
  )
  aette <- read.table(
    header = TRUE,
    sep = ";",
    stringsAsFactors = FALSE,
    text = '
      "USUBJID";"ARM";"AVAL";"CNSR"
      "AB12345-BRA-1-id-134";"A: Drug X";0.12153657881223;1
      "AB12345-BRA-1-id-141";"C: Combination";0.17006060715084;0
      "AB12345-BRA-1-id-265";"C: Combination";0.16374750804827;0
      "AB12345-BRA-11-id-345";"A: Drug X";0.000193170759467374;0
      "AB12345-BRA-13-id-177";"A: Drug X";0.00813739082325329;0
      "AB12345-BRA-5-id-234";"C: Combination";0.295211510600401;0
      "AB12345-CHN-1-id-197";"B: Placebo";0.126692171933347;0
      "AB12345-CHN-1-id-208";"C: Combination";0.476628708371204;0
      "AB12345-CHN-1-id-26";"A: Drug X";0.0120604296931942;0
      "AB12345-CHN-1-id-287";"A: Drug X";0.170260327061012;0
      "AB12345-CHN-1-id-288";"B: Placebo";0.151756323216143;0
      "AB12345-CHN-1-id-329";"A: Drug X";0.036851403615227;0
      "AB12345-CHN-1-id-346";"B: Placebo";0.330390987778167;0
      "AB12345-CHN-1-id-347";"C: Combination";1.08164812468314;1
      "AB12345-CHN-1-id-376";"C: Combination";0.000640198634268077;1
      "AB12345-CHN-1-id-53";"A: Drug X";0.36458818736054;1
      "AB12345-CHN-1-id-62";"A: Drug X";0.440157406283964;0
      "AB12345-CHN-1-id-74";"C: Combination";0.321429768368355;0
      "AB12345-CHN-10-id-380";"B: Placebo";1.02086929547918;1
      "AB12345-CHN-11-id-132";"A: Drug X";0.145836845907261;0
      "AB12345-CHN-11-id-244";"C: Combination";0.149740005475003;0
      "AB12345-CHN-11-id-27";"B: Placebo";0.325075967109835;0
      "AB12345-CHN-11-id-298";"C: Combination";0.117710797105435;0
      "AB12345-CHN-11-id-354";"A: Drug X";0.227623515377726;0
      "AB12345-CHN-11-id-358";"B: Placebo";0.0285250071658001;0
      "AB12345-CHN-11-id-38";"B: Placebo";0.32479145001103;0
      "AB12345-CHN-11-id-75";"C: Combination";0.160842853685698;0
      "AB12345-CHN-11-id-90";"B: Placebo";0.386651664500534;0
      "AB12345-CHN-12-id-258";"A: Drug X";0.113585347553217;1
      "AB12345-CHN-13-id-19";"B: Placebo";1.12158295212709;0
      "AB12345-CHN-14-id-375";"C: Combination";0.177892791960961;0
      "AB12345-CHN-15-id-245";"C: Combination";0.102099048421246;1
      "AB12345-CHN-15-id-399";"A: Drug X";0.207177760818675;0
      "AB12345-CHN-2-id-149";"B: Placebo";0.0234432862283077;1
      "AB12345-CHN-2-id-284";"A: Drug X";0.115060097627816;0
      "AB12345-CHN-2-id-342";"C: Combination";0.0433922738020425;0
      "AB12345-CHN-3-id-155";"C: Combination";0.0313170854775935;1
      "AB12345-CHN-3-id-271";"A: Drug X";0.0641145026625036;1
      "AB12345-CHN-3-id-303";"C: Combination";0.144523438939032;1
      "AB12345-CHN-4-id-310";"B: Placebo";0.526610523704474;0
      "AB12345-CHN-4-id-335";"A: Drug X";0.0305944441484312;0
      "AB12345-CHN-5-id-231";"B: Placebo";0.540617734048408;0
      "AB12345-CHN-5-id-338";"B: Placebo";0.138291309496501;0
      "AB12345-CHN-6-id-385";"B: Placebo";0.204149538609019;0
      "AB12345-CHN-7-id-126";"B: Placebo";0.593279773239223;0
      "AB12345-GBR-11-id-340";"B: Placebo";0.0558427172285249;0
      "AB12345-JPN-1-id-21";"B: Placebo";0.941608637056162;0
      "AB12345-JPN-11-id-373";"C: Combination";0.176598331759796;0
      "AB12345-JPN-3-id-109";"B: Placebo";0.0716350045859404;1
      "AB12345-NGA-1-id-325";"B: Placebo";0.0698633993033021;0
      "AB12345-NGA-1-id-388";"A: Drug X";0.128206887590322;0
      "AB12345-NGA-17-id-72";"A: Drug X";0.18614007236917;0
      "AB12345-NGA-2-id-353";"A: Drug X";0.0211816900556463;1
      "AB12345-NGA-5-id-99";"C: Combination";0.404122524722994;0
      "AB12345-PAK-1-id-169";"B: Placebo";0.282788981686712;0
      "AB12345-PAK-1-id-79";"A: Drug X";0.0376968542658821;0
      "AB12345-PAK-11-id-187";"A: Drug X";0.491244790870771;1
      "AB12345-PAK-12-id-328";"A: Drug X";0.0353237358956159;1
      "AB12345-PAK-13-id-251";"A: Drug X";0.00286209254865366;1
      "AB12345-PAK-2-id-213";"A: Drug X";0.367355034155071;0
      "AB12345-RUS-13-id-70";"A: Drug X";0.127628227564507;0
      "AB12345-RUS-2-id-176";"B: Placebo";0.297367851978458;0
      "AB12345-RUS-4-id-150";"B: Placebo";0.645346638140258;0
      "AB12345-USA-1-id-242";"C: Combination";0.0356754691766511;1
      "AB12345-USA-1-id-254";"C: Combination";0.00362538270793284;0
      "AB12345-USA-1-id-295";"C: Combination";0.197335745696367;0
      "AB12345-USA-11-id-157";"B: Placebo";0.122863854415119;0
      "AB12345-USA-11-id-224";"A: Drug X";0.0384198016984915;0
      "AB12345-USA-11-id-339";"A: Drug X";0.0241518833513214;1
      "AB12345-USA-12-id-152";"B: Placebo";0.215719104015855;0
      "AB12345-USA-12-id-226";"B: Placebo";1.78582434475392;0
      "AB12345-USA-15-id-372";"C: Combination";0.208456683170332;0
      "AB12345-USA-17-id-142";"A: Drug X";0.0846712856417557;0
      "AB12345-USA-6-id-151";"B: Placebo";0.404277671306944;0 '
  )
  asl$ARM <- factor(asl$ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination")) # nolint
  aette$ARM <- factor(aette$ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination")) # nolint
  tbl_tern <- t_events_patyear(
    events = !(aette$CNSR),
    time_in_years = aette$AVAL,
    col_by = aette$ARM,
    col_N = table(asl$ARM),
    total = "Total Patients",
    conf_level = 0.95,
    conf_method = "byar",
    lambda = 100,
    event_type = c("AE" = "adverse event")
  )
  tbl <- rtable(
    header = rheader(
      rrowl(NULL, "B: Placebo", "A: Drug X", "C: Combination", "Total Patients"),
      rrowl(NULL, "(N=26)", "(N=27)", "(N=21)", "(N=74)")),
    rrow("Total patient-years at risk", 10.7, 3.6, 4.5, 18.8),
    rrow("Number of adverse events observed", 23, 18, 15, 56),
    rrow("AE rate per 100 patient-years", 214.24, 499.63, 336.12, 297.85),
    rrowl("95% CI", list(c(139.47, 315.87), c(306.65, 772.64), c(196.37, 540.42), c(227.29, 383.79)))
  )
  comp <- compare_rtables(tbl_tern, tbl, comp.attr = FALSE)
  expect_true(all(comp == "."), "t_tte does not provide the same results as SAS")

})
