library(dplyr)

get_anl <- function() {
  read.table(
    header = TRUE,
    sep = ";",
    stringsAsFactors = FALSE,
    text = '
      "USUBJID";"ARM";"STRATA1";"BMRKR2";"AVISIT";"BASE";"AVAL"
      "AB12345-BRA-1-id-105";"A: Drug X";"B";"MEDIUM";"WEEK 1 DAY 8";"67.88985864";"50.43875924"
      "AB12345-BRA-1-id-105";"A: Drug X";"B";"MEDIUM";"WEEK 2 DAY 15";"67.88985864";"61.50291623"
      "AB12345-BRA-1-id-105";"A: Drug X";"B";"MEDIUM";"WEEK 3 DAY 22";"67.88985864";"50.68271806"
      "AB12345-BRA-1-id-105";"A: Drug X";"B";"MEDIUM";"WEEK 4 DAY 29";"67.88985864";"43.149968"
      "AB12345-BRA-1-id-105";"A: Drug X";"B";"MEDIUM";"WEEK 5 DAY 36";"67.88985864";"55.5516058"
      "AB12345-BRA-1-id-134";"A: Drug X";"B";"LOW";"WEEK 1 DAY 8";"60.02499749";"55.08326531"
      "AB12345-BRA-1-id-134";"A: Drug X";"B";"LOW";"WEEK 2 DAY 15";"60.02499749";"43.87093725"
      "AB12345-BRA-1-id-134";"A: Drug X";"B";"LOW";"WEEK 3 DAY 22";"60.02499749";"61.68408546"
      "AB12345-BRA-1-id-134";"A: Drug X";"B";"LOW";"WEEK 4 DAY 29";"60.02499749";"51.24487629"
      "AB12345-BRA-1-id-134";"A: Drug X";"B";"LOW";"WEEK 5 DAY 36";"60.02499749";"47.51623139"
      "AB12345-BRA-1-id-141";"C: Combination";"B";"HIGH";"WEEK 1 DAY 8";"43.29633267";"42.46491872"
      "AB12345-BRA-1-id-141";"C: Combination";"B";"HIGH";"WEEK 2 DAY 15";"43.29633267";"50.88502511"
      "AB12345-BRA-1-id-141";"C: Combination";"B";"HIGH";"WEEK 3 DAY 22";"43.29633267";"51.65439907"
      "AB12345-BRA-1-id-141";"C: Combination";"B";"HIGH";"WEEK 4 DAY 29";"43.29633267";"63.62737932"
      "AB12345-BRA-1-id-141";"C: Combination";"B";"HIGH";"WEEK 5 DAY 36";"43.29633267";"68.82593785"
      "AB12345-BRA-1-id-236";"B: Placebo";"A";"HIGH";"WEEK 1 DAY 8";"56.39948748";"58.12445932"
      "AB12345-BRA-1-id-236";"B: Placebo";"A";"HIGH";"WEEK 2 DAY 15";"56.39948748";"45.74268906"
      "AB12345-BRA-1-id-236";"B: Placebo";"A";"HIGH";"WEEK 3 DAY 22";"56.39948748";"62.35864011"
      "AB12345-BRA-1-id-236";"B: Placebo";"A";"HIGH";"WEEK 4 DAY 29";"56.39948748";"54.26595123"
      "AB12345-BRA-1-id-236";"B: Placebo";"A";"HIGH";"WEEK 5 DAY 36";"56.39948748";"49.47182985"
      "AB12345-BRA-1-id-265";"C: Combination";"A";"MEDIUM";"WEEK 1 DAY 8";"62.81857503";"42.59818754"
      "AB12345-BRA-1-id-265";"C: Combination";"A";"MEDIUM";"WEEK 2 DAY 15";"62.81857503";"53.01259956"
      "AB12345-BRA-1-id-265";"C: Combination";"A";"MEDIUM";"WEEK 3 DAY 22";"62.81857503";"59.02291642"
      "AB12345-BRA-1-id-265";"C: Combination";"A";"MEDIUM";"WEEK 4 DAY 29";"62.81857503";"45.08336743"
      "AB12345-BRA-1-id-265";"C: Combination";"A";"MEDIUM";"WEEK 5 DAY 36";"62.81857503";"59.64104106"
      "AB12345-BRA-1-id-42";"A: Drug X";"B";"MEDIUM";"WEEK 1 DAY 8";"48.2728554";"52.92784753"
      "AB12345-BRA-1-id-42";"A: Drug X";"B";"MEDIUM";"WEEK 2 DAY 15";"48.2728554";"49.84078804"
      "AB12345-BRA-1-id-42";"A: Drug X";"B";"MEDIUM";"WEEK 3 DAY 22";"48.2728554";"49.07336306"
      "AB12345-BRA-1-id-42";"A: Drug X";"B";"MEDIUM";"WEEK 4 DAY 29";"48.2728554";"70.31214801"
      "AB12345-BRA-1-id-42";"A: Drug X";"B";"MEDIUM";"WEEK 5 DAY 36";"48.2728554";"41.85491921"
      "AB12345-BRA-1-id-65";"B: Placebo";"A";"MEDIUM";"WEEK 1 DAY 8";"53.37767135";"55.48675537"
      "AB12345-BRA-1-id-65";"B: Placebo";"A";"MEDIUM";"WEEK 2 DAY 15";"53.37767135";"58.33272863"
      "AB12345-BRA-1-id-65";"B: Placebo";"A";"MEDIUM";"WEEK 3 DAY 22";"53.37767135";"35.04338482"
      "AB12345-BRA-1-id-65";"B: Placebo";"A";"MEDIUM";"WEEK 4 DAY 29";"53.37767135";"48.20473285"
      "AB12345-BRA-1-id-65";"B: Placebo";"A";"MEDIUM";"WEEK 5 DAY 36";"53.37767135";"48.32984847"
      "AB12345-BRA-1-id-93";"A: Drug X";"A";"LOW";"WEEK 1 DAY 8";"71.71464916";"47.70996987"
      "AB12345-BRA-1-id-93";"A: Drug X";"A";"LOW";"WEEK 2 DAY 15";"71.71464916";"53.07874642"
      "AB12345-BRA-1-id-93";"A: Drug X";"A";"LOW";"WEEK 3 DAY 22";"71.71464916";"51.2307956"
      "AB12345-BRA-1-id-93";"A: Drug X";"A";"LOW";"WEEK 4 DAY 29";"71.71464916";"51.83555366"
      "AB12345-BRA-1-id-93";"A: Drug X";"A";"LOW";"WEEK 5 DAY 36";"71.71464916";"60.62970709"
      "AB12345-BRA-11-id-171";"C: Combination";"B";"HIGH";"WEEK 1 DAY 8";"65.14085313";"46.73739036"
      "AB12345-BRA-11-id-171";"C: Combination";"B";"HIGH";"WEEK 2 DAY 15";"65.14085313";"44.68338469"
      "AB12345-BRA-11-id-171";"C: Combination";"B";"HIGH";"WEEK 3 DAY 22";"65.14085313";"45.85457862"
      "AB12345-BRA-11-id-171";"C: Combination";"B";"HIGH";"WEEK 4 DAY 29";"65.14085313";"53.50815722"
      "AB12345-BRA-11-id-171";"C: Combination";"B";"HIGH";"WEEK 5 DAY 36";"65.14085313";"49.59114575"
      "AB12345-BRA-11-id-217";"A: Drug X";"A";"MEDIUM";"WEEK 1 DAY 8";"61.41400831";"68.98362472"
      "AB12345-BRA-11-id-217";"A: Drug X";"A";"MEDIUM";"WEEK 2 DAY 15";"61.41400831";"49.84940525"
      "AB12345-BRA-11-id-217";"A: Drug X";"A";"MEDIUM";"WEEK 3 DAY 22";"61.41400831";"51.08326489"
      "AB12345-BRA-11-id-217";"A: Drug X";"A";"MEDIUM";"WEEK 4 DAY 29";"61.41400831";"50.15482906"
      "AB12345-BRA-11-id-217";"A: Drug X";"A";"MEDIUM";"WEEK 5 DAY 36";"61.41400831";"52.38921307"
      "AB12345-BRA-11-id-237";"C: Combination";"B";"HIGH";"WEEK 1 DAY 8";"44.12324939";"43.52861587"
      "AB12345-BRA-11-id-237";"C: Combination";"B";"HIGH";"WEEK 2 DAY 15";"44.12324939";"49.55231325"
      "AB12345-BRA-11-id-237";"C: Combination";"B";"HIGH";"WEEK 3 DAY 22";"44.12324939";"68.20788921"
      "AB12345-BRA-11-id-237";"C: Combination";"B";"HIGH";"WEEK 4 DAY 29";"44.12324939";"56.776013"
      "AB12345-BRA-11-id-237";"C: Combination";"B";"HIGH";"WEEK 5 DAY 36";"44.12324939";"59.16521401"
      "AB12345-BRA-11-id-321";"C: Combination";"C";"HIGH";"WEEK 1 DAY 8";"59.58228896";"46.23169784"
      "AB12345-BRA-11-id-321";"C: Combination";"C";"HIGH";"WEEK 2 DAY 15";"59.58228896";"64.57419992"
      "AB12345-BRA-11-id-321";"C: Combination";"C";"HIGH";"WEEK 3 DAY 22";"59.58228896";"52.54657508"
      "AB12345-BRA-11-id-321";"C: Combination";"C";"HIGH";"WEEK 4 DAY 29";"59.58228896";"45.02543605"
      "AB12345-BRA-11-id-321";"C: Combination";"C";"HIGH";"WEEK 5 DAY 36";"59.58228896";"71.87423258"
      "AB12345-BRA-11-id-345";"A: Drug X";"A";"LOW";"WEEK 1 DAY 8";"52.52563791";"57.06466667"
      "AB12345-BRA-11-id-345";"A: Drug X";"A";"LOW";"WEEK 2 DAY 15";"52.52563791";"48.3326902"
      "AB12345-BRA-11-id-345";"A: Drug X";"A";"LOW";"WEEK 3 DAY 22";"52.52563791";"59.23155396"
      "AB12345-BRA-11-id-345";"A: Drug X";"A";"LOW";"WEEK 4 DAY 29";"52.52563791";"44.54233766"
      "AB12345-BRA-11-id-345";"A: Drug X";"A";"LOW";"WEEK 5 DAY 36";"52.52563791";"49.58468904"
      "AB12345-BRA-11-id-397";"A: Drug X";"B";"HIGH";"WEEK 1 DAY 8";"63.55540711";"57.79381787"
      "AB12345-BRA-11-id-397";"A: Drug X";"B";"HIGH";"WEEK 2 DAY 15";"63.55540711";"53.20778727"
      "AB12345-BRA-11-id-397";"A: Drug X";"B";"HIGH";"WEEK 3 DAY 22";"63.55540711";"46.50804131"
      "AB12345-BRA-11-id-397";"A: Drug X";"B";"HIGH";"WEEK 4 DAY 29";"63.55540711";"45.67316412"
      "AB12345-BRA-11-id-397";"A: Drug X";"B";"HIGH";"WEEK 5 DAY 36";"63.55540711";"39.65255343"
      "AB12345-BRA-11-id-50";"A: Drug X";"B";"LOW";"WEEK 1 DAY 8";"52.42786609";"44.08455627"
      "AB12345-BRA-11-id-50";"A: Drug X";"B";"LOW";"WEEK 2 DAY 15";"52.42786609";"52.69746487"
      "AB12345-BRA-11-id-50";"A: Drug X";"B";"LOW";"WEEK 3 DAY 22";"52.42786609";"58.63046275"
      "AB12345-BRA-11-id-50";"A: Drug X";"B";"LOW";"WEEK 4 DAY 29";"52.42786609";"59.66696007"
      "AB12345-BRA-11-id-50";"A: Drug X";"B";"LOW";"WEEK 5 DAY 36";"52.42786609";"48.81776863"
      "AB12345-BRA-11-id-9";"C: Combination";"B";"HIGH";"WEEK 1 DAY 8";"39.74591759";"54.46811519"
      "AB12345-BRA-11-id-9";"C: Combination";"B";"HIGH";"WEEK 2 DAY 15";"39.74591759";"58.56746406"
      "AB12345-BRA-11-id-9";"C: Combination";"B";"HIGH";"WEEK 3 DAY 22";"39.74591759";"62.44449616"
      "AB12345-BRA-11-id-9";"C: Combination";"B";"HIGH";"WEEK 4 DAY 29";"39.74591759";"49.58219322"
      "AB12345-BRA-11-id-9";"C: Combination";"B";"HIGH";"WEEK 5 DAY 36";"39.74591759";"31.86985923"
      "AB12345-BRA-12-id-59";"B: Placebo";"A";"HIGH";"WEEK 1 DAY 8";"38.51253744";"49.78991235"
      "AB12345-BRA-12-id-59";"B: Placebo";"A";"HIGH";"WEEK 2 DAY 15";"38.51253744";"52.15765632"
      "AB12345-BRA-12-id-59";"B: Placebo";"A";"HIGH";"WEEK 3 DAY 22";"38.51253744";"52.77515777"
      "AB12345-BRA-12-id-59";"B: Placebo";"A";"HIGH";"WEEK 4 DAY 29";"38.51253744";"46.59603872"
      "AB12345-BRA-12-id-59";"B: Placebo";"A";"HIGH";"WEEK 5 DAY 36";"38.51253744";"63.84339836"
      "AB12345-BRA-13-id-177";"A: Drug X";"B";"MEDIUM";"WEEK 1 DAY 8";"58.66051115";"58.02330884"
      "AB12345-BRA-13-id-177";"A: Drug X";"B";"MEDIUM";"WEEK 2 DAY 15";"58.66051115";"47.932631"
      "AB12345-BRA-13-id-177";"A: Drug X";"B";"MEDIUM";"WEEK 3 DAY 22";"58.66051115";"54.19017287"
      "AB12345-BRA-13-id-177";"A: Drug X";"B";"MEDIUM";"WEEK 4 DAY 29";"58.66051115";"38.77362987"
      "AB12345-BRA-13-id-177";"A: Drug X";"B";"MEDIUM";"WEEK 5 DAY 36";"58.66051115";"54.54441502"
      "AB12345-BRA-14-id-120";"C: Combination";"C";"HIGH";"WEEK 1 DAY 8";"56.1122029";"45.03807429"
      "AB12345-BRA-14-id-120";"C: Combination";"C";"HIGH";"WEEK 2 DAY 15";"56.1122029";"42.50657506"
      "AB12345-BRA-14-id-120";"C: Combination";"C";"HIGH";"WEEK 3 DAY 22";"56.1122029";"51.33632589"
      "AB12345-BRA-14-id-120";"C: Combination";"C";"HIGH";"WEEK 4 DAY 29";"56.1122029";"51.46558662"
      "AB12345-BRA-14-id-120";"C: Combination";"C";"HIGH";"WEEK 5 DAY 36";"56.1122029";"52.5780998"
      "AB12345-BRA-14-id-23";"A: Drug X";"B";"MEDIUM";"WEEK 1 DAY 8";"53.90549966";"36.07474287"
      "AB12345-BRA-14-id-23";"A: Drug X";"B";"MEDIUM";"WEEK 2 DAY 15";"53.90549966";"51.65371614"
      "AB12345-BRA-14-id-23";"A: Drug X";"B";"MEDIUM";"WEEK 3 DAY 22";"53.90549966";"54.32860708"
      "AB12345-BRA-14-id-23";"A: Drug X";"B";"MEDIUM";"WEEK 4 DAY 29";"53.90549966";"43.41184086"
      "AB12345-BRA-14-id-23";"A: Drug X";"B";"MEDIUM";"WEEK 5 DAY 36";"53.90549966";"43.37302674"
      "AB12345-BRA-15-id-36";"A: Drug X";"B";"HIGH";"WEEK 1 DAY 8";"48.42133879";"50.57301306"
      "AB12345-BRA-15-id-36";"A: Drug X";"B";"HIGH";"WEEK 2 DAY 15";"48.42133879";"41.05193138"
      "AB12345-BRA-15-id-36";"A: Drug X";"B";"HIGH";"WEEK 3 DAY 22";"48.42133879";"59.90127632"
      "AB12345-BRA-15-id-36";"A: Drug X";"B";"HIGH";"WEEK 4 DAY 29";"48.42133879";"51.72452253"
      "AB12345-BRA-15-id-36";"A: Drug X";"B";"HIGH";"WEEK 5 DAY 36";"48.42133879";"48.20972062"
      "AB12345-BRA-2-id-101";"B: Placebo";"A";"MEDIUM";"WEEK 1 DAY 8";"57.91006483";"54.97906667"
      "AB12345-BRA-2-id-101";"B: Placebo";"A";"MEDIUM";"WEEK 2 DAY 15";"57.91006483";"34.96128911"
      "AB12345-BRA-2-id-101";"B: Placebo";"A";"MEDIUM";"WEEK 3 DAY 22";"57.91006483";"50.59813141"
      "AB12345-BRA-2-id-101";"B: Placebo";"A";"MEDIUM";"WEEK 4 DAY 29";"57.91006483";"39.85104338"
      "AB12345-BRA-2-id-101";"B: Placebo";"A";"MEDIUM";"WEEK 5 DAY 36";"57.91006483";"47.74097512"
      "AB12345-BRA-2-id-296";"A: Drug X";"C";"HIGH";"WEEK 1 DAY 8";"57.53687622";"38.7019598"
      "AB12345-BRA-2-id-296";"A: Drug X";"C";"HIGH";"WEEK 2 DAY 15";"57.53687622";"44.97015184"
      "AB12345-BRA-2-id-296";"A: Drug X";"C";"HIGH";"WEEK 3 DAY 22";"57.53687622";"57.65983342"
      "AB12345-BRA-2-id-296";"A: Drug X";"C";"HIGH";"WEEK 4 DAY 29";"57.53687622";"52.13464322"
      "AB12345-BRA-2-id-296";"A: Drug X";"C";"HIGH";"WEEK 5 DAY 36";"57.53687622";"45.01484368"
      "AB12345-BRA-3-id-13";"B: Placebo";"A";"LOW";"WEEK 1 DAY 8";"38.61659204";"63.66251828"
      "AB12345-BRA-3-id-13";"B: Placebo";"A";"LOW";"WEEK 2 DAY 15";"38.61659204";"46.94717406"
      "AB12345-BRA-3-id-13";"B: Placebo";"A";"LOW";"WEEK 3 DAY 22";"38.61659204";"47.51358487"
      "AB12345-BRA-3-id-13";"B: Placebo";"A";"LOW";"WEEK 4 DAY 29";"38.61659204";"62.12765014"
      "AB12345-BRA-3-id-13";"B: Placebo";"A";"LOW";"WEEK 5 DAY 36";"38.61659204";"38.58603568"
      "AB12345-BRA-3-id-8";"B: Placebo";"B";"MEDIUM";"WEEK 1 DAY 8";"50.4380212";"52.74254977"
      "AB12345-BRA-3-id-8";"B: Placebo";"B";"MEDIUM";"WEEK 2 DAY 15";"50.4380212";"51.57955432"
      "AB12345-BRA-3-id-8";"B: Placebo";"B";"MEDIUM";"WEEK 3 DAY 22";"50.4380212";"44.10220552"
      "AB12345-BRA-3-id-8";"B: Placebo";"B";"MEDIUM";"WEEK 4 DAY 29";"50.4380212";"34.78926025"
      "AB12345-BRA-3-id-8";"B: Placebo";"B";"MEDIUM";"WEEK 5 DAY 36";"50.4380212";"42.40548755"
      "AB12345-BRA-4-id-368";"C: Combination";"B";"MEDIUM";"WEEK 1 DAY 8";"64.44871403";"40.15868367"
      "AB12345-BRA-4-id-368";"C: Combination";"B";"MEDIUM";"WEEK 2 DAY 15";"64.44871403";"56.10868726"
      "AB12345-BRA-4-id-368";"C: Combination";"B";"MEDIUM";"WEEK 3 DAY 22";"64.44871403";"38.43982358"
      "AB12345-BRA-4-id-368";"C: Combination";"B";"MEDIUM";"WEEK 4 DAY 29";"64.44871403";"39.85501425"
      "AB12345-BRA-4-id-368";"C: Combination";"B";"MEDIUM";"WEEK 5 DAY 36";"64.44871403";"54.90633766"
      "AB12345-BRA-4-id-383";"B: Placebo";"A";"HIGH";"WEEK 1 DAY 8";"53.54191561";"50.9584111"
      "AB12345-BRA-4-id-383";"B: Placebo";"A";"HIGH";"WEEK 2 DAY 15";"53.54191561";"42.24304843"
      "AB12345-BRA-4-id-383";"B: Placebo";"A";"HIGH";"WEEK 3 DAY 22";"53.54191561";"45.89153143"
      "AB12345-BRA-4-id-383";"B: Placebo";"A";"HIGH";"WEEK 4 DAY 29";"53.54191561";"37.14690379"
      "AB12345-BRA-4-id-383";"B: Placebo";"A";"HIGH";"WEEK 5 DAY 36";"53.54191561";"59.0242451"
      "AB12345-BRA-5-id-234";"C: Combination";"A";"MEDIUM";"WEEK 1 DAY 8";"46.40451953";"62.06415907"
      "AB12345-BRA-5-id-234";"C: Combination";"A";"MEDIUM";"WEEK 2 DAY 15";"46.40451953";"32.31162791"
      "AB12345-BRA-5-id-234";"C: Combination";"A";"MEDIUM";"WEEK 3 DAY 22";"46.40451953";"39.36419827"
      "AB12345-BRA-5-id-234";"C: Combination";"A";"MEDIUM";"WEEK 4 DAY 29";"46.40451953";"52.90701455"
      "AB12345-BRA-5-id-234";"C: Combination";"A";"MEDIUM";"WEEK 5 DAY 36";"46.40451953";"46.28288834"
      "AB12345-BRA-6-id-369";"A: Drug X";"A";"HIGH";"WEEK 1 DAY 8";"50.12889161";"51.79801688"
      "AB12345-BRA-6-id-369";"A: Drug X";"A";"HIGH";"WEEK 2 DAY 15";"50.12889161";"59.38716776"
      "AB12345-BRA-6-id-369";"A: Drug X";"A";"HIGH";"WEEK 3 DAY 22";"50.12889161";"40.9292181"
      "AB12345-BRA-6-id-369";"A: Drug X";"A";"HIGH";"WEEK 4 DAY 29";"50.12889161";"40.3866197"
      "AB12345-BRA-6-id-369";"A: Drug X";"A";"HIGH";"WEEK 5 DAY 36";"50.12889161";"48.07753903"
      "AB12345-BRA-7-id-301";"C: Combination";"B";"LOW";"WEEK 1 DAY 8";"54.62606612";"54.50953768"
      "AB12345-BRA-7-id-301";"C: Combination";"B";"LOW";"WEEK 2 DAY 15";"54.62606612";"43.28439046"
      "AB12345-BRA-7-id-301";"C: Combination";"B";"LOW";"WEEK 3 DAY 22";"54.62606612";"60.32936162"
      "AB12345-BRA-7-id-301";"C: Combination";"B";"LOW";"WEEK 4 DAY 29";"54.62606612";"53.05441096"
      "AB12345-BRA-7-id-301";"C: Combination";"B";"LOW";"WEEK 5 DAY 36";"54.62606612";"61.30877814"
      "AB12345-CAN-1-id-18";"A: Drug X";"A";"MEDIUM";"WEEK 1 DAY 8";"43.08102992";"48.25184065"
      "AB12345-CAN-1-id-18";"A: Drug X";"A";"MEDIUM";"WEEK 2 DAY 15";"43.08102992";"48.92231147"
      "AB12345-CAN-1-id-18";"A: Drug X";"A";"MEDIUM";"WEEK 3 DAY 22";"43.08102992";"39.99679467"
      "AB12345-CAN-1-id-18";"A: Drug X";"A";"MEDIUM";"WEEK 4 DAY 29";"43.08102992";"39.85133709"
      "AB12345-CAN-1-id-18";"A: Drug X";"A";"MEDIUM";"WEEK 5 DAY 36";"43.08102992";"48.33532464"
      "AB12345-CAN-1-id-341";"B: Placebo";"B";"MEDIUM";"WEEK 1 DAY 8";"53.59961608";"53.09282171"
      "AB12345-CAN-1-id-341";"B: Placebo";"B";"MEDIUM";"WEEK 2 DAY 15";"53.59961608";"43.8675434"
      "AB12345-CAN-1-id-341";"B: Placebo";"B";"MEDIUM";"WEEK 3 DAY 22";"53.59961608";"49.56288108"
      "AB12345-CAN-1-id-341";"B: Placebo";"B";"MEDIUM";"WEEK 4 DAY 29";"53.59961608";"58.37644522"
      "AB12345-CAN-1-id-341";"B: Placebo";"B";"MEDIUM";"WEEK 5 DAY 36";"53.59961608";"43.9545255"
      "AB12345-CAN-11-id-139";"A: Drug X";"B";"LOW";"WEEK 1 DAY 8";"46.49299033";"55.44430984"
      "AB12345-CAN-11-id-139";"A: Drug X";"B";"LOW";"WEEK 2 DAY 15";"46.49299033";"45.44546582"
      "AB12345-CAN-11-id-139";"A: Drug X";"B";"LOW";"WEEK 3 DAY 22";"46.49299033";"43.1332461"
      "AB12345-CAN-11-id-139";"A: Drug X";"B";"LOW";"WEEK 4 DAY 29";"46.49299033";"47.74561565"
      "AB12345-CAN-11-id-139";"A: Drug X";"B";"LOW";"WEEK 5 DAY 36";"46.49299033";"62.97058997"
      "AB12345-CAN-11-id-306";"C: Combination";"A";"HIGH";"WEEK 1 DAY 8";"50.62980745";"58.53109508"
      "AB12345-CAN-11-id-306";"C: Combination";"A";"HIGH";"WEEK 2 DAY 15";"50.62980745";"29.72895902"
      "AB12345-CAN-11-id-306";"C: Combination";"A";"HIGH";"WEEK 3 DAY 22";"50.62980745";"52.9082627"
      "AB12345-CAN-11-id-306";"C: Combination";"A";"HIGH";"WEEK 4 DAY 29";"50.62980745";"65.90462976"
      "AB12345-CAN-11-id-306";"C: Combination";"A";"HIGH";"WEEK 5 DAY 36";"50.62980745";"45.86928504"
      "AB12345-CAN-14-id-104";"A: Drug X";"B";"HIGH";"WEEK 1 DAY 8";"37.86075699";"53.42297722"
      "AB12345-CAN-14-id-104";"A: Drug X";"B";"HIGH";"WEEK 2 DAY 15";"37.86075699";"61.14149868"
      "AB12345-CAN-14-id-104";"A: Drug X";"B";"HIGH";"WEEK 3 DAY 22";"37.86075699";"49.11191688"
      "AB12345-CAN-14-id-104";"A: Drug X";"B";"HIGH";"WEEK 4 DAY 29";"37.86075699";"52.32121689"
      "AB12345-CAN-14-id-104";"A: Drug X";"B";"HIGH";"WEEK 5 DAY 36";"37.86075699";"42.79681252"
      "AB12345-CAN-4-id-324";"C: Combination";"B";"MEDIUM";"WEEK 1 DAY 8";"41.47118369";"39.3171842"
      "AB12345-CAN-4-id-324";"C: Combination";"B";"MEDIUM";"WEEK 2 DAY 15";"41.47118369";"45.80490508"
      "AB12345-CAN-4-id-324";"C: Combination";"B";"MEDIUM";"WEEK 3 DAY 22";"41.47118369";"37.75439439"
      "AB12345-CAN-4-id-324";"C: Combination";"B";"MEDIUM";"WEEK 4 DAY 29";"41.47118369";"52.40049525"
      "AB12345-CAN-4-id-324";"C: Combination";"B";"MEDIUM";"WEEK 5 DAY 36";"41.47118369";"51.04490632"
      "AB12345-CAN-4-id-331";"B: Placebo";"A";"MEDIUM";"WEEK 1 DAY 8";"49.52881869";"37.13936423"
      "AB12345-CAN-4-id-331";"B: Placebo";"A";"MEDIUM";"WEEK 2 DAY 15";"49.52881869";"43.60217652"
      "AB12345-CAN-4-id-331";"B: Placebo";"A";"MEDIUM";"WEEK 3 DAY 22";"49.52881869";"57.25095293"
      "AB12345-CAN-4-id-331";"B: Placebo";"A";"MEDIUM";"WEEK 4 DAY 29";"49.52881869";"52.75023299"
      "AB12345-CAN-4-id-331";"B: Placebo";"A";"MEDIUM";"WEEK 5 DAY 36";"49.52881869";"51.94990354"
      "AB12345-CAN-5-id-121";"C: Combination";"A";"HIGH";"WEEK 1 DAY 8";"56.08540439";"50.64515939"
      "AB12345-CAN-5-id-121";"C: Combination";"A";"HIGH";"WEEK 2 DAY 15";"56.08540439";"58.39085159"
      "AB12345-CAN-5-id-121";"C: Combination";"A";"HIGH";"WEEK 3 DAY 22";"56.08540439";"58.23111566"
      "AB12345-CAN-5-id-121";"C: Combination";"A";"HIGH";"WEEK 4 DAY 29";"56.08540439";"49.91943435"
      "AB12345-CAN-5-id-121";"C: Combination";"A";"HIGH";"WEEK 5 DAY 36";"56.08540439";"47.71210561"
      "AB12345-CHN-1-id-107";"B: Placebo";"B";"HIGH";"WEEK 1 DAY 8";"58.03627562";"40.21813637"
      "AB12345-CHN-1-id-107";"B: Placebo";"B";"HIGH";"WEEK 2 DAY 15";"58.03627562";"46.97678868"
      "AB12345-CHN-1-id-107";"B: Placebo";"B";"HIGH";"WEEK 3 DAY 22";"58.03627562";"45.19739798"
      "AB12345-CHN-1-id-107";"B: Placebo";"B";"HIGH";"WEEK 4 DAY 29";"58.03627562";"60.32665349"
      "AB12345-CHN-1-id-107";"B: Placebo";"B";"HIGH";"WEEK 5 DAY 36";"58.03627562";"45.84002151"
      "AB12345-CHN-1-id-119";"A: Drug X";"C";"MEDIUM";"WEEK 1 DAY 8";"49.25720896";"52.79694364"
      "AB12345-CHN-1-id-119";"A: Drug X";"C";"MEDIUM";"WEEK 2 DAY 15";"49.25720896";"46.97466787"
      "AB12345-CHN-1-id-119";"A: Drug X";"C";"MEDIUM";"WEEK 3 DAY 22";"49.25720896";"44.79746183"
      "AB12345-CHN-1-id-119";"A: Drug X";"C";"MEDIUM";"WEEK 4 DAY 29";"49.25720896";"42.11954487"
      "AB12345-CHN-1-id-119";"A: Drug X";"C";"MEDIUM";"WEEK 5 DAY 36";"49.25720896";"43.27039232"
      "AB12345-CHN-1-id-12";"B: Placebo";"C";"LOW";"WEEK 1 DAY 8";"60.17067693";"48.27665503"
      "AB12345-CHN-1-id-12";"B: Placebo";"C";"LOW";"WEEK 2 DAY 15";"60.17067693";"41.15831196"
      "AB12345-CHN-1-id-12";"B: Placebo";"C";"LOW";"WEEK 3 DAY 22";"60.17067693";"58.12504141"
      "AB12345-CHN-1-id-12";"B: Placebo";"C";"LOW";"WEEK 4 DAY 29";"60.17067693";"52.61436821"
      "AB12345-CHN-1-id-12";"B: Placebo";"C";"LOW";"WEEK 5 DAY 36";"60.17067693";"60.14166533"'
  )
}

get_mmrm <- function() {
  anl <- get_anl() %>%
    mutate(
      ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination")),
      AVISIT = factor(AVISIT)
    )

  mmrm <- fit_mmrm(
    vars = list(
      response = "AVAL",
      visit = "AVISIT",
      arm = "ARM",
      covariates = c("BMRKR2"),
      id = "USUBJID"
    ),
    data = anl,
    cor_struct = "random-quadratic"
  )
}


get_mmrm_no_arm <- function() {
  anl <- get_anl() %>%
    mutate(
      ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination")),
      AVISIT = factor(AVISIT)
    )

  mmrm <- fit_mmrm(
    vars = list(
      response = "AVAL",
      visit = "AVISIT",
      covariates = c("STRATA1"),
      id = "USUBJID"
    ),
    data = anl,
    cor_struct = "random-quadratic"
  )
}

test_that("h_mmrm_fixed works as expected", {

  if (compareVersion(as.character(packageVersion("lme4")), "1.1.21") <= 0) {
    skip("tests dont run with older version of lme4")
  }

  mmrm <- get_mmrm()
  result <- h_mmrm_fixed(mmrm, format = "xx.xxxx")
  result2 <- as.rtable(mmrm, type = "fixed", format = "xx.xxxx")
  expect_identical(result, result2)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "(Intercept)", "BMRKR2LOW", "BMRKR2MEDIUM", "ARMA: Drug X",
      "ARMC: Combination", "AVISITWEEK 2 DAY 15", "AVISITWEEK 3 DAY 22",
      "AVISITWEEK 4 DAY 29", "AVISITWEEK 5 DAY 36", "ARMA: Drug X:AVISITWEEK 2 DAY 15",
      "ARMC: Combination:AVISITWEEK 2 DAY 15", "ARMA: Drug X:AVISITWEEK 3 DAY 22",
      "ARMC: Combination:AVISITWEEK 3 DAY 22", "ARMA: Drug X:AVISITWEEK 4 DAY 29",
      "ARMC: Combination:AVISITWEEK 4 DAY 29", "ARMA: Drug X:AVISITWEEK 5 DAY 36",
      "ARMC: Combination:AVISITWEEK 5 DAY 36", "Estimate", "52.0558",
      "1.6235", "-2.278", "0.1208", "-3.3034", "-5.1729", "-1.4592",
      "-1.5838", "-1.1984", "4.0368", "5.4127", "1.0474", "5.444",
      "-1.6001", "5.6466", "-1.5418", "6.9197", "Std. Error", "2.5006",
      "1.5467", "1.2309", "3.0751", "3.2642", "3.3257", "3.4081", "3.4457",
      "3.4141", "4.2682", "4.5188", "4.3739", "4.6307", "4.4221", "4.6818",
      "4.3815", "4.6388", "t value", "20.8177", "1.0497", "-1.8507",
      "0.0393", "-1.012", "-1.5554", "-0.4282", "-0.4596", "-0.351",
      "0.9458", "1.1978", "0.2395", "1.1756", "-0.3618", "1.2061",
      "-0.3519", "1.4917", "df", "100", "142", "142", "88", "89", "150",
      "71", "65", "66", "150", "150", "71", "71", "65", "65", "66",
      "66", "Pr(>|t|)", "<0.0001", "0.2957", "0.0663", "0.9688", "0.3143",
      "0.1220", "0.6698", "0.6473", "0.7267", "0.3458", "0.2329", "0.8114",
      "0.2437", "0.7187", "0.2322", "0.7260", "0.1406"),
    .Dim = c(18L, 6L)
  )

  result_numbers <- as.numeric(result_matrix[grepl("^-?\\d+", result_matrix)])
  expected_numbers <- as.numeric(expected_matrix[grepl("^-?\\d+", result_matrix)])

  expect_equal(result_numbers, expected_numbers, tolerance = 1e-3)
})

test_that("h_mmrm_cov works as expected", {

  if (compareVersion(as.character(packageVersion("lme4")), "1.1.21") <= 0) {
    skip("tests dont run with older version of lme4")
  }

  skip_if_too_deep(2)

  mmrm <- get_mmrm()
  result <- h_mmrm_cov(mmrm, format = "xx.xxxx")
  result2 <- as.rtable(mmrm, type = "cov", format = "xx.xxxx")
  expect_identical(result, result2)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22",
      "WEEK 4 DAY 29", "WEEK 5 DAY 36", "WEEK 1 DAY 8", "62.9854",
      "0.1138", "-2.2943", "-3.144", "-2.4352", "WEEK 2 DAY 15", "0.1138",
      "58.9081", "-0.064", "-0.0877", "-0.0679", "WEEK 3 DAY 22", "-2.2943",
      "-0.064", "60.195", "1.7678", "1.3693", "WEEK 4 DAY 29", "-3.144",
      "-0.0877", "1.7678", "61.3275", "1.8764", "WEEK 5 DAY 36", "-2.4352",
      "-0.0679", "1.3693", "1.8764", "60.3583"),
    .Dim = c(6L, 6L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("h_mmrm_diagnostic works as expected", {

  skip_if_too_deep(2)

  mmrm <- get_mmrm()
  result <- h_mmrm_diagnostic(mmrm, format = "xx.x")
  result2 <- as.rtable(mmrm, type = "diagnostic", format = "xx.x")
  expect_identical(result, result2)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "REML criterion", "AIC", "AICc", "BIC", "Diagnostic statistic value",
      "1351.4", "1365.4", "1366", "1377.4"),
    .Dim = c(5L, 2L)
  )
  expect_identical(result_matrix, expected_matrix)
})

test_that("tidy.mmrm works as expected", {

  skip_if_too_deep(2)

  mmrm <- get_mmrm()
  result <- broom::tidy(mmrm)
  result_one_row <- result[8, ]
  expected_one_row <- tibble::tibble(
    ARM = factor("A: Drug X", levels = c("B: Placebo", "A: Drug X", "C: Combination")),
    AVISIT = factor(
      "WEEK 3 DAY 22",
      levels = c("WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22", "WEEK 4 DAY 29", "WEEK 5 DAY 36")
    ),
    estimate_est = 51.1925843621987, se_est = 1.88904859234177,
    df_est = 140.319306325214, lower_cl_est = 47.4579079047326,
    upper_cl_est = 54.9272608196649, n = 17L,
    estimate_contr = 1.1682418182127,
    se_contr = 3.00637183592203,
    df_contr = 140.141644095178,
    lower_cl_contr = -4.77546443092749,
    upper_cl_contr = 7.11194806735289,
    t_stat = 0.388588598474018,
    p_value = 0.698170225099465,
    relative_reduc = -0.0233534667084426,
    conf_level = 0.95
  )
  expect_is(result_one_row, "tbl_df")
  expect_equal(as.data.frame(result_one_row), as.data.frame(expected_one_row), tolerance = 0.001)
})

test_that("tidy.mmrm works as expected when treatment is not considered in the model", {

  skip_if_too_deep(2)

  mmrm <- get_mmrm_no_arm()
  result <- broom::tidy(mmrm)
  result_one_row <- result[5, ]
  expected_one_row <- tibble::tibble(
    AVISIT = factor(
      "WEEK 5 DAY 36",
      levels = c("WEEK 1 DAY 8", "WEEK 2 DAY 15", "WEEK 3 DAY 22", "WEEK 4 DAY 29", "WEEK 5 DAY 36")
    ),
    estimate_est = 50.84261, se_est = 1.263442,
    df_est = 69.64544, lower_cl_est = 48.32253,
    upper_cl_est = 53.36269, n = 41L,
    conf_level = 0.95
  )
  expect_is(result_one_row, "tbl_df")
  expect_equivalent(as.data.frame(result_one_row), as.data.frame(expected_one_row), tolerance = 0.001)
})

test_that("s_mmrm_lsmeans works as expected when not in reference column", {
  mmrm <- get_mmrm()
  df <- broom::tidy(mmrm)
  result <- s_mmrm_lsmeans(df[8, ], FALSE)
  expected <- list(
    n = 17L,
    adj_mean_se = c(51.1925843621987, 1.88904859234177),
    adj_mean_ci = with_label(c(47.4579079047326, 54.9272608196649), label = "95% CI"),
    diff_mean_se = c(1.1682418182127, 3.00637183592203),
    diff_mean_ci = with_label(c(-4.77546443092749, 7.11194806735289), label = "95% CI"),
    change = with_label(-0.0233534667084426, label = "Relative Reduction (%)"),
    p_value = 0.698170225099465
  )
  expect_equal(result, expected, tolerance = 0.001)
})

test_that("s_mmrm_lsmeans works as expected when in reference column", {

  skip_if_too_deep(2)

  mmrm <- get_mmrm()
  df <- broom::tidy(mmrm)
  result <- s_mmrm_lsmeans(df[2, ], TRUE)
  expected <- list(
    n = 11L,
    adj_mean_se = c(46.3107108312587, 2.31529223374449),
    adj_mean_ci = with_label(c(41.7359227134741, 50.8854989490432), label = "95% CI"),
    diff_mean_se = character(0),
    diff_mean_ci = with_label(character(0), label = "95% CI"),
    change = with_label(character(0), label = "Relative Reduction (%)"),
    p_value = character(0)
  )
  expect_equal(result, expected, tolerance = 0.001)
})

test_that("s_mmrm_lsmeans_single works as expected", {

  skip_if_too_deep(2)

  mmrm <- get_mmrm_no_arm()
  df <- broom::tidy(mmrm)
  result <- s_mmrm_lsmeans_single(df[2, ])
  expected <- list(
    n = 41L,
    adj_mean_se = c(48.703420, 1.180417),
    adj_mean_ci = with_label(c(46.37198, 51.03486), label = "95% CI")
  )
  expect_equal(result, expected, tolerance = 0.001)
})

test_that("summarize_lsmeans works as expected", {

  skip_if_too_deep(2)

  if (compareVersion(as.character(packageVersion("lme4")), "1.1.21") <= 0) {
    skip("tests dont run with older version of lme4")
  }

  mmrm <- get_mmrm()
  df <- broom::tidy(mmrm)
  result <- basic_table() %>%
    split_cols_by("ARM", ref_group = mmrm$ref_level) %>%
    split_rows_by("AVISIT") %>%
    summarize_lsmeans(show_relative = "increase") %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "WEEK 1 DAY 8", "n", "Adjusted Mean (SE)", "95% CI",
      "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)",
      "p-value (MMRM)", "WEEK 2 DAY 15", "n", "Adjusted Mean (SE)",
      "95% CI", "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)",
      "p-value (MMRM)", "WEEK 3 DAY 22", "n", "Adjusted Mean (SE)",
      "95% CI", "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)",
      "p-value (MMRM)", "WEEK 4 DAY 29", "n", "Adjusted Mean (SE)",
      "95% CI", "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)",
      "p-value (MMRM)", "WEEK 5 DAY 36", "n", "Adjusted Mean (SE)",
      "95% CI", "Difference in Adjusted Means (SE)", "95% CI", "Relative Increase (%)",
      "p-value (MMRM)", "B: Placebo", "", "11", "51.484 (2.394)", "(46.726, 56.241)",
      "", "", "", "", "", "11", "46.311 (2.315)", "(41.736, 50.885)",
      "", "", "", "", "", "11", "50.024 (2.340)", "(45.397, 54.651)",
      "", "", "", "", "", "11", "49.900 (2.362)", "(45.235, 54.565)",
      "", "", "", "", "", "11", "50.285 (2.344)", "(45.638, 54.933)",
      "", "", "", "", "A: Drug X", "", "17", "51.604 (1.932)", "(47.765, 55.443)",
      "0.121 (3.075)", "(-5.99, 6.232)", "0.2%", "0.9688", "", "17",
      "50.468 (1.869)", "(46.776, 54.161)", "4.158 (2.974)", "(-1.719, 10.034)",
      "9%", "0.1642", "", "17", "51.193 (1.889)", "(47.458, 54.927)",
      "1.168 (3.006)", "(-4.775, 7.112)", "2.3%", "0.6982", "", "17",
      "48.421 (1.907)", "(44.655, 52.186)", "-1.479 (3.034)", "(-7.472, 4.513)",
      "-3%", "0.6266", "", "17", "48.864 (1.892)", "(45.113, 52.615)",
      "-1.421 (3.010)", "(-7.391, 4.549)", "-2.8%", "0.6379", "C: Combination",
      "", "13", "48.180 (2.215)", "(43.78, 52.58)", "-3.303 (3.264)",
      "(-9.789, 3.183)", "-6.4%", "0.3143", "", "13", "48.420 (2.143)",
      "(44.186, 52.654)", "2.109 (3.158)", "(-4.13, 8.349)", "4.6%",
      "0.5051", "", "13", "52.165 (2.166)", "(47.883, 56.446)", "2.141 (3.192)",
      "(-4.169, 8.451)", "4.3%", "0.5035", "", "13", "52.243 (2.186)",
      "(47.927, 56.559)", "2.343 (3.221)", "(-4.018, 8.705)", "4.7%",
      "0.4680", "", "13", "53.902 (2.169)", "(49.602, 58.202)", "3.616 (3.196)",
      "(-2.721, 9.954)", "7.2%", "0.2604"),
    .Dim = c(41L, 4L)
  )

  result_numbers <- as.numeric(result_matrix[grepl("^-?\\d+", result_matrix)])
  expected_numbers <- as.numeric(expected_matrix[grepl("^-?\\d+", result_matrix)])

  expect_equal(result_numbers, expected_numbers, tolerance = 1e-2)

})

test_that("summarize_lsmeans works as expected when treatment is not considered in the model", {

  skip_if_too_deep(2)

  if (compareVersion(as.character(packageVersion("lme4")), "1.1.21") <= 0) {
    skip("tests dont run with older version of lme4")
  }

  mmrm <- get_mmrm_no_arm()
  df <- broom::tidy(mmrm)
  result <- basic_table() %>%
    split_rows_by("AVISIT") %>%
    summarize_lsmeans(arms = FALSE) %>%
    build_table(df)
  result_matrix <- to_string_matrix(result)
  expected_matrix <- structure(
    c("", "WEEK 1 DAY 8", "n", "Adjusted Mean (SE)", "95% CI",
      "WEEK 2 DAY 15", "n", "Adjusted Mean (SE)", "95% CI", "WEEK 3 DAY 22",
      "n", "Adjusted Mean (SE)", "95% CI", "WEEK 4 DAY 29", "n", "Adjusted Mean (SE)",
      "95% CI", "WEEK 5 DAY 36", "n", "Adjusted Mean (SE)", "95% CI",
      "all obs", "", "41", "50.486 (1.238)", "(48.03, 52.942)", "",
      "41", "48.703 (1.194)", "(46.346, 51.061)", "", "41", "51.187 (1.230)",
      "(48.752, 53.623)", "", "41", "50.029 (1.266)", "(47.527, 52.532)",
      "", "41", "50.843 (1.263)", "(48.323, 53.363)"),
    .Dim = c(21L, 2L)
  )
  expect_identical(result_matrix, expected_matrix)
})
