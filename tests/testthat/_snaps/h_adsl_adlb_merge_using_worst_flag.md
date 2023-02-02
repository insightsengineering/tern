# h_adsl_adlb_merge_using_worst_flag generates missing for patients without any lab

    Code
      res
    Output
                      USUBJID PARAMCD                                PARAM
      1  AB12345-CHN-3-id-128     ALT Alanine Aminotransferase Measurement
      2  AB12345-CHN-3-id-128     CRP       C-Reactive Protein Measurement
      3  AB12345-CHN-3-id-128     IGA         Immunoglobulin A Measurement
      4 AB12345-CHN-11-id-175     ALT Alanine Aminotransferase Measurement
      5 AB12345-CHN-11-id-175     CRP       C-Reactive Protein Measurement
      6 AB12345-CHN-11-id-175     IGA         Immunoglobulin A Measurement
               AVISIT AVISITN    ATOXGR    BTOXGR STUDYID COUNTRY SITEID SUBJID
      1 WEEK 4 DAY 29       4         1         0 AB12345     CHN  CHN-3 id-128
      2 WEEK 5 DAY 36       5         3         0 AB12345     CHN  CHN-3 id-128
      3 WEEK 5 DAY 36       5         4         0 AB12345     CHN  CHN-3 id-128
      4     <Missing>      NA <Missing> <Missing> AB12345     CHN CHN-11 id-175
      5     <Missing>      NA <Missing> <Missing> AB12345     CHN CHN-11 id-175
      6     <Missing>      NA <Missing> <Missing> AB12345     CHN CHN-11 id-175
             AGE SEX ARMCD        ARM ACTARMCD     ACTARM  RACE             TRTSDTM
      1 36.12988   M ARM B B: Placebo    ARM B B: Placebo ASIAN 2021-01-01 09:05:27
      2 36.12988   M ARM B B: Placebo    ARM B B: Placebo ASIAN 2021-01-01 09:05:27
      3 36.12988   M ARM B B: Placebo    ARM B B: Placebo ASIAN 2021-01-01 09:05:27
      4 38.29802   F ARM B B: Placebo    ARM B B: Placebo ASIAN 2019-11-03 18:20:42
      5 38.29802   F ARM B B: Placebo    ARM B B: Placebo ASIAN 2019-11-03 18:20:42
      6 38.29802   F ARM B B: Placebo    ARM B B: Placebo ASIAN 2019-11-03 18:20:42
                    TRTEDTM EOSDY STRATA1 STRATA2    BMRKR1 BMRKR2 REGION1 SAFFL
      1 2023-01-01 20:43:51   731       C      S1  7.677225 MEDIUM    Asia     Y
      2 2023-01-01 20:43:51   731       C      S1  7.677225 MEDIUM    Asia     Y
      3 2023-01-01 20:43:51   731       C      S1  7.677225 MEDIUM    Asia     Y
      4 2021-11-03 06:59:06   731       B      S1 11.351954    LOW    Asia     Y
      5 2021-11-03 06:59:06   731       B      S1 11.351954    LOW    Asia     Y
      6 2021-11-03 06:59:06   731       B      S1 11.351954    LOW    Asia     Y

# h_adsl_adlb_merge_using_worst_flag generates missing for patients missing baseline lab

    Code
      res
    Output
                      USUBJID PARAMCD                                PARAM
      1 AB12345-CHN-11-id-175     ALT Alanine Aminotransferase Measurement
      2 AB12345-CHN-11-id-175     CRP       C-Reactive Protein Measurement
      3 AB12345-CHN-11-id-175     IGA         Immunoglobulin A Measurement
      4  AB12345-CHN-3-id-128     ALT Alanine Aminotransferase Measurement
      5  AB12345-CHN-3-id-128     CRP       C-Reactive Protein Measurement
      6  AB12345-CHN-3-id-128     IGA         Immunoglobulin A Measurement
               AVISIT AVISITN ATOXGR    BTOXGR STUDYID COUNTRY SITEID SUBJID      AGE
      1 WEEK 5 DAY 36       5      0 <Missing> AB12345     CHN CHN-11 id-175 38.29802
      2 WEEK 4 DAY 29       4      0 <Missing> AB12345     CHN CHN-11 id-175 38.29802
      3  WEEK 1 DAY 8       1      2 <Missing> AB12345     CHN CHN-11 id-175 38.29802
      4 WEEK 4 DAY 29       4      1         0 AB12345     CHN  CHN-3 id-128 36.12988
      5 WEEK 5 DAY 36       5      3         0 AB12345     CHN  CHN-3 id-128 36.12988
      6 WEEK 5 DAY 36       5      4         0 AB12345     CHN  CHN-3 id-128 36.12988
        SEX ARMCD        ARM ACTARMCD     ACTARM  RACE             TRTSDTM
      1   F ARM B B: Placebo    ARM B B: Placebo ASIAN 2019-11-03 18:20:42
      2   F ARM B B: Placebo    ARM B B: Placebo ASIAN 2019-11-03 18:20:42
      3   F ARM B B: Placebo    ARM B B: Placebo ASIAN 2019-11-03 18:20:42
      4   M ARM B B: Placebo    ARM B B: Placebo ASIAN 2021-01-01 09:05:27
      5   M ARM B B: Placebo    ARM B B: Placebo ASIAN 2021-01-01 09:05:27
      6   M ARM B B: Placebo    ARM B B: Placebo ASIAN 2021-01-01 09:05:27
                    TRTEDTM EOSDY STRATA1 STRATA2    BMRKR1 BMRKR2 REGION1 SAFFL
      1 2021-11-03 06:59:06   731       B      S1 11.351954    LOW    Asia     Y
      2 2021-11-03 06:59:06   731       B      S1 11.351954    LOW    Asia     Y
      3 2021-11-03 06:59:06   731       B      S1 11.351954    LOW    Asia     Y
      4 2023-01-01 20:43:51   731       C      S1  7.677225 MEDIUM    Asia     Y
      5 2023-01-01 20:43:51   731       C      S1  7.677225 MEDIUM    Asia     Y
      6 2023-01-01 20:43:51   731       C      S1  7.677225 MEDIUM    Asia     Y

# h_adsl_adlb_merge_using_worst_flag generates missing for patients missing post-baseline lab

    Code
      res
    Output
                      USUBJID PARAMCD                                PARAM
      1 AB12345-CHN-11-id-175     ALT Alanine Aminotransferase Measurement
      2 AB12345-CHN-11-id-175     CRP       C-Reactive Protein Measurement
      3 AB12345-CHN-11-id-175     IGA         Immunoglobulin A Measurement
      4  AB12345-CHN-3-id-128     ALT Alanine Aminotransferase Measurement
      5  AB12345-CHN-3-id-128     CRP       C-Reactive Protein Measurement
      6  AB12345-CHN-3-id-128     IGA         Immunoglobulin A Measurement
               AVISIT AVISITN    ATOXGR BTOXGR STUDYID COUNTRY SITEID SUBJID      AGE
      1 WEEK 5 DAY 36       5 <Missing>      0 AB12345     CHN CHN-11 id-175 38.29802
      2 WEEK 4 DAY 29       4 <Missing>      0 AB12345     CHN CHN-11 id-175 38.29802
      3  WEEK 1 DAY 8       1 <Missing>      1 AB12345     CHN CHN-11 id-175 38.29802
      4 WEEK 4 DAY 29       4         1      0 AB12345     CHN  CHN-3 id-128 36.12988
      5 WEEK 5 DAY 36       5         3      0 AB12345     CHN  CHN-3 id-128 36.12988
      6 WEEK 5 DAY 36       5         4      0 AB12345     CHN  CHN-3 id-128 36.12988
        SEX ARMCD        ARM ACTARMCD     ACTARM  RACE             TRTSDTM
      1   F ARM B B: Placebo    ARM B B: Placebo ASIAN 2019-11-03 18:20:42
      2   F ARM B B: Placebo    ARM B B: Placebo ASIAN 2019-11-03 18:20:42
      3   F ARM B B: Placebo    ARM B B: Placebo ASIAN 2019-11-03 18:20:42
      4   M ARM B B: Placebo    ARM B B: Placebo ASIAN 2021-01-01 09:05:27
      5   M ARM B B: Placebo    ARM B B: Placebo ASIAN 2021-01-01 09:05:27
      6   M ARM B B: Placebo    ARM B B: Placebo ASIAN 2021-01-01 09:05:27
                    TRTEDTM EOSDY STRATA1 STRATA2    BMRKR1 BMRKR2 REGION1 SAFFL
      1 2021-11-03 06:59:06   731       B      S1 11.351954    LOW    Asia     Y
      2 2021-11-03 06:59:06   731       B      S1 11.351954    LOW    Asia     Y
      3 2021-11-03 06:59:06   731       B      S1 11.351954    LOW    Asia     Y
      4 2023-01-01 20:43:51   731       C      S1  7.677225 MEDIUM    Asia     Y
      5 2023-01-01 20:43:51   731       C      S1  7.677225 MEDIUM    Asia     Y
      6 2023-01-01 20:43:51   731       C      S1  7.677225 MEDIUM    Asia     Y

# h_adsl_adlb_merge_using_worst_flag generates missing for patients without any worst flagged post-baseline values

    Code
      res
    Output
                      USUBJID PARAMCD                                PARAM
      1  AB12345-CHN-3-id-128     ALT Alanine Aminotransferase Measurement
      2  AB12345-CHN-3-id-128     CRP       C-Reactive Protein Measurement
      3  AB12345-CHN-3-id-128     IGA         Immunoglobulin A Measurement
      4 AB12345-CHN-11-id-175     ALT Alanine Aminotransferase Measurement
      5 AB12345-CHN-11-id-175     CRP       C-Reactive Protein Measurement
      6 AB12345-CHN-11-id-175     IGA         Immunoglobulin A Measurement
               AVISIT AVISITN    ATOXGR    BTOXGR STUDYID COUNTRY SITEID SUBJID
      1 WEEK 4 DAY 29       4         1         0 AB12345     CHN  CHN-3 id-128
      2 WEEK 5 DAY 36       5         3         0 AB12345     CHN  CHN-3 id-128
      3 WEEK 5 DAY 36       5         4         0 AB12345     CHN  CHN-3 id-128
      4     <Missing>      NA <Missing> <Missing> AB12345     CHN CHN-11 id-175
      5     <Missing>      NA <Missing> <Missing> AB12345     CHN CHN-11 id-175
      6     <Missing>      NA <Missing> <Missing> AB12345     CHN CHN-11 id-175
             AGE SEX ARMCD        ARM ACTARMCD     ACTARM  RACE             TRTSDTM
      1 36.12988   M ARM B B: Placebo    ARM B B: Placebo ASIAN 2021-01-01 09:05:27
      2 36.12988   M ARM B B: Placebo    ARM B B: Placebo ASIAN 2021-01-01 09:05:27
      3 36.12988   M ARM B B: Placebo    ARM B B: Placebo ASIAN 2021-01-01 09:05:27
      4 38.29802   F ARM B B: Placebo    ARM B B: Placebo ASIAN 2019-11-03 18:20:42
      5 38.29802   F ARM B B: Placebo    ARM B B: Placebo ASIAN 2019-11-03 18:20:42
      6 38.29802   F ARM B B: Placebo    ARM B B: Placebo ASIAN 2019-11-03 18:20:42
                    TRTEDTM EOSDY STRATA1 STRATA2    BMRKR1 BMRKR2 REGION1 SAFFL
      1 2023-01-01 20:43:51   731       C      S1  7.677225 MEDIUM    Asia     Y
      2 2023-01-01 20:43:51   731       C      S1  7.677225 MEDIUM    Asia     Y
      3 2023-01-01 20:43:51   731       C      S1  7.677225 MEDIUM    Asia     Y
      4 2021-11-03 06:59:06   731       B      S1 11.351954    LOW    Asia     Y
      5 2021-11-03 06:59:06   731       B      S1 11.351954    LOW    Asia     Y
      6 2021-11-03 06:59:06   731       B      S1 11.351954    LOW    Asia     Y

# h_adsl_adlb_merge_using_worst_flag generates missing and by visit lab results

    Code
      res
    Output
                       USUBJID        AVISIT AVISITN PARAMCD
      1   AB12345-CHN-3-id-128  WEEK 1 DAY 8       1     ALT
      2   AB12345-CHN-3-id-128 WEEK 2 DAY 15       2     ALT
      3   AB12345-CHN-3-id-128 WEEK 3 DAY 22       3     ALT
      4   AB12345-CHN-3-id-128 WEEK 4 DAY 29       4     ALT
      5   AB12345-CHN-3-id-128 WEEK 5 DAY 36       5     ALT
      6   AB12345-CHN-3-id-128  WEEK 1 DAY 8       1     CRP
      7   AB12345-CHN-3-id-128 WEEK 2 DAY 15       2     CRP
      8   AB12345-CHN-3-id-128 WEEK 3 DAY 22       3     CRP
      9   AB12345-CHN-3-id-128 WEEK 4 DAY 29       4     CRP
      10  AB12345-CHN-3-id-128 WEEK 5 DAY 36       5     CRP
      11  AB12345-CHN-3-id-128  WEEK 1 DAY 8       1     IGA
      12  AB12345-CHN-3-id-128 WEEK 2 DAY 15       2     IGA
      13  AB12345-CHN-3-id-128 WEEK 3 DAY 22       3     IGA
      14  AB12345-CHN-3-id-128 WEEK 4 DAY 29       4     IGA
      15  AB12345-CHN-3-id-128 WEEK 5 DAY 36       5     IGA
      16 AB12345-CHN-11-id-175  WEEK 1 DAY 8       1     ALT
      17 AB12345-CHN-11-id-175 WEEK 2 DAY 15       2     ALT
      18 AB12345-CHN-11-id-175 WEEK 3 DAY 22       3     ALT
      19 AB12345-CHN-11-id-175 WEEK 4 DAY 29       4     ALT
      20 AB12345-CHN-11-id-175 WEEK 5 DAY 36       5     ALT
      21 AB12345-CHN-11-id-175  WEEK 1 DAY 8       1     CRP
      22 AB12345-CHN-11-id-175 WEEK 2 DAY 15       2     CRP
      23 AB12345-CHN-11-id-175 WEEK 3 DAY 22       3     CRP
      24 AB12345-CHN-11-id-175 WEEK 4 DAY 29       4     CRP
      25 AB12345-CHN-11-id-175 WEEK 5 DAY 36       5     CRP
      26 AB12345-CHN-11-id-175  WEEK 1 DAY 8       1     IGA
      27 AB12345-CHN-11-id-175 WEEK 2 DAY 15       2     IGA
      28 AB12345-CHN-11-id-175 WEEK 3 DAY 22       3     IGA
      29 AB12345-CHN-11-id-175 WEEK 4 DAY 29       4     IGA
      30 AB12345-CHN-11-id-175 WEEK 5 DAY 36       5     IGA
                                        PARAM    ATOXGR    BTOXGR STUDYID COUNTRY
      1  Alanine Aminotransferase Measurement         0         0 AB12345     CHN
      2  Alanine Aminotransferase Measurement         0         0 AB12345     CHN
      3  Alanine Aminotransferase Measurement         0         0 AB12345     CHN
      4  Alanine Aminotransferase Measurement         1         0 AB12345     CHN
      5  Alanine Aminotransferase Measurement        -4         0 AB12345     CHN
      6        C-Reactive Protein Measurement         0         0 AB12345     CHN
      7        C-Reactive Protein Measurement         0         0 AB12345     CHN
      8        C-Reactive Protein Measurement         0         0 AB12345     CHN
      9        C-Reactive Protein Measurement         2         0 AB12345     CHN
      10       C-Reactive Protein Measurement         3         0 AB12345     CHN
      11         Immunoglobulin A Measurement        -1         0 AB12345     CHN
      12         Immunoglobulin A Measurement         0         0 AB12345     CHN
      13         Immunoglobulin A Measurement         0         0 AB12345     CHN
      14         Immunoglobulin A Measurement        -4         0 AB12345     CHN
      15         Immunoglobulin A Measurement         4         0 AB12345     CHN
      16 Alanine Aminotransferase Measurement <Missing> <Missing> AB12345     CHN
      17 Alanine Aminotransferase Measurement <Missing> <Missing> AB12345     CHN
      18 Alanine Aminotransferase Measurement <Missing> <Missing> AB12345     CHN
      19 Alanine Aminotransferase Measurement <Missing> <Missing> AB12345     CHN
      20 Alanine Aminotransferase Measurement <Missing> <Missing> AB12345     CHN
      21       C-Reactive Protein Measurement <Missing> <Missing> AB12345     CHN
      22       C-Reactive Protein Measurement <Missing> <Missing> AB12345     CHN
      23       C-Reactive Protein Measurement <Missing> <Missing> AB12345     CHN
      24       C-Reactive Protein Measurement <Missing> <Missing> AB12345     CHN
      25       C-Reactive Protein Measurement <Missing> <Missing> AB12345     CHN
      26         Immunoglobulin A Measurement <Missing> <Missing> AB12345     CHN
      27         Immunoglobulin A Measurement <Missing> <Missing> AB12345     CHN
      28         Immunoglobulin A Measurement <Missing> <Missing> AB12345     CHN
      29         Immunoglobulin A Measurement <Missing> <Missing> AB12345     CHN
      30         Immunoglobulin A Measurement <Missing> <Missing> AB12345     CHN
         SITEID SUBJID      AGE SEX ARMCD        ARM ACTARMCD     ACTARM  RACE
      1   CHN-3 id-128 36.12988   M ARM B B: Placebo    ARM B B: Placebo ASIAN
      2   CHN-3 id-128 36.12988   M ARM B B: Placebo    ARM B B: Placebo ASIAN
      3   CHN-3 id-128 36.12988   M ARM B B: Placebo    ARM B B: Placebo ASIAN
      4   CHN-3 id-128 36.12988   M ARM B B: Placebo    ARM B B: Placebo ASIAN
      5   CHN-3 id-128 36.12988   M ARM B B: Placebo    ARM B B: Placebo ASIAN
      6   CHN-3 id-128 36.12988   M ARM B B: Placebo    ARM B B: Placebo ASIAN
      7   CHN-3 id-128 36.12988   M ARM B B: Placebo    ARM B B: Placebo ASIAN
      8   CHN-3 id-128 36.12988   M ARM B B: Placebo    ARM B B: Placebo ASIAN
      9   CHN-3 id-128 36.12988   M ARM B B: Placebo    ARM B B: Placebo ASIAN
      10  CHN-3 id-128 36.12988   M ARM B B: Placebo    ARM B B: Placebo ASIAN
      11  CHN-3 id-128 36.12988   M ARM B B: Placebo    ARM B B: Placebo ASIAN
      12  CHN-3 id-128 36.12988   M ARM B B: Placebo    ARM B B: Placebo ASIAN
      13  CHN-3 id-128 36.12988   M ARM B B: Placebo    ARM B B: Placebo ASIAN
      14  CHN-3 id-128 36.12988   M ARM B B: Placebo    ARM B B: Placebo ASIAN
      15  CHN-3 id-128 36.12988   M ARM B B: Placebo    ARM B B: Placebo ASIAN
      16 CHN-11 id-175 38.29802   F ARM B B: Placebo    ARM B B: Placebo ASIAN
      17 CHN-11 id-175 38.29802   F ARM B B: Placebo    ARM B B: Placebo ASIAN
      18 CHN-11 id-175 38.29802   F ARM B B: Placebo    ARM B B: Placebo ASIAN
      19 CHN-11 id-175 38.29802   F ARM B B: Placebo    ARM B B: Placebo ASIAN
      20 CHN-11 id-175 38.29802   F ARM B B: Placebo    ARM B B: Placebo ASIAN
      21 CHN-11 id-175 38.29802   F ARM B B: Placebo    ARM B B: Placebo ASIAN
      22 CHN-11 id-175 38.29802   F ARM B B: Placebo    ARM B B: Placebo ASIAN
      23 CHN-11 id-175 38.29802   F ARM B B: Placebo    ARM B B: Placebo ASIAN
      24 CHN-11 id-175 38.29802   F ARM B B: Placebo    ARM B B: Placebo ASIAN
      25 CHN-11 id-175 38.29802   F ARM B B: Placebo    ARM B B: Placebo ASIAN
      26 CHN-11 id-175 38.29802   F ARM B B: Placebo    ARM B B: Placebo ASIAN
      27 CHN-11 id-175 38.29802   F ARM B B: Placebo    ARM B B: Placebo ASIAN
      28 CHN-11 id-175 38.29802   F ARM B B: Placebo    ARM B B: Placebo ASIAN
      29 CHN-11 id-175 38.29802   F ARM B B: Placebo    ARM B B: Placebo ASIAN
      30 CHN-11 id-175 38.29802   F ARM B B: Placebo    ARM B B: Placebo ASIAN
                     TRTSDTM             TRTEDTM EOSDY STRATA1 STRATA2    BMRKR1
      1  2021-01-01 09:05:27 2023-01-01 20:43:51   731       C      S1  7.677225
      2  2021-01-01 09:05:27 2023-01-01 20:43:51   731       C      S1  7.677225
      3  2021-01-01 09:05:27 2023-01-01 20:43:51   731       C      S1  7.677225
      4  2021-01-01 09:05:27 2023-01-01 20:43:51   731       C      S1  7.677225
      5  2021-01-01 09:05:27 2023-01-01 20:43:51   731       C      S1  7.677225
      6  2021-01-01 09:05:27 2023-01-01 20:43:51   731       C      S1  7.677225
      7  2021-01-01 09:05:27 2023-01-01 20:43:51   731       C      S1  7.677225
      8  2021-01-01 09:05:27 2023-01-01 20:43:51   731       C      S1  7.677225
      9  2021-01-01 09:05:27 2023-01-01 20:43:51   731       C      S1  7.677225
      10 2021-01-01 09:05:27 2023-01-01 20:43:51   731       C      S1  7.677225
      11 2021-01-01 09:05:27 2023-01-01 20:43:51   731       C      S1  7.677225
      12 2021-01-01 09:05:27 2023-01-01 20:43:51   731       C      S1  7.677225
      13 2021-01-01 09:05:27 2023-01-01 20:43:51   731       C      S1  7.677225
      14 2021-01-01 09:05:27 2023-01-01 20:43:51   731       C      S1  7.677225
      15 2021-01-01 09:05:27 2023-01-01 20:43:51   731       C      S1  7.677225
      16 2019-11-03 18:20:42 2021-11-03 06:59:06   731       B      S1 11.351954
      17 2019-11-03 18:20:42 2021-11-03 06:59:06   731       B      S1 11.351954
      18 2019-11-03 18:20:42 2021-11-03 06:59:06   731       B      S1 11.351954
      19 2019-11-03 18:20:42 2021-11-03 06:59:06   731       B      S1 11.351954
      20 2019-11-03 18:20:42 2021-11-03 06:59:06   731       B      S1 11.351954
      21 2019-11-03 18:20:42 2021-11-03 06:59:06   731       B      S1 11.351954
      22 2019-11-03 18:20:42 2021-11-03 06:59:06   731       B      S1 11.351954
      23 2019-11-03 18:20:42 2021-11-03 06:59:06   731       B      S1 11.351954
      24 2019-11-03 18:20:42 2021-11-03 06:59:06   731       B      S1 11.351954
      25 2019-11-03 18:20:42 2021-11-03 06:59:06   731       B      S1 11.351954
      26 2019-11-03 18:20:42 2021-11-03 06:59:06   731       B      S1 11.351954
      27 2019-11-03 18:20:42 2021-11-03 06:59:06   731       B      S1 11.351954
      28 2019-11-03 18:20:42 2021-11-03 06:59:06   731       B      S1 11.351954
      29 2019-11-03 18:20:42 2021-11-03 06:59:06   731       B      S1 11.351954
      30 2019-11-03 18:20:42 2021-11-03 06:59:06   731       B      S1 11.351954
         BMRKR2 REGION1 SAFFL
      1  MEDIUM    Asia     Y
      2  MEDIUM    Asia     Y
      3  MEDIUM    Asia     Y
      4  MEDIUM    Asia     Y
      5  MEDIUM    Asia     Y
      6  MEDIUM    Asia     Y
      7  MEDIUM    Asia     Y
      8  MEDIUM    Asia     Y
      9  MEDIUM    Asia     Y
      10 MEDIUM    Asia     Y
      11 MEDIUM    Asia     Y
      12 MEDIUM    Asia     Y
      13 MEDIUM    Asia     Y
      14 MEDIUM    Asia     Y
      15 MEDIUM    Asia     Y
      16    LOW    Asia     Y
      17    LOW    Asia     Y
      18    LOW    Asia     Y
      19    LOW    Asia     Y
      20    LOW    Asia     Y
      21    LOW    Asia     Y
      22    LOW    Asia     Y
      23    LOW    Asia     Y
      24    LOW    Asia     Y
      25    LOW    Asia     Y
      26    LOW    Asia     Y
      27    LOW    Asia     Y
      28    LOW    Asia     Y
      29    LOW    Asia     Y
      30    LOW    Asia     Y

