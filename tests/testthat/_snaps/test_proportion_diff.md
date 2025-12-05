# prop_chisq returns right result

    Code
      res
    Output
      [1] 0.05653028

---

    Code
      res_less
    Output
      [1] 0.9717349

---

    Code
      res_greater
    Output
      [1] 0.02826514

# prop_cmh returns right result

    Code
      res
    Output
      [1] 0.6477165

---

    Code
      res_less
    Output
      [1] 0.6761418

---

    Code
      res_greater
    Output
      [1] 0.3238582

# prop_cmh also works when there are strata with just one observation

    Code
      res
    Output
      [1] 0.3325724

# prop_fisher returns right result

    Code
      res
    Output
      [1] 0.1109695

---

    Code
      res_less
    Output
      [1] 0.9875791

---

    Code
      res_greater
    Output
      [1] 0.05548477

# prop_schouten returns right result

    Code
      res
    Output
        [1] 5.717098e-16 2.711495e-15 2.388967e-05 5.496444e-01 7.034548e-01
        [6] 1.528589e-02 6.524537e-06 3.273227e-02 5.066009e-04 8.413189e-20
       [11] 7.430342e-01 3.570927e-02 7.211155e-17 1.456706e-07 5.529267e-01
       [16] 5.027609e-02 3.260721e-13 1.974675e-17 1.481596e-01 7.243689e-01
       [21] 6.734540e-21 1.240955e-07 1.151700e-04 1.371002e-01 6.621880e-02
       [26] 1.032789e-01 2.491135e-04 1.401064e-08 8.069295e-10 3.838188e-01
       [31] 2.100092e-04 2.897827e-08 3.391773e-01 8.044239e-05 7.426580e-02
       [36] 6.502371e-06 1.735152e-06 7.972925e-07 6.737793e-01 7.559297e-01
       [41] 1.340414e-14 4.699432e-07 9.127204e-05 4.510684e-11 1.957185e-01
       [46] 1.994881e-01 5.772153e-03 5.600847e-02 2.910405e-01 3.223508e-01
       [51] 4.809361e-02 9.683368e-13 6.568565e-02 5.456156e-01 6.423022e-01
       [56] 2.369506e-02 1.508898e-05 1.015739e-06 1.160665e-02 9.735705e-01
       [61] 3.316501e-01 9.246754e-02           NA 2.662878e-01 4.718556e-02
       [66] 3.963034e-05 6.313901e-10 1.034260e-13 9.160885e-03 1.772388e-01
       [71] 3.102986e-03 9.381853e-04 1.313180e-03 3.275858e-01 1.691572e-01
       [76] 3.690328e-01 1.734528e-05 7.046703e-13 9.068436e-01 1.379968e-02
       [81] 3.203479e-04 3.722763e-07 1.318029e-02 3.668561e-26 9.758038e-01
       [86] 8.383837e-07 5.848356e-05 8.842058e-09 1.060928e-01 1.318974e-02
       [91] 4.432164e-01 5.103689e-06 4.082526e-04 5.098830e-02 1.118992e-03
       [96] 2.445414e-15 1.476617e-08 5.641109e-01 5.317809e-03 2.889880e-04

# prop_schouten returns right result for less or greater alternative

    Code
      res_less
    Output
      [1] 0.9578287

---

    Code
      res_greater
    Output
      [1] 0.04217134

# prop_cmh with Wilson-Hilferty transformation works

    Code
      res_less
    Output
      [1] 0.6522653

---

    Code
      res_greater
    Output
      [1] 0.3477347

# s_test_proportion_diff and d_test_proportion_diff return right result

    Code
      res
    Output
      $d
      [1] "p-value (Cochran-Mantel-Haenszel Test)"
      
      $s
      $s$pval
      [1] 0.6477165
      attr(,"label")
      [1] "p-value (Cochran-Mantel-Haenszel Test)"
      
      

# s_test_proportion_diff and d_test_proportion_diff work with less and greater alternatives

    Code
      res
    Output
      $d
      [1] "p-value (Cochran-Mantel-Haenszel Test, 1-sided, direction greater)"
      
      $s
      $s$pval
      [1] 0.6761418
      attr(,"label")
      [1] "p-value (Cochran-Mantel-Haenszel Test, 1-sided, direction greater)"
      
      

---

    Code
      res
    Output
      $d
      [1] "p-value (Cochran-Mantel-Haenszel Test, 1-sided, direction less)"
      
      $s
      $s$pval
      [1] 0.3238582
      attr(,"label")
      [1] "p-value (Cochran-Mantel-Haenszel Test, 1-sided, direction less)"
      
      

# test_proportion_diff returns right result

    Code
      res
    Output
                                                 B     A   
      —————————————————————————————————————————————————————
        p-value (Cochran-Mantel-Haenszel Test)       0.6477

# test_proportion_diff uses alternative argument

    Code
      res
    Output
                                                                             B     A   
      —————————————————————————————————————————————————————————————————————————————————
        p-value (Cochran-Mantel-Haenszel Test, 1-sided, direction greater)       0.6761

# test_proportion_diff edge case: all responder by chisq

    Code
      res
    Output
                                     B     A   
      —————————————————————————————————————————
        p-value (Chi-Squared Test)       1.0000

# test_proportion_diff edge case: all responder by schouten

    Code
      res
    Output
                                                              B     A   
      ——————————————————————————————————————————————————————————————————
        p-value (Chi-Squared Test with Schouten Correction)       1.0000

# test_proportion_diff edge case: all responder by fisher

    Code
      res
    Output
                                          B     A   
      ——————————————————————————————————————————————
      Variable Label                                
          p-value (Fisher's Exact Test)       1.0000

# test_proportion_diff edge case: all responder by CMH

    Code
      res
    Output
                                                   B   A 
      ———————————————————————————————————————————————————
      Variable Label                                     
          p-value (Cochran-Mantel-Haenszel Test)       NA

# test_proportion_diff edge case: all responder by CMH with Wilson-Hilferty transformation

    Code
      res
    Output
                                                                                       B   A 
      ———————————————————————————————————————————————————————————————————————————————————————
      Variable Label                                                                         
          p-value (Cochran-Mantel-Haenszel Test with Wilson-Hilferty Transformation)       NA

