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

    c(5.71709780151386e-16, 2.71149466993128e-15, 2.3889666278922e-05, 
    0.549644428899163, 0.703454844664116, 0.0152858916552256, 6.52453722516531e-06, 
    0.0327322670572509, 0.000506600905337401, 8.41318893931116e-20, 
    0.743034241134202, 0.0357092689206786, 7.21115471170251e-17, 
    1.45670568158378e-07, 0.552926717015723, 0.0502760939221872, 
    3.26072131893282e-13, 1.97467455935962e-17, 0.148159618767163, 
    0.724368897795955, 6.73453970817436e-21, 1.24095498290808e-07, 
    0.000115170047481382, 0.137100155156638, 0.0662187965427006, 
    0.103278869004223, 0.000249113546833614, 1.40106351061449e-08, 
    8.06929481327806e-10, 0.383818805598473, 0.000210009244514486, 
    2.89782748092434e-08, 0.339177309250819, 8.04423918783469e-05, 
    0.0742658033596529, 6.50237102665681e-06, 1.73515164584978e-06, 
    7.97292519631648e-07, 0.673779281512192, 0.755929661910774, 1.34041438943676e-14, 
    4.69943184727821e-07, 9.12720383190411e-05, 4.51068424702267e-11, 
    0.195718524551842, 0.19948806332682, 0.0057721529577541, 0.0560084668646058, 
    0.29104053765655, 0.322350844144929, 0.048093607319379, 9.68336827831432e-13, 
    0.0656856504670571, 0.545615628847437, 0.642302173037625, 0.0236950615828867, 
    1.50889801946692e-05, 1.01573874991605e-06, 0.0116066467015481, 
    0.973570459792593, 0.331650059277109, 0.0924675432126077, NA, 
    0.266287847316958, 0.0471855622246397, 3.9630338558056e-05, 6.31390056514998e-10, 
    1.03425994808443e-13, 0.00916088462925696, 0.177238826615554, 
    0.00310298563550831, 0.000938185285257554, 0.001313179571731, 
    0.327585821520465, 0.16915719848394, 0.369032771061995, 1.73452827974862e-05, 
    7.04670334226855e-13, 0.906843585080977, 0.0137996779055368, 
    0.000320347934086559, 3.72276286720585e-07, 0.0131802938305678, 
    3.66856133697241e-26, 0.975803811608676, 8.38383670121597e-07, 
    5.84835590891316e-05, 8.84205783916123e-09, 0.106092781764247, 
    0.0131897374630874, 0.443216415158973, 5.10368883112867e-06, 
    0.000408252631151325, 0.0509882952700247, 0.00111899204979067, 
    2.4454141800447e-15, 1.4766170428799e-08, 0.564110859931511, 
    0.00531780904841515, 0.000288988027910912)

# prop_schouten returns right result for less or greater alternative

    0.957828656150792

---

    0.0421713438492082

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

