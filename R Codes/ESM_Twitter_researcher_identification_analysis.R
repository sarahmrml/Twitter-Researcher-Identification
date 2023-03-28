##### Analysis Code: Chi-Square test and Fisher`s exact z test #####

## independence test (Chi-Square and Fisher's Exact Test for small sample)

# https://towardsdatascience.com/fishers-exact-test-in-r-independence-test-for-a-small-sample-56965db48e87
# https://stackoverflow.com/questions/17052639/fisher-test-error-ldstp-is-too-small


# gender ####
gender_dat <- data.frame("OVID" = c(132413, 93219), "Twitter" = c(8793, 4772))
row.names(gender_dat) <- c("female", "male")

chisq.test(gender_dat)$expected
chisq.test(gender_dat)
# p-value < 2.2e-16


# location ####
location_dat <- data.frame("OVID" = c(96862, 26812, 19360, 14070, 12398, 11913, 7813, 7099, 6885, 6630, 5530, 4707, 
                                      3564, 3241, 2899, 2862, 2829, 2649, 2498, 2463, 2407, 2209, 2196, 1906, 1707, 
                                      1703, 1476, 1400, 1371, 1306, 1246, 1239, 1186, 1018, 856, 672, 655, 649, 584,
                                      566, 511, 443, 426, 381, 317, 315, 311, 296, 279, 277, 261, 251, 235, 224, 219,
                                      217, 202, 188, 186, 182, 177, 172, 170, 170, 157, 153, 144, 133, 131, 111, 111,
                                      108, 86, 86, 83, 82, 81, 73, 72, 68, 68, 66, 62, 62, 61, 58, 56, 51, 43, 42, 42,
                                      40, 39, 34, 33, 33, 33, 32, 32, 31, 30, 28, 26, 25, 22, 22, 21, 21, 19, 19, 18, 18,
                                      18, 18, 17, 14, 14, 13, 13, 13, 12, 12, 11, 9, 9, 9, 9, 9, 8, 8, 8, 8, 8, 8, 8, 7, 7,
                                      7, 7, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2,
                                      2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0),
                           "Twitter" = c(4548, 8, 2937, 833, 368, 768, 71, 98, 330, 25, 112, 37, 54, 5, 72, 63, 22, 6, 3,
                                         65, 55, 114, 34, 51, 268, 19, 19, 78, 13, 20, 15, 15, 41, 13, 21, 21, 8, 6, 4, 17,
                                         13, 3, 8, 5, 24, 0, 10, 15, 2, 5, 0, 3, 2, 1, 3, 7, 3, 0, 2, 11, 1, 1, 1, 0, 3, 0, 1,
                                         4, 5, 4, 1, 4, 1, 1, 0, 56, 0, 0, 0, 0, 2, 4, 2, 2, 4, 0, 3, 0, 0, 2, 4, 2, 3, 1, 0, 2,
                                         0, 1, 0, 5, 2, 0, 1, 0, 0, 1, 1, 2, 1, 0, 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 2, 0, 0, 0,
                                         0, 0, 2, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0,
                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1)
                           )
row.names(location_dat) <- c("US", "CN", "GB", "CA", "DE", "AU", "IT", "ES", "NL", "JP", "FR", "BR", "SE", "KR", "CH", "TR", "IL", "IR", "TW", "NO", "IN", "BE", "DK", "FI",
                             "IE", "PL", "PT", "NZ", "RU", "AT", "SG", "MX", "ZA", "GR", "CL", "MY", "HU", "CZ", "AR", "CO", "PK", "TH", "SA", "NG", "PH", "EG", "RS", "ID",
                             "LB", "RO", "JO", "HR", "ET", "BD", "VN", "CY", "GH", "GE", "EE", "AE", "SK", "SI", "TN", "UG", "LT", "KE", "QA", "PE", "IS", "LU", "MA", "EC",
                             "LK", "TZ", "AG", "HK", "UY", "BH", "KH", "CU", "KZ", "NP", "KW", "OM", "UA", "BG", "MT", "LV", "PS", "BA", "CR", "JM", "ZW", "MD", "AM", "CM",
                             "LY", "BW", "RW", "PR", "TT", "ZM", "MK", "DO", "DZ", "VE", "IQ", "PA", "FJ", "MW", "BO", "BY", "GT", "XK", "SD", "MZ", "SY", "AL", "MM", "SN", 
                             "BI", "ME", "CD", "GM", "ML", "PG", "TJ", "UZ", "BB", "BJ", "BN", "KP", "MU", "NA", "NC", "CG", "CI", "HT", "PY", "AZ", "GD", "GP", "LI", "RE",
                             "SR", "TG", "BF", "HN", "MC", "MG", "BS", "GY", "KG", "MQ", "NI", "YE", "FO", "KN", "LA", "LR", "MN", "SS", "AF", "AO", "BT", "CW", "GL", "NE",
                             "PF", "SL", "SV", "AW", "AX", "BM", "ER", "GN", "LS", "MV", "SZ", "WS", "GG", "IM", "MR")

chisq.test(location_dat)$expected
fisher.test(location_dat, simulate.p.value = TRUE, B = 1e7)
# p-value = 1e-07


# subdiscipline ####
## one assignment possible
subdis_author_one <- data.frame("OVID" = c(38435, 9013, 8208, 5858, 4811, 4421, 2523, 1193, 1031, 822, 622,
                                           615, 0, 0, 0),
                                "Twitter" = c(2231, 662, 747, 251, 421, 1433, 654, 94, 112, 83, 211, 260,
                                              32, 118, 54)
                                )

chisq.test(subdis_author_one)$expected
fisher.test(subdis_author_one, simulate.p.value = TRUE, B = 1e7)
# p-value = 1e-07



## multiple assignments possible
subdis_author_multiple <- data.frame("OVID" = c(46090, 12311, 10962, 7985, 6484, 7223, 4664, 5375, 1813,
                                                1935, 1055, 1553, 0, 0, 0),
                                     "Twitter" = c(3140, 1657, 1642, 347, 684, 2329, 1293, 203, 225,
                                                   221, 258, 432, 55, 274, 86)
                                     )

chisq.test(subdis_author_multiple)$expected
chisq.test(subdis_author_multiple)
# X-squared = 8196.5, df = 14, p-value < 2.2e-16