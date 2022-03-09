context("Repeated Measures ANOVA")

# Does not test:
#    - type I and type II sum of squares
#    - Contrasts apart from 'repeated'


opts <- options()
on.exit(options(opts))
options(list(
  afex.type = 3,
  afex.set_data_arg = FALSE,
  afex.check_contrasts = TRUE,
  afex.method_mixed = "KR",
  afex.return_aov = "afex_aov",
  afex.es_aov = "ges",
  afex.correction_aov = "GG",
  afex.factorize = TRUE,
  afex.lmer_function = "lmerTest",
  afex.sig_symbols = c(" +", " *", " **", " ***"),
  afex.emmeans_model = c("univariate"),
  afex.include_aov = TRUE
))

initOpts <- function(){
  options <- jaspTools::analysisOptions("AnovaRepeatedMeasures")

  options$repeatedMeasuresFactors <- list(
    list(name = "Drink", levels = c("Beer", "Wine", "Water")),
    list(name = "Imagery", levels = c("Positive", "Neutral", "Negative"))
  )

  options$repeatedMeasuresCells <- c("beerpos", "beerneut", "beerneg",
                                     "winepos", "wineneut", "wineneg",
                                     "waterpos", "waterneu", "waterneg")
  options$withinModelTerms <- list(
    list(components = "Drink"),
    list(components = "Imagery") ,
    list(components = c("Drink", "Imagery"))
  )

  options
}

test_that("Within subjects table results match", {
  options <- initOpts()

  options$sphericityNone <- TRUE
  options$sphericityHuynhFeldt <- TRUE
  options$sphericityGreenhouseGeisser <- TRUE

  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures", dataset = "AnovaRepeatedMeasures.csv",
                            options = options)
  table <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_withinAnovaTable$data

  refTable <- list(1, 1, 1, 1, 1, 1, "TRUE", 5.10598105687077, 1046.17222222222,
                   2092.34444444444, "Drink", "None", 2, 0.0108629307294978, "FALSE",
                   5.10598105687077, 1812.76427443316, 2092.34444444444, "Drink",
                   "Greenhouse-Geisser", 1.15422864073086, 0.0297686804863521,
                   "FALSE", 5.10598105687077, 1770.93939197604, 2092.34444444444,
                   "Drink", "Huynh-Feldt", 1.18148845405137, 0.028813909529067,
                   "TRUE", "", 204.891520467836, 7785.87777777778, "Residuals",
                   "None", 38, "", 0, "", 355.027614525487, 7785.87777777778, "Residuals",
                   "Greenhouse-Geisser", 21.9303441738863, "", 0, "", 346.836263638895,
                   7785.87777777778, "Residuals", "Huynh-Feldt", 22.4482806269761,
                   "", 1, 1, 1, 1, 1, 1, "TRUE", 122.564824909945, 10814.3388888889,
                   21628.6777777778, "Imagery", "None", 2, 2.68019659683571e-17,
                   "FALSE", 122.564824909945, 14468.4903478118, 21628.6777777778,
                   "Imagery", "Greenhouse-Geisser", 1.49488144635967, 1.75728558571484e-13,
                   "FALSE", 122.564824909945, 13571.4963320567, 21628.6777777778,
                   "Imagery", "Huynh-Feldt", 1.59368408969683, 3.14280380271786e-14,
                   "TRUE", "", 88.2336257309941, 3352.87777777778, "Residuals",
                   "None", 38, "", 0, "", 118.047656482539, 3352.87777777778, "Residuals",
                   "Greenhouse-Geisser", 28.4027474808338, "", 0, "", 110.729129193702,
                   3352.87777777778, "Residuals", "Huynh-Feldt", 30.2799977042398,
                   "", "TRUE", 17.1549223629789, 656.105555555556, 2624.42222222222,
                   "Drink <unicode> Imagery", "None", 4, 4.58904028152479e-10,
                   "FALSE", 17.1549223629789, 821.777615847198, 2624.42222222222,
                   "Drink <unicode> Imagery", "Greenhouse-Geisser", 3.19359175963514,
                   1.90024850184092e-08, "FALSE", 17.1549223629789, 670.4615906467,
                   2624.42222222222, "Drink <unicode> Imagery", "Huynh-Feldt",
                   3.91435133471376, 6.80963952075043e-10, "TRUE", "", 38.2459064327485,
                   2906.68888888889, "Residuals", "None", 76, "", 0, "", 47.9033130234755,
                   2906.68888888889, "Residuals", "Greenhouse-Geisser", 60.6782434330676,
                   "", 0, "", 39.0827528367944, 2906.68888888889, "Residuals",
                   "Huynh-Feldt", 74.3726753595615, "")

  jaspTools::expect_equal_tables(table, refTable)
})

test_that("Sphericity Assumptions table match (Field Chapter 8)", {
  options <- initOpts()

  options$sphericityTests <- TRUE

  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures", dataset = "AnovaRepeatedMeasures.csv",
                            options = options)

  # isNewgroup is FALSE now, so changed here
  refTable <- list("Drink", 0.267241056560857, 23.7528754979348, 2, 6.95230186958065e-06,
                   0.577114320365429, 0.590744227025686, 0.5, "FALSE", "Imagery",
                   0.662101262364057, 7.42206186804268, 2, 0.0244523015633462,
                   0.747440723179836, 0.796842044848417, 0.5, "FALSE", "Drink <unicode> Imagery",
                   0.595043993796251, 9.04133890303752, 9, 0.435658665786593, 0.798397939908785,
                   0.97858783367844, 0.25, "FALSE")

  table <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_assumptionsContainer$collection$rmAnovaContainer_assumptionsContainer_sphericityTable$data
  jaspTools::expect_equal_tables(table, refTable)

})

test_that("Post-hoc tests match (Field Chapter 8)", {
  options <- initOpts()

  options$postHocTestsVariables <- list(list(components = "Drink"),
                                        list(components = "Imagery"),
                                        list(components = c("Drink", "Imagery")))
  options$postHocTestEffectSize <- TRUE
  options$postHocTestsBonferroni <- TRUE
  options$postHocTestsHolm <- TRUE

  options$postHocTestPooledError <- FALSE
  options$confidenceIntervalsPostHoc <- TRUE
  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures", dataset = "AnovaRepeatedMeasures.csv",
                            options = options)

  refTable <- list("TRUE", 2.84948954082566, 0.703009687611414, 0.362221443896782,
                   -0.322203183269585, 1.04664607106315, "Beer", "Wine", 3.5, 0.234336562537138,
                   -3.98021184328794, 1.22829017262714, 10.9802118432879, "FALSE",
                   3.3351289023547, 0.0660988675936689, 0.860707145259498, 0.108019449155344,
                   1.61339484136365, "Beer", "Water", 8.31666666666667, 0.0440659117291126,
                   -0.438399936264868, 2.49365674016224, 17.0717332695982, "TRUE",
                   1.1164571680934, 0.00112213065327869, 0.498485701362716, -0.199590871891703,
                   1.19656227461713, "Wine", "Water", 4.81666666666667, 0.00112213065327869,
                   1.88584835284471, 4.31424223366509, 7.74748498048862)

  table <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_postHocStandardContainer$collection[[1]]$data
  jaspTools::expect_equal_tables(table, refTable)


  refTable <- list("TRUE", 1.11255461788524, 8.64627761186188e-10, 1.37299175877066,
                   0.670396659452184, 2.07558685808914, "Positive", "Neutral",
                   13.2666666666667, 5.76418507457459e-10, 10.3460929604728, 11.9245082024684,
                   16.1872403728605, "FALSE", 1.91462133431161, 5.36180351283324e-11,
                   2.77875593389389, 1.99539484787656, 3.56211701991122, "Positive",
                   "Negative", 26.85, 5.36180351283324e-11, 21.8239162137161, 14.0236607201777,
                   31.8760837862839, "TRUE", 1.97985098972636, 4.54655986206157e-06,
                   1.40576417512323, 0.956611951019807, 1.85491639922665, "Neutral",
                   "Negative", 13.5833333333333, 1.51551995402052e-06, 8.38601479290266,
                   6.86078568731614, 18.780651873764)

  table <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_postHocStandardContainer$collection[[3]]$data
  jaspTools::expect_equal_tables(table, refTable)


  refTable <- list("TRUE", 3.06258786722021, 1, -0.445014916787477, -1.50992951200853,
                   0.619899678433579, "Beer, Positive", "Wine, Positive", -4.30000000000001,
                   0.824625255450165, -14.5169752077379, -1.40404134882926, 5.91697520773788,
                   "FALSE", 3.06258786722021, 1, 0.377745220063787, -0.679804865526276,
                   1.43529530565385, "Beer, Positive", "Water, Positive", 3.64999999999999,
                   0.950172562866207, -6.5669752077379, 1.1918025402853, 13.8669752077379,
                   "FALSE", 2.34325584459239, 0.000293009660554563, 1.1435848443027,
                   0.143343188082501, 2.1438265005229, "Beer, Positive", "Beer, Neutral",
                   11.05, 0.000146504830277281, 3.33452659612508, 4.71566091491908,
                   18.7654734038749, "FALSE", 3.32350744762006, 0.211308693316513,
                   0.972823306465645, -0.266900906542999, 2.21254751947429, "Beer, Positive",
                   "Wine, Neutral", 9.4, 0.0704362311055042, -1.59721324755597,
                   2.82833727564874, 20.397213247556, "FALSE", 3.32350744762006,
                   8.80568662674528e-06, 1.93529742881995, 0.409715528435423, 3.46087932920448,
                   "Beer, Positive", "Water, Neutral", 18.7, 5.13665053226808e-06,
                   7.70278675244403, 5.62658585687568, 29.697213247556, "FALSE",
                   2.34325584459239, 8.29767861995479e-09, 1.71796456248188, 0.507729351375581,
                   2.92819977358819, "Beer, Positive", "Beer, Negative", 16.6,
                   6.68424111051914e-09, 8.88452659612507, 7.08416028847572, 24.3154734038749,
                   "FALSE", 3.32350744762006, 3.19971300390688e-14, 3.42040534879676,
                   2.18858427557457, 4.65222642201896, "Beer, Positive", "Wine, Negative",
                   33.05, 2.84418933680612e-14, 22.052786752444, 9.94431350640328,
                   44.047213247556, "FALSE", 3.32350744762006, 1.52526279020822e-12,
                   3.13062819367934, 1.82893799648637, 4.43231839087231, "Beer, Positive",
                   "Water, Negative", 30.25, 1.31342073601263e-12, 19.252786752444,
                   9.10183006259302, 41.247213247556, "TRUE", 3.06258786722021,
                   0.417051696818704, 0.822760136851264, -0.303868326258607, 1.94938859996113,
                   "Wine, Positive", "Water, Positive", 7.95, 0.127432462916826,
                   -2.26697520773789, 2.59584388911457, 18.1669752077379, "FALSE",
                   3.32350744762006, 0.000505451688385996, 1.58859976109018, 0.180635601431827,
                   2.99656392074853, "Wine, Positive", "Beer, Neutral", 15.35,
                   0.000238685519515609, 4.35278675244406, 4.61861459374555, 26.347213247556,
                   "FALSE", 2.34325584459239, 2.46937758318436e-06, 1.41783822325312,
                   0.322906394249917, 2.51277005225633, "Wine, Positive", "Wine, Neutral",
                   13.7, 1.57765790036778e-06, 5.98452659612508, 5.84656602121189,
                   21.4154734038749, "FALSE", 3.32350744762006, 3.19644183160535e-08,
                   2.38031234560743, 0.686268083845554, 4.07435660736931, "Wine, Positive",
                   "Water, Neutral", 23, 2.48612142458194e-08, 12.0027867524441,
                   6.92039971701287, 33.997213247556, "FALSE", 3.32350744762006,
                   5.19878021666088e-07, 2.16297947926936, 0.553344410866276, 3.77261454767245,
                   "Wine, Positive", "Beer, Negative", 20.9, 3.61026403934783e-07,
                   9.90278675244405, 6.28853713415517, 31.897213247556, "FALSE",
                   2.34325584459239, 3.3906654401086e-27, 3.86542026558424, 2.78364489877289,
                   4.9471956323956, "Wine, Positive", "Wine, Negative", 37.35,
                   3.3906654401086e-27, 29.6345265961251, 15.9393606490704, 45.0654734038749,
                   "FALSE", 3.32350744762006, 4.0893609820243e-15, 3.57564311046682,
                   2.14086462895077, 5.01042159198286, "Wine, Positive", "Water, Negative",
                   34.55, 3.74858090018894e-15, 23.5527867524441, 10.3956439227302,
                   45.547213247556, "TRUE", 3.32350744762006, 1, 0.765839624238914,
                   -0.432192582952464, 1.96387183143029, "Water, Positive", "Beer, Neutral",
                   7.40000000000002, 0.229581612565895, -3.59721324755594, 2.22656338721284,
                   18.397213247556, "FALSE", 3.32350744762006, 1, 0.595078086401858,
                   -0.575242164605178, 1.76539833740889, "Water, Positive", "Wine, Neutral",
                   5.75000000000001, 0.524074945350722, -5.24721324755595, 1.73009992925322,
                   16.747213247556, "FALSE", 2.34325584459239, 1.82822037928189e-07,
                   1.55755220875617, 0.41026074847123, 2.7048436690411, "Water, Positive",
                   "Water, Neutral", 15.05, 1.32038138503692e-07, 7.33452659612507,
                   6.42268749045539, 22.7654734038749, "FALSE", 3.32350744762006,
                   0.00710816778967513, 1.3402193424181, 0.00720948750754924, 2.67322919732865,
                   "Water, Positive", "Beer, Negative", 12.95, 0.00276428747376255,
                   1.95278675244405, 3.89648592762247, 23.947213247556, "FALSE",
                   3.32350744762006, 4.94431770017089e-12, 3.04266012873298, 1.87739405894888,
                   4.20792619851708, "Water, Positive", "Wine, Negative", 29.4,
                   4.12026475014241e-12, 18.4027867524441, 8.84607616000776, 40.397213247556,
                   "FALSE", 2.34325584459239, 6.66991226252023e-18, 2.75288297361555,
                   1.83935544468238, 3.66641050254872, "Water, Positive", "Water, Negative",
                   26.6, 6.48463692189467e-18, 18.8845265961251, 11.3517267273165,
                   34.3154734038749, "TRUE", 3.06258786722021, 1, -0.170761537837056,
                   -1.21304210234721, 0.871519026673097, "Beer, Neutral", "Wine, Neutral",
                   -1.65000000000002, 1, -11.8669752077379, -0.538760052457745,
                   8.56697520773787, "FALSE", 3.06258786722021, 0.538381439939075,
                   0.791712584517252, -0.328615669436179, 1.91204083847068, "Beer, Neutral",
                   "Water, Neutral", 7.64999999999998, 0.149550399983076, -2.56697520773791,
                   2.49788751594043, 17.8669752077379, "FALSE", 2.34325584459239,
                   0.714895488198306, 0.574379718179183, -0.276687012357518, 1.42544644871589,
                   "Beer, Neutral", "Beer, Negative", 5.54999999999999, 0.178723872049576,
                   -2.16547340387494, 2.36849937355663, 13.2654734038749, "FALSE",
                   3.32350744762006, 1.21708122834007e-07, 2.27682050449406, 1.1446810293211,
                   3.40895997966703, "Beer, Neutral", "Wine, Negative", 22, 9.12810921255052e-08,
                   11.002786752444, 6.61951277279492, 32.9972132475559, "FALSE",
                   3.32350744762006, 4.67520816749833e-06, 1.98704334937664, 0.859401431142084,
                   3.11468526761119, "Beer, Neutral", "Water, Negative", 19.2,
                   2.85707165791565e-06, 8.20278675244404, 5.77702932898466, 30.197213247556,
                   "TRUE", 3.06258786722021, 0.122628414872502, 0.962474122354309,
                   -0.194994185935976, 2.11994243064459, "Wine, Neutral", "Water, Neutral",
                   9.3, 0.0442824831484033, -0.916975207737885, 3.03664756839817,
                   19.5169752077379, "FALSE", 3.32350744762006, 1, 0.74514125601624,
                   -0.449198983578502, 1.93948149561098, "Wine, Neutral", "Beer, Negative",
                   7.20000000000001, 0.232136379173532, -3.79721324755596, 2.16638599836925,
                   18.197213247556, "FALSE", 2.34325584459239, 3.28810752994369e-15,
                   2.44758204233112, 1.65290463197709, 3.24225945268515, "Wine, Neutral",
                   "Wine, Negative", 23.65, 3.10543488939126e-15, 15.9345265961251,
                   10.0927946278585, 31.3654734038749, "FALSE", 3.32350744762006,
                   5.55054933336568e-07, 2.15780488721369, 1.02299342575276, 3.29261634867463,
                   "Wine, Neutral", "Water, Negative", 20.85, 3.70036622224379e-07,
                   9.85278675244406, 6.27349278694428, 31.847213247556, "TRUE",
                   3.32350744762006, 1, -0.217332866338069, -1.35001992801137,
                   0.915354195335231, "Water, Neutral", "Beer, Negative", -2.09999999999999,
                   1, -13.097213247556, -0.631862582857695, 8.89721324755597, "FALSE",
                   3.32350744762006, 0.00156701471652034, 1.48510791997681, 0.239587156715425,
                   2.7306286832382, "Water, Neutral", "Wine, Negative", 14.35,
                   0.000652922798550143, 3.35278675244405, 4.3177276495276, 25.347213247556,
                   "FALSE", 2.34325584459239, 0.000123837133230087, 1.19533076485938,
                   0.316083837451252, 2.07457769226752, "Water, Neutral", "Water, Negative",
                   11.55, 6.53584869825456e-05, 3.83452659612509, 4.92903923686112,
                   19.2654734038749, "TRUE", 3.06258786722021, 3.8089199979706e-05,
                   1.70244078631488, 0.584166390274611, 2.82071518235515, "Beer, Negative",
                   "Wine, Negative", 16.45, 2.11606666553922e-05, 6.23302479226211,
                   5.37127446238171, 26.6669752077379, "FALSE", 3.06258786722021,
                   0.00116683843484595, 1.41266363119745, 0.341980766799286, 2.48334649559562,
                   "Beer, Negative", "Water, Negative", 13.65, 0.000518594859931535,
                   3.43302479226212, 4.45701497942312, 23.8669752077379, "TRUE",
                   3.06258786722021, 1, -0.289777155117426, -1.3394573179511, 0.759903007716252,
                   "Wine, Negative", "Water, Negative", -2.79999999999999, 1, -13.0169752077379,
                   -0.914259482958587, 7.41697520773789)

  table <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_postHocStandardContainer$collection[[2]]$data
  jaspTools::expect_equal_tables(table, refTable)

})

test_that("Descriptives Match", {
  options <- initOpts()

  options$descriptives <- TRUE

  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures", dataset = "AnovaRepeatedMeasures.csv",
                            options = options)

  refTable <- list(4.45, 20, 17.3037111930543, "Beer", "Negative", 10, 20, 10.295630140987,
                   "Beer", "Neutral", 21.05, 20, 13.0079934938807, "Beer", "Positive",
                   -9.2, 20, 6.8024763292882, "Water", "Negative", 2.35, 20, 6.83855170878193,
                   "Water", "Neutral", 17.4, 20, 7.07404447704126, "Water", "Positive",
                   -12, 20, 6.18146635643918, "Wine", "Negative", 11.65, 20, 6.24310145596511,
                   "Wine", "Neutral", 25.35, 20, 6.73775692801786, "Wine", "Positive"
  )

  table <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_descriptivesContainer$collection$rmAnovaContainer_descriptivesContainer_tableDescriptives$data
  jaspTools::expect_equal_tables(table, refTable)
})

test_that("Field - Chapter 8 marginal means match", {

  # compared to SPSS, we pool the standard errors in marginal means
  options <- initOpts()

  options$marginalMeansTerms <- options$withinModelTerms[3]
  options$marginalMeansBootstrapping <- TRUE
  options$marginalMeansBootstrappingReplicates <- 500
  set.seed(1)
  results <- jaspTools::runAnalysis("AnovaRepeatedMeasures", dataset = "AnovaRepeatedMeasures.csv",
                            options = options)

  table <- results$results$rmAnovaContainer$collection$rmAnovaContainer_marginalMeansContainer$collection[[1]]$data

  refTable <- list("TRUE", "Beer", "Positive", 2.86790158635509, -0.0460999999999991,
                   15.2090610346416, 21.175, 26.25, "FALSE", "Wine", "Positive",
                   1.41905772950904, -0.0318999999999932, 22.75, 25.175, 28.4027564717744,
                   "FALSE", "Water", "Positive", 1.58643387977874, -0.0897000000000112,
                   14.75, 17.25, 21.6001985182309, "TRUE", "Beer", "Neutral", 2.34101761568638,
                   0.111500000000007, 4.70000000000001, 10.15, 14.65, "FALSE",
                   "Wine", "Neutral", 1.39196783688339, -0.0412999999999979, 8.80000000000002,
                   11.625, 14.2593188494378, "FALSE", "Water", "Neutral", 1.52816109840729,
                   -0.0161000000000051, -0.999999999999996, 2.35000000000001, 5.19101856310838,
                   "TRUE", "Beer", "Negative", 3.86263448961985, 0.156300000000011,
                   -3.11760706046903, 4.70000000000001, 11.5528285326562, "FALSE",
                   "Wine", "Negative", 1.35802704867387, 0.0551000000000048, -14.6,
                   -12, -9.21860910945718, "FALSE", "Water", "Negative", 1.4412391684524,
                   0.0620000000000118, -12.1435538334574, -9.14999999999998, -6.44999999999998
  )

  jaspTools::expect_equal_tables(table, refTable)
})

# Error handling
test_that("Analysis handles errors", {

  options <- jaspTools::analysisOptions("AnovaRepeatedMeasures")
  options[["betweenModelTerms"]] <- list(list(components = "Celebrity"))
  options[["betweenSubjectFactors"]] <- "Celebrity"
  options[["repeatedMeasuresCells"]] <- c("Stick Insect", "Kangaroo Testicle", "Fish Eye", "Witchetty Grub")
  options[["repeatedMeasuresFactors"]] <- list(list(
    levels = c("Stick Insect", "Kangaroo Testicle", "Fish Eye", "Witchetty Grub"),
    name = "Animal"
  ))

  results <- jaspTools::runAnalysis(options = options, dataset = "Bush Tucker Food.csv")
  expect_identical(results$status, "validationError", label = "Duplicate variables in subject and betweenSubjectFactors")

})

# Mixed Effects
initOpts <- function(){
  options <- jaspTools::analysisOptions("AnovaRepeatedMeasures")

  options$repeatedMeasuresFactors <- list(
    list(name = "Looks", levels = c("Attractive", "Average" , "Ugly")),
    list(name = "Charisma", levels = c("High", "Some", "None"))
  )

  options$repeatedMeasuresCells <- c("att_high", "att_some", "att_none",
                                     "av_high", "av_some", "av_none",
                                     "ug_high", "ug_some", "ug_none")
  options$withinModelTerms <- list(
    list(components = "Looks"),
    list(components = "Charisma"),
    list(components = c("Looks", "Charisma"))
  )

  options$betweenSubjectFactors <- "gender"
  options$betweenModelTerms <- list(
    list(components = "gender")
  )
  options
}

test_that("Between Subjects table match", {
  options <- initOpts()
  options$sphericityCorrections <- TRUE
  options$sphericityTests <- TRUE
  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv", options = options)

  refTable <- list("gender", 0.200000000000001, 1, 0.200000000000001, 0.00473545746857648,
                0.945895847556855, TRUE, "Residuals", 760.222222222222, 18, "", "",
                42.2345679012346, TRUE)

  table <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_betweenTable$data
  jaspTools::expect_equal_tables(table, refTable)
})

test_that("Homogeneity tests correct", {
  options <- initOpts()

  options$homogeneityTests <- TRUE

  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv", options = options)

  refTable <- list("att_high", 1.13105200239091, 1, 18, 0.301611198987337, "TRUE",
                "att_some", 0.598562976996908, 1, 18, 0.449169168742317, "FALSE",
                "att_none", 1.94893878806521, 1, 18, 0.179682774529315, "FALSE",
                "av_high", 0.101977401129945, 1, 18, 0.753145830077659, "FALSE",
                "av_some", 1.76314835904338, 1, 18, 0.200826123727507, "FALSE",
                "av_none", 0.00399511707912524, 1, 18, 0.950298338730636, "FALSE",
                "ug_high", 0.00491266375545877, 1, 18, 0.944894541517532, "FALSE",
                "ug_some", 0.123626373626372, 1, 18, 0.729216564281406, "FALSE",
                "ug_none", 0.0819838056680181, 1, 18, 0.777896246470082, "FALSE")

  table <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_assumptionsContainer$collection$rmAnovaContainer_assumptionsContainer_rmAnovaLevenesTable$data
  jaspTools::expect_equal_tables(table, refTable)
})



test_that("Contrast table match", {
  options <- initOpts()

  options$contrasts <- list(list(contrast = "repeated", variable = "Looks"),
                            list(contrast = "difference", variable = "Charisma"))

  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv", options = options)

  # Difference contrast
  refTable <- list("Some - High", 1.08612650363252, 36, -12.8, 6.48261408516436e-14,
                   -11.7849992217212, "None - High, Some", 0.940613143869335, 36,
                   -21.4, 5.89263452901201e-23, -22.7511173317952)
  table <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_contrastContainer$collection[[1]]$collection[[1]]$data
  jaspTools::expect_equal_tables(table, refTable)

  # Repeated contrast
  refTable <- list("Attractive - Average", 14.31667, 0.9040603, 15.83596, 36,
                   8.431449e-18, "Average - Ugly", 11.96667, 0.9040603, 36,
                   13.23658, 2.1268e-15)
  table <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_contrastContainer$collection[[2]]$collection[[1]]$data
  jaspTools::expect_equal_tables(table, refTable)
})

test_that("Descriptives Plots match", {
  options <- initOpts()
  options$sphericityCorrections <- TRUE
  options$sphericityTests <- TRUE
  options$plotHorizontalAxis <- "Charisma"
  options$plotSeparateLines <- "gender"
  options$plotErrorBars <- TRUE

  options$usePooledStandErrorCI <- FALSE
  options$errorBarType <- "confidenceInterval"
  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv", options = options)
  descPlot <-  results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(descPlot, "mixedRMANOVA1")

  options$usePooledStandErrorCI <- TRUE
  options$errorBarType <- "confidenceInterval"
  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv", options = options)
  descPlot <-  results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(descPlot, "mixedRMANOVA2")

  options$usePooledStandErrorCI <- FALSE
  options$errorBarType <- "standardError"
  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv", options = options)
  descPlot <-  results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(descPlot, "mixedRMANOVA3")

  options$usePooledStandErrorCI <- TRUE
  options$errorBarType <- "standardError"
  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv", options = options)
  descPlot <-  results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(descPlot, "mixedRMANOVA4")

})

test_that("Raincloud Plots match", {
  options <- initOpts()
  options$sphericityCorrections <- TRUE
  options$sphericityTests <- TRUE
  options$rainCloudPlotsHorizontalAxis <- "Charisma"
  options$rainCloudPlotsSeparatePlots <- "gender"
  set.seed(1)
  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv", options = options)
  testPlot <-  results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(testPlot, "raincloud-plots")
})

test_that("Raincloud Plots match for between subjects", {
  options <- initOpts()
  options$sphericityCorrections <- TRUE
  options$sphericityTests <- TRUE
  options$rainCloudPlotsHorizontalAxis <- "gender"
  options$rainCloudPlotsSeparatePlots <- "Charisma"
  set.seed(1)
  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                                    dataset = "AnovaMixedEffects.csv", options = options)
  testPlot <-  results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(testPlot, "raincloud-plots-between-subjects")
})


test_that("Effect Size Calculation correct", {
  options <- jaspTools::analysisOptions("AnovaRepeatedMeasures")

  options$repeatedMeasuresFactors <- list(
    list(name = "Animal", levels = c("Stick", "Kangaroo", "Fish", "Grub"))
    )
  options$repeatedMeasuresCells <- c("Stick Insect", "Kangaroo Testicle",
                                     "Fish Eye", "Witchetty Grub")

  options$withinModelTerms <- list(
    list(components = "Animal")
  )

  options$effectSizeEstimates <- TRUE
  options$effectSizeEtaSquared <- TRUE
  options$effectSizePartialEtaSquared <- TRUE
  options$effectSizeOmegaSquared <- TRUE
  options$effectSizeGenEtaSquared <- TRUE

  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaRepeatedMeasuresOneWay.csv",
                            options = options)

  refTable <- list(1, 1, 1, 1, 1, 1, "TRUE", 3.79380603096984, 27.7083333333333,
                   83.1249999999999, "Animal", 0.351479915433404, 0.351479915433404,
                   0.327424913835549, 3, 0.238785176929506, 0.0255702968630395,
                   "TRUE", "", 7.30357142857143, 153.375, "Residuals", "", "",
                   "", 21, "", "")

  table <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_withinAnovaTable$data
  jaspTools::expect_equal_tables(table, refTable)
})


test_that("Simple Effects table match", {
  options <- initOpts()

  options$betweenSubjectFactors <- "gender"
  options$betweenModelTerms <- list(
    list(components = "gender")
  )

  options$simpleFactor <- "Looks"
  options$moderatorFactorOne <- "gender"
  options$moderatorFactorTwo <- "Charisma"

  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv",
                            options = options)

  refTable <- list("Female", "High", 42.4666666666668, 2, 21.2333333333334, 0.639629588307488,
                   0.539062933641058, "TRUE", "Female", "Some", 6444.46666666667, 2,
                   3222.23333333334, 105.034770010866, 1.18808350406329e-10, "FALSE",
                   "Female", "None", 187.8, 2, 93.8999999999999, 10.1696750902527,
                   0.0011082808185639, "FALSE", "Male", "High", 5661.66666666667, 2,
                   2830.83333333333, 82.5850891410049, 8.54593593608342e-10, "TRUE",
                   "Male", "Some", 8157.26666666666, 2 ,4078.63333333333, 121.267591674926,
                   3.58637028279497e-11, "FALSE", "Male", "None", 10955, 2, 5477.5,
                   292.566765578635, 1.87815435905324e-14, "FALSE")

  table <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_simpleEffectsContainer$collection[[1]]$data
  jaspTools::expect_equal_tables(table, refTable)
})

test_that("Nonparametric table match", {

  # Kendall W now also verified by case presented in
  # https://github.com/jasp-stats/jasp-issues/issues/1473
  options <- initOpts()

  options$betweenSubjectFactors <- "gender"
  options$betweenModelTerms <- list(
    list(components = "gender")
  )

  options$friedmanWithinFactor <- "Charisma"

  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv",
                            options = options)

  refTable <- list("Charisma", 40.074508162411, 2, 0.539957264957265, 1.98577994376659e-09)

  table <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_nonparametricContainer$collection[[1]]$data
  jaspTools::expect_equal_tables(table, refTable)
})



test_that("Conover table match", {

  options <- initOpts()

  options$betweenSubjectFactors <- "gender"
  options$betweenModelTerms <- list(
    list(components = "gender")
  )

  options$conoverTest <- TRUE

  options$friedmanWithinFactor <- "Charisma"

  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv",
                            options = options)

  refTable <- list( "High", "Some", 1.31809226918034, 406, 306.5, 0.189380510753162, 0.568141532259486,
                    0.233862092301299, 158, "High", "None", 2.89450412880305, 406, 187.5,
                    0.00433496034546402, 0.0130048810363921, 0.0130048810363921, 158, "Some",
                    "None", 1.57641185962271, 306.5, 187.5, 0.116931046150649, 0.350793138451948,
                    0.233862092301299, 158)

  table <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_nonparametricContainer$collection$rmAnovaContainer_nonparametricContainer_conoverContainer$collection$rmAnovaContainer_nonparametricContainer_conoverContainer_Charisma$data
  jaspTools::expect_equal_tables(table, refTable)
})

# Andy Field tests ----
# should we put this in verification?
test_that("Field - Chapter 8 results match", {
  options <- jaspTools::analysisOptions("AnovaRepeatedMeasures")

  options$repeatedMeasuresFactors <- list(
    list(name = "Animal", levels = c("Stick", "Kangaroo", "Fish", "Grub"))
  )
  options$repeatedMeasuresCells <- c("Stick Insect", "Kangaroo Testicle",
                                     "Fish Eye", "Witchetty Grub")

  options$withinModelTerms <- list(
    list(components = "Animal")
  )

  options$sphericityTests <- TRUE
  options$sphericityNone <- TRUE
  options$sphericityHuynhFeldt <- TRUE
  options$sphericityGreenhouseGeisser <- TRUE

  options$postHocTestsVariables <- "Animal"
  options$postHocTestPooledError <- FALSE
  options$postHocTestsBonferroni <- TRUE
  options$confidenceIntervalsPostHoc <- TRUE
  options$postHocTestEffectSize <- TRUE

  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaRepeatedMeasuresOneWay.csv",
                            options = options)

  # output 2 (chi square and df for sphericity)
  # changed isnewgroup
  table <- results$results$rmAnovaContainer$collection$rmAnovaContainer_assumptionsContainer$collection[[1]]$data
  refTable <- list("Animal", 0.136248029372535, 11.4059814340564, 5, 0.0468458123067897,
                   0.532845552798473, 0.66576361409737, 0.333333333333333, "FALSE")

  jaspTools::expect_equal_tables(table, refTable)

  # sphericity corrections
  table <- results$results$rmAnovaContainer$collection$rmAnovaContainer_withinAnovaTable$data
  refTable <- list(1, 1, 1, 1, 1, 1, "TRUE", 3.79380603096984, 27.7083333333333,
                   83.1249999999999, "Animal", "None", 3, 0.0255702968630395, "FALSE",
                   3.79380603096984, 52.0006842279357, 83.1249999999999, "Animal",
                   "Greenhouse-Geisser", 1.59853665839542, 0.0625841206869634,
                   "FALSE", 3.79380603096984, 41.6188760494215, 83.1249999999999,
                   "Animal", "Huynh-Feldt", 1.99729084229211, 0.0483306135519432,
                   "TRUE", "", 7.30357142857143, 153.375, "Residuals", "None",
                   21, "", 0, "", 13.7067324484806, 153.375, "Residuals", "Greenhouse-Geisser",
                   11.1897566087679, "", 0, "", 10.9702171670548, 153.375, "Residuals",
                   "Huynh-Feldt", 13.9810358960448, "")

  jaspTools::expect_equal_tables(table, refTable)

  # post hoc tests (bonferroni, confidence intervals, se not pooled)
  table <- results$results$rmAnovaContainer$collection$rmAnovaContainer_postHocStandardContainer$collection[[1]]$data
  refTable <- list("TRUE", 0.811469126250126, 0.0121396972553231, 1.56917133617844,
                   -0.405503836463409, 3.5438465088203, "Stick", "Kangaroo", 3.875,
                   0.0101164143794359, 0.924654528093315, 4.77528950227193, 6.82534547190668,
                   "FALSE", 0.7319250547114, 0.00564485983507568, 1.61978976637775,
                   -0.37869346860645, 3.61827300136195, "Stick", "Fish", 4, 0.00564485983507568,
                   1.33886145376625, 5.46504040851179, 6.66113854623375, "FALSE",
                   1.79222029098785, 1, 0.961750173786788, -0.769854396896755,
                   2.69335474447033, "Stick", "Grub", 2.375, 0.906918466006362,
                   -4.14116783575007, 1.32517191772833, 8.89116783575007, "TRUE",
                   1.20174723988509, 1, 0.0506184301993047, -1.51715763808039,
                   1.618394498479, "Kangaroo", "Fish", 0.125, 0.920074728368496,
                   -4.24432153408686, 0.104015217053423, 4.49432153408686, "FALSE",
                   1.33630620956212, 1, -0.607421162391656, -2.24224022162526,
                   1.02739789684195, "Kangaroo", "Grub", -1.5, 0.906918466006362,
                   -6.3585520347291, -1.12249721603218, 3.3585520347291, "TRUE",
                   1.82186619392628, 1, -0.658039592590961, -2.30429909272349,
                   0.988219907541572, "Fish", "Grub", -1.625, 0.906918466006362,
                   -8.24895462968414, -0.891942561653216, 4.99895462968414)

  jaspTools::expect_equal_tables(table, refTable)
})


test_that("Field - Chapter 9 match",  {
  options <- initOpts()

  options$sphericityTests <- TRUE

  options$marginalMeansTerms <- list(
    list(components = "Charisma"),
    list(components = "Looks"),
    list(components = "gender"),
    list(components = c("Looks", "Charisma")),
    list(components = c("Looks", "gender")),
    list(components = c("Charisma", "gender")),
    list(components = c("Charisma", "Looks", "gender"))
  )

  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv",
                            options = options)

  # sphericity
  # changed isnewgroup because automitically first row is TRUE
  table <- results$results$rmAnovaContainer$collection$rmAnovaContainer_assumptionsContainer$collection[[1]]$data
  refTable <- list("Looks", 0.960205401636332, 0.690336975266099, 2, 0.708101037148711,
                   0.961728404411513, 1, 0.5, "FALSE", "Charisma", 0.929329811889938,
                   1.24595694485668, 2, 0.536344568684782, 0.933994437414187, 1,
                   0.5, "FALSE", "Looks <unicode> Charisma", 0.613354486153378,
                   8.02466743180245, 9, 0.533938195513754, 0.799354278085918, 0.992241110561882,
                   0.25, "FALSE")

  jaspTools::expect_equal_tables(table, refTable)

  # marginal charisma
  table <- results$results$rmAnovaContainer$collection$rmAnovaContainer_marginalMeansContainer$collection[[1]]$data
  refTable <- list("High", 82.1000000000002, 0.792376225226709, 80.5111143833479,
                   83.6888856166524, "TRUE", "Some", 69.3000000000002, 0.792376225226709,
                   67.7111143833479, 70.8888856166524, "FALSE", "None", 54.3000000000002,
                   0.792376225226709, 52.7111143833479, 55.8888856166524, "FALSE")

  jaspTools::expect_equal_tables(table, refTable)

  # gender (strategy) * looks interaction
  table <- results$results$rmAnovaContainer$collection$rmAnovaContainer_marginalMeansContainer$collection[[6]]$data
  refTable <- list("Female", "Attractive", 76.1666666666668, 1.00705331467645, 74.1441569164011,
                   78.1891764169326, "TRUE", "Female", "Average", 68.1000000000002,
                   1.00705331467645, 66.0774902497344, 70.1225097502659, "FALSE",
                   "Female", "Ugly", 61.3333333333335, 1.00705331467645, 59.3108235830677,
                   63.3558430835992, "FALSE", "Male", "Attractive", 88.0333333333335,
                   1.00705331467645, 86.0108235830677, 90.0558430835993, "TRUE",
                   "Male", "Average", 67.4666666666668, 1.00705331467645, 65.4441569164011,
                   69.4891764169326, "FALSE", "Male", "Ugly", 50.3000000000002,
                   1.00705331467645, 48.2774902497344, 52.3225097502659, "FALSE")

  jaspTools::expect_equal_tables(table, refTable)

  # gender (strategy) * charisma interaction
  table <- results$results$rmAnovaContainer$collection$rmAnovaContainer_marginalMeansContainer$collection[[3]]$data
  refTable <- list("Female", "High", 88.2333333333335, 1.12058920421761, 85.9863097452043,
                   90.4803569214627, "TRUE", "Female", "Some", 69.0666666666668,
                   1.12058920421761, 66.8196430785377, 71.313690254796, "FALSE",
                   "Female", "None", 48.3000000000002, 1.12058920421761, 46.052976411871,
                   50.5470235881293, "FALSE", "Male", "High", 75.9666666666669,
                   1.12058920421761, 73.7196430785377, 78.213690254796, "TRUE",
                   "Male", "Some", 69.5333333333335, 1.12058920421761, 67.2863097452043,
                   71.7803569214627, "FALSE", "Male", "None", 60.3000000000002,
                   1.12058920421761, 58.052976411871, 62.5470235881293, "FALSE")

  jaspTools::expect_equal_tables(table, refTable)

  # looks * charisma interaction
  table <- results$results$rmAnovaContainer$collection$rmAnovaContainer_marginalMeansContainer$collection[[5]]$data
  refTable <- list("TRUE", "High", "Attractive", 1.23097873335623, 86.5185279595704,
                   88.9500000000002, 91.38147204043, "FALSE", "High", "Average",
                   1.23097873335623, 83.1685279595704, 85.6000000000002, 88.0314720404299,
                   "FALSE", "High", "Ugly", 1.23097873335623, 69.3185279595704,
                   71.7500000000002, 74.1814720404299, "TRUE", "Some", "Attractive",
                   1.23097873335623, 85.3685279595704, 87.8000000000002, 90.2314720404299,
                   "FALSE", "Some", "Average", 1.23097873335623, 67.9185279595704,
                   70.3500000000002, 72.7814720404299, "FALSE", "Some", "Ugly",
                   1.23097873335623, 47.3185279595704, 49.7500000000002, 52.1814720404299,
                   "TRUE", "None", "Attractive", 1.23097873335623, 67.1185279595704,
                   69.5500000000002, 71.9814720404299, "FALSE", "None", "Average",
                   1.23097873335623, 44.9685279595704, 47.4000000000001, 49.8314720404299,
                   "FALSE", "None", "Ugly", 1.23097873335623, 43.5185279595704,
                   45.9500000000002, 48.3814720404299)
  jaspTools::expect_equal_tables(table, refTable)

  # gender (strategy) * looks * charisma interaction
  table <- results$results$rmAnovaContainer$collection$rmAnovaContainer_marginalMeansContainer$collection[[2]]$data
  refTable <- list("Female", "Attractive", "High", 89.6000000000002, 1.74086681970523,
                   86.1613792638934, 93.0386207361069, "TRUE", "Female", "Attractive",
                   "Some", 87.1000000000002, 1.74086681970523, 83.6613792638934,
                   90.5386207361069, "FALSE", "Female", "Attractive", "None", 51.8000000000002,
                   1.74086681970523, 48.3613792638934, 55.2386207361069, "FALSE",
                   "Female", "Average", "High", 88.4000000000002, 1.74086681970523,
                   84.9613792638934, 91.8386207361069, "TRUE", "Female", "Average",
                   "Some", 68.9000000000002, 1.74086681970523, 65.4613792638934,
                   72.3386207361069, "FALSE", "Female", "Average", "None", 47.0000000000001,
                   1.74086681970523, 43.5613792638934, 50.4386207361069, "FALSE",
                   "Female", "Ugly", "High", 86.7000000000001, 1.74086681970523,
                   83.2613792638934, 90.1386207361069, "TRUE", "Female", "Ugly",
                   "Some", 51.2000000000001, 1.74086681970523, 47.7613792638934,
                   54.6386207361069, "FALSE", "Female", "Ugly", "None", 46.1000000000002,
                   1.74086681970523, 42.6613792638934, 49.5386207361069, "FALSE",
                   "Male", "Attractive", "High", 88.3000000000002, 1.74086681970523,
                   84.8613792638934, 91.738620736107, "TRUE", "Male", "Attractive",
                   "Some", 88.5000000000002, 1.74086681970524, 85.0613792638934,
                   91.9386207361069, "FALSE", "Male", "Attractive", "None", 87.3000000000002,
                   1.74086681970524, 83.8613792638934, 90.738620736107, "FALSE",
                   "Male", "Average", "High", 82.8000000000002, 1.74086681970523,
                   79.3613792638934, 86.238620736107, "TRUE", "Male", "Average",
                   "Some", 71.8000000000002, 1.74086681970523, 68.3613792638934,
                   75.2386207361069, "FALSE", "Male", "Average", "None", 47.8000000000001,
                   1.74086681970523, 44.3613792638934, 51.2386207361069, "FALSE",
                   "Male", "Ugly", "High", 56.8000000000002, 1.74086681970523,
                   53.3613792638934, 60.2386207361069, "TRUE", "Male", "Ugly",
                   "Some", 48.3000000000002, 1.74086681970523, 44.8613792638934,
                   51.7386207361069, "FALSE", "Male", "Ugly", "None", 45.8000000000002,
                   1.74086681970523, 42.3613792638934, 49.2386207361069, "FALSE")

  jaspTools::expect_equal_tables(table, refTable)
})

# test model without interaction effect
options <- analysisOptions("AnovaRepeatedMeasures")
options$contrasts <- list(list(contrast = "none", variable = "Drink"))
options$customContrasts <- list()
options$labelYAxis <- "Alcohol Attitudes"
options$moderatorFactorOne <- "Drink"
options$plotErrorBars <- TRUE
options$plotHorizontalAxis <- "Drink"
options$plotSeparateLines <- "Imagery"
options$postHocTestPooledError <- FALSE
options$postHocTestsBonferroni <- TRUE
options$postHocTestsHolm <- FALSE
options$rainCloudPlotsHorizontalAxis <- ""
options$rainCloudPlotsHorizontalDisplay <- FALSE
options$rainCloudPlotsLabelYAxis <- ""
options$rainCloudPlotsSeparatePlots <- ""
options$repeatedMeasuresCells <- c("beerpos", "beerneut", "beerneg", "winepos", "wineneut", "wineneg", "waterpos", "waterneu", "waterneg")
options$repeatedMeasuresFactors <- list(list(levels = c("Beer", "Wine", "Water"), name = "Drink"),
    list(levels = c("Positive", "Neutral", "Negative"), name = "Imagery"))
options$simpleFactor <- "Imagery"
options$sphericityGreenhouseGeisser <- TRUE
options$sphericityHuynhFeldt <- TRUE
options$sphericityTests <- TRUE
options$withinModelTerms <- list(list(components = "Drink"))
set.seed(1)
results <- runAnalysis("AnovaRepeatedMeasures", "Alcohol Attitudes.csv", options)


test_that("Test of Sphericity table results match", {
  errorMessage <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_simpleEffectsContainer"]][["collection"]][["rmAnovaContainer_simpleEffectsContainer_simpleEffectsTable"]][["error"]][["errorMessage"]]
  testthat::expect_equal(object = errorMessage, expected = "Moderator factors must also appear in the model terms!")
})

# test model without interaction effect
options <- analysisOptions("AnovaRepeatedMeasures")
options$withinModelTerms <- list(list(components = "Drink"), list(components = "Imagery"))
options$sphericityGreenhouseGeisser <- TRUE
options$sphericityHuynhFeldt <- TRUE
options$postHocTestPooledError <- FALSE
options$postHocTestsHolm <- FALSE
options$postHocTestsBonferroni <- TRUE
options$labelYAxis <- "Alcohol Attitudes"
options$plotErrorBars <- TRUE
options$contrasts <- list(list(contrast = "none", variable = "Drink"), list(contrast = "none", variable = "Imagery"))
options$customContrasts <- list()
options$rainCloudPlotsHorizontalAxis <- ""
options$rainCloudPlotsHorizontalDisplay <- FALSE
options$rainCloudPlotsLabelYAxis <- ""
options$rainCloudPlotsSeparatePlots <- ""
options$repeatedMeasuresCells <- c("beerpos", "beerneut", "beerneg", "winepos", "wineneut", "wineneg", "waterpos", "waterneu", "waterneg")
options$repeatedMeasuresFactors <- list(list(levels = c("Beer", "Wine", "Water"), name = "Drink"), list(levels = c("Positive", "Neutral", "Negative"), name = "Imagery"))
set.seed(1)
results <- runAnalysis("AnovaRepeatedMeasures", "AnovaRepeatedMeasures.csv", options)


test_that("No interaction: Between Subjects Effects table results match", {
	table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_betweenTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list("TRUE", "", 101.058187134503, "", 1920.10555555556, "Residuals",
			 19))
})

test_that("No interaction: Within Subjects Effects table results match", {
	table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_withinAnovaTable"]][["data"]]
	jaspTools::expect_equal_tables(table,
		list(1, 1, 1, 1, 1, 1, "TRUE", 5.10598105687077, 1046.17222222222,
			 2092.34444444444, "Drink", "None", 2, 0.0108629307294978, "FALSE",
			 5.10598105687077, 1812.76427443316, 2092.34444444444, "Drink",
			 "Greenhouse-Geisser", 1.15422864073086, 0.0297686804863521,
			 "FALSE", 5.10598105687077, 1770.93939197604, 2092.34444444444,
			 "Drink", "Huynh-Feldt", 1.18148845405137, 0.028813909529067,
			 "TRUE", "", 204.891520467836, 7785.87777777778, "Residuals",
			 "None", 38, "", 0, "", 355.027614525487, 7785.87777777778, "Residuals",
			 "Greenhouse-Geisser", 21.9303441738863, "", 0, "", 346.836263638895,
			 7785.87777777778, "Residuals", "Huynh-Feldt", 22.4482806269761,
			 "", 1, 1, 1, 1, 1, 1, "TRUE", 122.564824909945, 10814.3388888889,
			 21628.6777777778, "Imagery", "None", 2, 2.68019659683571e-17,
			 "FALSE", 122.564824909945, 14468.4903478118, 21628.6777777778,
			 "Imagery", "Greenhouse-Geisser", 1.49488144635967, 1.75728558571484e-13,
			 "FALSE", 122.564824909945, 13571.4963320567, 21628.6777777778,
			 "Imagery", "Huynh-Feldt", 1.59368408969683, 3.14280380271786e-14,
			 "TRUE", "", 88.2336257309941, 3352.87777777778, "Residuals",
			 "None", 38, "", 0, "", 118.047656482539, 3352.87777777778, "Residuals",
			 "Greenhouse-Geisser", 28.4027474808338, "", 0, "", 110.729129193702,
			 3352.87777777778, "Residuals", "Huynh-Feldt", 30.2799977042398,
			 ""))
})


# Ordinal restrictions ----
options <- analysisOptions("AnovaRepeatedMeasures")
options$betweenModelTerms <- list(list(components = "facGender"), list(components = "facExperim"),
                                  list(components = c("facGender", "facExperim")))
options$betweenSubjectFactors <- c("facGender", "facExperim")
options$contrasts <- list(list(contrast = "none", variable = "fac1"), list(contrast = "none",
                                                                           variable = "fac2"), list(contrast = "none", variable = c("fac1",
                                                                                                                                    "fac2")), list(contrast = "none", variable = "facGender"), list(
                                                                                                                                      contrast = "none", variable = "facExperim"), list(contrast = "none",
                                                                                                                                                                                        variable = c("facGender", "facExperim")), list(contrast = "none",
                                                                                                                                                                                                                                       variable = c("facGender", "fac1")), list(contrast = "none",
                                                                                                                                                                                                                                                                                variable = c("facGender", "fac2")), list(contrast = "none",
                                                                                                                                                                                                                                                                                                                         variable = c("facGender", "fac1", "fac2")), list(contrast = "none",
                                                                                                                                                                                                                                                                                                                                                                          variable = c("facExperim", "fac1")), list(contrast = "none",
                                                                                                                                                                                                                                                                                                                                                                                                                    variable = c("facExperim", "fac2")), list(contrast = "none",
                                                                                                                                                                                                                                                                                                                                                                                                                                                              variable = c("facExperim", "fac1", "fac2")), list(contrast = "none",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                variable = c("facGender", "facExperim", "fac1")), list(contrast = "none",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       variable = c("facGender", "facExperim", "fac2")), list(contrast = "none",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              variable = c("facGender", "facExperim", "fac1", "fac2")))
options$customContrasts <- list()
options$rainCloudPlotsHorizontalAxis <- ""
options$rainCloudPlotsHorizontalDisplay <- FALSE
options$rainCloudPlotsLabelYAxis <- ""
options$rainCloudPlotsSeparatePlots <- ""
options$repeatedMeasuresCells <- c("contNormal", "contGamma", "contcor1", "contcor2")
options$repeatedMeasuresFactors <- list(list(levels = c("l1", "l2"), name = "fac1"), list(levels = c("a",
                                                                                                     "b"), name = "fac2"))
options$restrictedBootstrapping <- TRUE
options$restrictedBootstrappingConfidenceIntervalLevel <- 0.95
options$restrictedBootstrappingReplicates <- 100
options$restrictedIncludeIntercept <- TRUE
options$restrictedInformedHypothesisTestByDefault <- FALSE
options$restrictedMarginalMeansByDefault <- TRUE
options$restrictedModelComparison <- "complement"
options$restrictedModelComparisonCoefficients <- FALSE
options$restrictedModelComparisonHighlightCoefficients <- TRUE
options$restrictedModelComparisonMatrix <- FALSE
options$restrictedModelComparisonReference <- "complement"
options$restrictedModelComparisonWeights <- TRUE
options$restrictedModelMarginalMeansTerms <- list(list(variable = c("fac1", "fac2")), list(variable = c("facGender",
                                                                                                        "facExperim")))
options$restrictedModelShowAvailableCoefficients <- TRUE
options$restrictedModelSummaryByDefault <- TRUE
options$restrictedModels <- list(list(informedHypothesisTest = FALSE, marginalMeans = TRUE,
                                      modelName = "Model 1", modelSummary = TRUE, restrictionSyntax = "contNormal..Intercept. < 0\ncontcor2.facGenderm.facExperimexperimental > 0"))
options$restrictedSE <- "standard"
options$withinModelTerms <- list(list(components = "fac1"), list(components = "fac2"), list(
  components = c("fac1", "fac2")))
set.seed(1)
results <- runAnalysis("AnovaRepeatedMeasures", "test.csv", options)


test_that("Ordinal restrictions: Within factor marginal means table results match", {
  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_ordinalRestrictions"]][["collection"]][["rmAnovaContainer_ordinalRestrictions_Model 1"]][["collection"]][["rmAnovaContainer_ordinalRestrictions_Model 1_marginalMeansContainer"]][["collection"]][["rmAnovaContainer_ordinalRestrictions_Model 1_marginalMeansContainer_fac1_fac2"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.111192059436893, "l1", "a", -0.348433940610132, -0.193021696619891,
                                      0.063157376558472, 0.0991694646477927, "l2", "a", -0.117008378664137,
                                      0.0623965782769017, 0.238451181290795, 0.150936928904207, "l1",
                                      "b", 1.78929345522918, 2.06675525153415, 2.36426645338382, 0.0930601642018357,
                                      "l2", "b", -0.117820277971269, 0.0739325926937613, 0.219009692466684
                                 ))
})

test_that("Ordinal restrictions: Between factor marginal means table results match", {
  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_ordinalRestrictions"]][["collection"]][["rmAnovaContainer_ordinalRestrictions_Model 1"]][["collection"]][["rmAnovaContainer_ordinalRestrictions_Model 1_marginalMeansContainer"]][["collection"]][["rmAnovaContainer_ordinalRestrictions_Model 1_marginalMeansContainer_facGender_facExperim"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.116320641523988, "control", "f", 0.206795573859205, 0.421218273543759,
                                      0.645706227994994, 0.1391233133912, "control", "m", 0.377076198054344,
                                      0.6620015604446, 0.931755102677303, 0.115680273869516, "experimental",
                                      "f", 0.205479564635142, 0.432494923921876, 0.627514748263424,
                                      0.0972146860548308, "experimental", "m", 0.3445931803434, 0.488458932735193,
                                      0.689501306286302))
})

test_that("Ordinal restrictions: Coefficients table results match", {
  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_ordinalRestrictions"]][["collection"]][["rmAnovaContainer_ordinalRestrictions_Model 1"]][["collection"]][["rmAnovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer"]][["collection"]][["rmAnovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer_coefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("contNormal..Intercept.", -0.441571745227907, -0.81244949558875,
                                      0.193448313939833, -0.0554231431302942, "contNormal:facGenderm",
                                      0.58164309673504, -0.0826348336172975, 0.381269716912694, 1.08935760753205,
                                      "contNormal:facExperimexperimental", 0.099527455577631, -0.357946798142768,
                                      0.272849532129813, 0.607116934811138, "contNormal:facGenderm:facExperimexperimental",
                                      -0.264268801066169, -0.960286428404513, 0.455638999017653, 0.633938720194386,
                                      "contGamma..Intercept.", 1.64709667642169, 1.25665112180151,
                                      0.236609950348413, 2.08804679977783, "contGamma:facGenderm",
                                      0.686014101318584, -0.163539673627095, 0.493777273938826, 1.71124935227611,
                                      "contGamma:facExperimexperimental", 0.40757499005641, -0.140772516021531,
                                      0.323351644438931, 1.05780465183572, "contGamma:facGenderm:facExperimexperimental",
                                      -0.465122633957602, -1.6697324704635, 0.576741635866038, 0.550201288145503,
                                      "contcor1..Intercept.", 0.199270271857154, -0.122081713914664,
                                      0.177136020618645, 0.474399684943018, "contcor1:facGenderm",
                                      -0.07033744997919, -0.49683689022239, 0.250317102817171, 0.448604752069799,
                                      "contcor1:facExperimexperimental", -0.246127834791597, -0.711623183730042,
                                      0.244875440504839, 0.213464906637517, "contcor1:facGenderm:facExperimexperimental",
                                      0.0765935743857436, -0.495992960371517, 0.294930934246959, 0.559167142166626,
                                      "contcor2..Intercept.", 0.332326331880421, 0.00317549214264327,
                                      0.172359202440059, 0.612071705312522, "contcor2:facGenderm",
                                      -0.212893461529658, -0.606617240416401, 0.201191759979036, 0.182078001714229,
                                      "contcor2:facExperimexperimental", -0.273596296677489, -0.587464000214955,
                                      0.185437062955922, 0.125504776318483, "contcor2:facGenderm:facExperimexperimental",
                                      0, 0, 0.0682869041725479, 0.271108272580777))
})

test_that("Ordinal restrictions: Restriction Matrix table results match", {
  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_ordinalRestrictions"]][["collection"]][["rmAnovaContainer_ordinalRestrictions_Model 1"]][["collection"]][["rmAnovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer"]][["collection"]][["rmAnovaContainer_ordinalRestrictions_Model 1_modelSummaryContainer_restrictionMatrix"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                      0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0))
})

test_that("Ordinal restrictions: Model Comparison Table results match", {
  table <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_ordinalRestrictions"]][["collection"]][["rmAnovaContainer_ordinalRestrictions_modelComparison"]][["collection"]][["rmAnovaContainer_ordinalRestrictions_modelComparison_comparisonTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(11.3366660265192, 0.400261631091091, 9.32382290342566, "Model 1",
                                      14.9921559166853, 0.667393736737701, 10.5279158319605, 0.599738368908909,
                                      10.2438861673345, "Complement", 15.5078440833147, 1))
})
