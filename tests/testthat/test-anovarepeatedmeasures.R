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

initOptsAnovaRepeatedMeasures <- function(){
  options <- initClassicalAnovaOptions("AnovaRepeatedMeasures")

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
  options <- initOptsAnovaRepeatedMeasures()

  options$sphericityCorrectionNone <- TRUE
  options$sphericityCorrectionHuynhFeldt <- TRUE
  options$sphericityCorrectionGreenhouseGeisser <- TRUE

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
  options <- initOptsAnovaRepeatedMeasures()

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
  options <- initOptsAnovaRepeatedMeasures()

  options$postHocTerms <- list(list(components = "Drink"),
                                        list(components = "Imagery"),
                                        list(components = c("Drink", "Imagery")))
  options$postHocEffectSize <- TRUE
  options$postHocCorrectionBonferroni <- TRUE
  options$postHocCorrectionHolm <- TRUE

  options$poolErrorTermFollowup <- FALSE
  options$postHocCi <- TRUE
  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures", dataset = "AnovaRepeatedMeasures.csv",
                            options = options)

  refTable <- list("TRUE", 2.84948954082566, 0.703009687611415, 0.362221443896783,
                   -0.427137554801075, 1.15158044259464, "Beer", "Wine", 3.5, 0.234336562537138,
                   -3.98021184328793, 1.22829017262714, 10.9802118432879, "FALSE",
                   3.3351289023547, 0.0660988675936689, 0.860707145259498, -0.116698645024244,
                   1.83811293554324, "Beer", "Water", 8.31666666666667, 0.0440659117291126,
                   -0.438399936264863, 2.49365674016224, 17.0717332695982, "TRUE",
                   1.1164571680934, 0.00112213065327869, 0.498485701362715, 0.128265517717469,
                   0.868705885007961, "Wine", "Water", 4.81666666666667, 0.00112213065327869,
                   1.88584835284472, 4.31424223366509, 7.74748498048862)

  table <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_postHocStandardContainer$collection[[1]]$data
  jaspTools::expect_equal_tables(table, refTable)


  refTable <- list("TRUE", 1.11255461788524, 8.64627761186185e-10, 1.37299175877066,
                   0.714799874120407, 2.03118364342092, "Positive", "Neutral",
                   13.2666666666667, 5.76418507457457e-10, 10.3460929604728, 11.9245082024684,
                   16.1872403728605, "FALSE", 1.91462133431161, 5.36180351283324e-11,
                   2.77875593389389, 1.9137049465132, 3.64380692127458, "Positive",
                   "Negative", 26.85, 5.36180351283324e-11, 21.8239162137161, 14.0236607201777,
                   31.8760837862839, "TRUE", 1.97985098972637, 4.54655986206157e-06,
                   1.40576417512323, 0.857440646887211, 1.95408770335925, "Neutral",
                   "Negative", 13.5833333333333, 1.51551995402052e-06, 8.38601479290267,
                   6.86078568731614, 18.780651873764)

  table <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_postHocStandardContainer$collection[[3]]$data
  jaspTools::expect_equal_tables(table, refTable)


  refTable <- list("TRUE", 3.46721486893565, 1, -0.445014916787477, -1.81373548045414,
                   0.923705646879189, "Beer, Positive", "Wine, Positive", -4.30000000000001,
                   0.922662837497563, -17.2656041481929, -1.24018849784179, 8.6656041481929,
                   "FALSE", 3.75939174811424, 1, 0.377745220063787, -1.095098448011,
                   1.85058888813857, "Beer, Positive", "Water, Positive", 3.65,
                   1, -10.4081957238189, 0.970901742770191, 17.7081957238188, "FALSE",
                   2.66603885153513, 0.0198201447121736, 1.1435848443027, -0.0997231522906254,
                   2.38689284089602, "Beer, Positive", "Beer, Neutral", 11.05,
                   0.00825839363007232, 1.08038297325989, 4.14472579558896, 21.0196170267401,
                   "FALSE", 3.34065861769801, 0.399051281084609, 0.972823306465644,
                   -0.448352572155917, 2.39399918508721, "Beer, Positive", "Wine, Neutral",
                   9.4, 0.121932335886964, -3.09234872040626, 2.81381639841947,
                   21.8923487204063, "FALSE", 3.64193413044859, 0.00212064208374063,
                   1.93529742881995, 0.100950399557992, 3.76964445808191, "Beer, Positive",
                   "Water, Neutral", 18.7, 0.00117813449096702, 5.08103548405554,
                   5.1346343262108, 32.3189645159445, "FALSE", 3.23907069193749,
                   0.00216685898320669, 1.71796456248188, 0.0877920219702268, 3.34813710299354,
                   "Beer, Positive", "Beer, Negative", 16.6, 0.00117813449096702,
                   4.48753798446683, 5.12492673942553, 28.7124620155332, "FALSE",
                   3.06720771352296, 5.6165754362217e-08, 3.42040534879676, 2.10440771856973,
                   4.73640297902379, "Beer, Positive", "Wine, Negative", 33.05,
                   4.83649551452424e-08, 21.5802173193464, 10.7752728497279, 44.5197826806536,
                   "FALSE", 3.59742232277794, 2.85560149977802e-06, 3.13062819367934,
                   1.55209869024117, 4.7091576971175, "Beer, Positive", "Water, Negative",
                   30.25, 2.06237886095079e-06, 16.7974868001677, 8.40879865799043,
                   43.7025131998323, "TRUE", 2.22482997576278, 0.0730045740515507,
                   0.822760136851263, -0.172462854620356, 1.81798312832288, "Wine, Positive",
                   "Water, Positive", 7.95, 0.0283906676867142, -0.369722270811851,
                   3.5733067634862, 16.2697222708119, "FALSE", 3.09181857573551,
                   0.00309676924424363, 1.58859976109018, 0.0522315422126434, 3.12496797996771,
                   "Wine, Positive", "Beer, Neutral", 15.35, 0.00146236325422616,
                   3.78818532558322, 4.96471562738716, 26.9118146744168, "FALSE",
                   1.87378030504618, 2.23616547272198e-05, 1.41783822325312, 0.292836386008158,
                   2.54284006049808, "Wine, Positive", "Wine, Neutral", 13.7, 1.55289268939026e-05,
                   6.69302333017313, 7.31142277624824, 20.7069766698269, "FALSE",
                   1.60262941837206, 4.30058246593928e-10, 2.38031234560743, 0.808786558672926,
                   3.95183813254193, "Wine, Positive", "Water, Neutral", 23, 3.94220059377767e-10,
                   17.006988484899, 14.3514150784547, 28.993011515101, "FALSE",
                   4.74890569112063, 0.011052375943825, 2.16297947926936, -0.0951987256471951,
                   4.42115768418591, "Wine, Positive", "Beer, Negative", 20.9,
                   0.00491216708614447, 3.14152997270924, 4.40101390917875, 38.6584700272908,
                   "FALSE", 2.48709828811345, 1.94121686860167e-10, 3.86542026558424,
                   2.58913664033462, 5.14170389083386, "Wine, Positive", "Wine, Negative",
                   37.35, 1.88729417780718e-10, 28.0495288436721, 15.0175005863284,
                   46.6504711563279, "FALSE", 2.36807161164833, 3.22326890748401e-10,
                   3.57564311046681, 2.20892896511485, 4.94235725581878, "Wine, Positive",
                   "Water, Negative", 34.55, 3.04419841262379e-10, 25.6946275261761,
                   14.5899304016195, 43.4053724738239, "TRUE", 3.12830877463745,
                   1, 0.765839624238912, -0.530912899083633, 2.06259214756146,
                   "Water, Positive", "Beer, Neutral", 7.4, 0.230354980867463,
                   -4.29826929062482, 2.36549539482643, 19.0982692906248, "FALSE",
                   1.68565932750988, 0.105471813829176, 0.595078086401858, -0.150500827453272,
                   1.34065700025699, "Water, Positive", "Wine, Neutral", 5.75,
                   0.038087043882758, -0.553500762244738, 3.41112815986023, 12.0535007622447,
                   "FALSE", 1.65985890332503, 8.95625360209062e-07, 1.55755220875617,
                   0.415016344793981, 2.70008807271835, "Water, Positive", "Water, Neutral",
                   15.05, 6.71719020156797e-07, 8.84297958278184, 9.06703574011734,
                   21.2570204172182, "FALSE", 4.7386845831884, 0.47583632133112,
                   1.3402193424181, -0.665816349121889, 3.34625503395808, "Water, Positive",
                   "Beer, Negative", 12.95, 0.132176755925311, -4.77024832935314,
                   2.732825908258, 30.6702483293531, "FALSE", 2.18415345518337,
                   1.30969584816634e-09, 3.04266012873298, 2.13192840866075, 3.9533918488052,
                   "Water, Positive", "Wine, Negative", 29.4, 1.16417408725897e-09,
                   21.2323870399446, 13.4605926750379, 37.5676129600554, "FALSE",
                   1.6389984104549, 4.91844300671117e-11, 2.75288297361555, 1.93596124750842,
                   3.56980469972268, "Water, Positive", "Water, Negative", 26.6,
                   4.91844300671117e-11, 20.470987119988, 16.229423915437, 32.729012880012,
                   "TRUE", 3.02557084652937, 1, -0.170761537837055, -1.34624990673336,
                   1.00472683105925, "Beer, Neutral", "Wine, Neutral", -1.65, 1,
                   -12.9640821671819, -0.545351632368059, 9.66408216718189, "FALSE",
                   3.03425617348236, 0.748384942811422, 0.791712584517254, -0.476982389768982,
                   2.06040755880349, "Beer, Neutral", "Water, Neutral", 7.65, 0.187096235702856,
                   -3.69656083245846, 2.52121098635526, 18.9965608324585, "FALSE",
                   2.60412122767375, 1, 0.574379718179184, -0.491964315738882,
                   1.64072375209725, "Beer, Neutral", "Beer, Negative", 5.55, 0.324433942639524,
                   -4.188076891176, 2.13123718704823, 15.288076891176, "FALSE",
                   2.20406323340373, 1.95183637425166e-07, 2.27682050449406, 1.41464206030051,
                   3.13899894868762, "Beer, Neutral", "Wine, Negative", 22, 1.57231263481384e-07,
                   13.757934687598, 9.9815648056637, 30.242065312402, "FALSE",
                   2.93401108597626, 0.000103833214494514, 1.98704334937664, 0.850452142396452,
                   3.12363455635682, "Beer, Neutral", "Water, Negative", 19.2,
                   6.63378870381617e-05, 8.22830434652135, 6.54394255419503, 30.1716956534786,
                   "TRUE", 1.39566999568025, 8.13249630417116e-05, 0.962474122354309,
                   0.167089182192478, 1.75785906251614, "Wine, Neutral", "Water, Neutral",
                   9.3, 5.42166420278077e-05, 4.08091050900022, 6.66346631279924,
                   14.5190894909998, "FALSE", 4.84637683440451, 1, 0.745141256016239,
                   -1.18413805977333, 2.6744205718058, "Wine, Neutral", "Beer, Negative",
                   7.2, 0.922662837497563, -10.9229620785372, 1.48564592602191,
                   25.3229620785372, "FALSE", 2.39217474278114, 2.27680923691313e-07,
                   2.44758204233112, 1.52153444862556, 3.37362963603667, "Wine, Neutral",
                   "Wine, Negative", 23.65, 1.77085162871022e-07, 14.7044941860708,
                   9.88640151450833, 32.5955058139292, "FALSE", 2.01347434733309,
                   1.07662631203344e-07, 2.15780488721369, 1.3635418886875, 2.95206788573988,
                   "Wine, Neutral", "Water, Negative", 20.85, 8.97188593361198e-08,
                   13.320639306506, 10.3552349835579, 28.379360693494, "TRUE",
                   4.99678844227391, 1, -0.21733286633807, -2.15561081603155, 1.72094508335541,
                   "Water, Neutral", "Beer, Negative", -2.1, 1, -20.7854242969594,
                   -0.420269944237292, 16.5854242969594, "FALSE", 2.43578043259024,
                   0.00040741668068189, 1.48510791997681, 0.364551143384814, 2.6056646965688,
                   "Water, Neutral", "Wine, Negative", 14.35, 0.000248976860416711,
                   5.24143128571844, 5.89133560972902, 23.4585687142816, "FALSE",
                   2.04357143514151, 0.000681887975826525, 1.19533076485938, 0.295094811547108,
                   2.09556671817166, "Water, Neutral", "Water, Negative", 11.55,
                   0.000397767985898806, 3.90809164468282, 5.65186995736228, 19.1919083553172,
                   "TRUE", 3.26744306920967, 0.00264983094672072, 1.70244078631488,
                   0.352002031252378, 3.05287954137738, "Beer, Negative", "Wine, Negative",
                   16.45, 0.00132491547336036, 4.23143985488445, 5.03451771050411,
                   28.6685601451155, "FALSE", 4.32938247693351, 0.188595268759247,
                   1.41266363119745, -0.289165924357466, 3.11449318675237, "Beer, Negative",
                   "Water, Negative", 13.65, 0.0628650895864158, -2.53966851606586,
                   3.15287458955769, 29.8396685160659, "TRUE", 1.89264504415326,
                   1, -0.289777155117426, -1.04304119549722, 0.463486885262369,
                   "Wine, Negative", "Water, Negative", -2.8, 0.922662837497563,
                   -9.87752111223013, -1.47941105420149, 4.27752111223013)

  table <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_postHocStandardContainer$collection[[2]]$data
  jaspTools::expect_equal_tables(table, refTable)

})

test_that("Post-hoc tests match (pooled errors)", {
  options <- initOptsAnovaRepeatedMeasures()

  options$postHocTerms <- list(list(components = "Drink"),
                               list(components = "Imagery"),
                               list(components = c("Drink", "Imagery")))
  options$postHocEffectSize <- TRUE
  options$postHocCorrectionBonferroni <- TRUE
  options$postHocCorrectionHolm <- TRUE

  options$poolErrorTermFollowup <- TRUE
  options$postHocCi <- TRUE

  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures", dataset = "AnovaRepeatedMeasures.csv",
                                    options = options)

  refTable <- list("TRUE", 2.61337279180141, 0.565320794343042, 0.362221443896782,
                   -0.322203183269585, 1.04664607106315, "Beer", "Wine", 3.49999999999999,
                   0.188440264781014, -3.04548944633882, 1.33926549284514, 10.0454894463388,
                   "FALSE", 2.61337279180141, 0.00873013088303192, 0.860707145259498,
                   0.108019449155344, 1.61339484136365, "Beer", "Water", 8.31666666666667,
                   0.00873013088303192, 1.77117722032785, 3.18234990918917, 14.8621561130055,
                   "TRUE", 2.61337279180141, 0.219382129920764, 0.498485701362716,
                   -0.199590871891703, 1.19656227461714, "Wine", "Water", 4.81666666666667,
                   0.146254753280509, -1.72882277967214, 1.84308441634403, 11.3621561130055)

  table <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_postHocStandardContainer$collection[[1]]$data
  jaspTools::expect_equal_tables(table, refTable)


  refTable <-list("TRUE", 1.71496963754459, 7.62555340826258e-09, 1.37299175877066,
                  0.670396659452183, 2.07558685808914, "Positive", "Neutral",
                  13.2666666666667, 2.89922577241315e-09, 8.971330119751, 7.73580264992986,
                  17.5620032135823, "FALSE", 1.71496963754458, 1.10774185836497e-17,
                  2.77875593389389, 1.99539484787656, 3.56211701991122, "Positive",
                  "Negative", 26.85, 1.10774185836497e-17, 22.5546634530843, 15.6562538555741,
                  31.1453365469156, "TRUE", 1.71496963754459, 4.34883865861973e-09,
                  1.40576417512323, 0.956611951019805, 1.85491639922665, "Neutral",
                  "Negative", 13.5833333333333, 2.89922577241315e-09, 9.28799678641767,
                  7.92045120564427, 17.878669880249)

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
  options <- initOptsAnovaRepeatedMeasures()

  options$descriptives <- TRUE

  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures", dataset = "AnovaRepeatedMeasures.csv",
                            options = options)

  refTable <- list("Beer", "Positive", 21.05, 20, 13.0079934938807, 2.90867577031922,
                   0.617956935576279, "Beer", "Neutral", 10, 20, 10.295630140987,
                   2.30217288664427, 1.0295630140987, "Beer", "Negative", 4.45,
                   20, 17.3037111930543, 3.86922744906933, 3.88847442540545, "Wine",
                   "Positive", 25.35, 20, 6.73775692801786, 1.50660825069181, 0.265789227929698,
                   "Wine", "Neutral", 11.65, 20, 6.24310145596511, 1.39599992459659,
                   0.535888536992713, "Wine", "Negative", -12, 20, 6.18146635643918,
                   1.3822178973626, -0.515122196369932, "Water", "Positive", 17.4,
                   20, 7.07404447704126, 1.58180443265212, 0.406554280289727, "Water",
                   "Neutral", 2.35, 20, 6.83855170878193, 1.52914664884837, 2.91002200373699,
                   "Water", "Negative", -9.2, 20, 6.8024763292882, 1.52107994876217,
                   -0.739399601009587)

  table <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_descriptivesContainer$collection$rmAnovaContainer_descriptivesContainer_tableDescriptives$data
  jaspTools::expect_equal_tables(table, refTable)
})

test_that("Field - Chapter 8 marginal means match", {
  # Field does not use bootstrapped marginal means, so these results are not in the book
  # including the results here to have a unit test for bootstrapped means
  # unaffected by whether we use the pooled error
  options <- initOptsAnovaRepeatedMeasures()

  options$marginalMeanTerms <- options$withinModelTerms[3]
  options$marginalMeanBootstrap <- TRUE
  options$marginalMeanBootstrapSamples <- 500
  set.seed(1)
  results <- jaspTools::runAnalysis("AnovaRepeatedMeasures", dataset = testthat::test_path("AnovaRepeatedMeasures.csv"),
                            options = options)

  table <- results$results$rmAnovaContainer$collection$rmAnovaContainer_marginalMeansContainer$collection[[1]]$data

  refTable <- list("TRUE", "Beer", "Positive", 2.86790158635509, -0.0460999999999991,
                   15.1685266947136, 21.175, 26.25, "FALSE", "Wine", "Positive",
                   1.41905772950904, -0.0319000000000074, 22.8465962270579, 25.175,
                   28.5280824858423, "FALSE", "Water", "Positive", 1.58643387977874,
                   -0.0897000000000041, 14.75, 17.25, 21.6153338048902, "TRUE",
                   "Beer", "Neutral", 2.34101761568637, 0.111499999999999, 4.74075144739576,
                   10.15, 14.65, "FALSE", "Wine", "Neutral", 1.39196783688339,
                   -0.0413000000000014, 8.8, 11.625, 14.3296850141834, "FALSE",
                   "Water", "Neutral", 1.52816109840729, -0.0160999999999993, -1.27414552469888,
                   2.35, 4.91555056576276, "TRUE", "Beer", "Negative", 3.86263448961985,
                   0.156299999999999, -3.10608796464486, 4.7, 11.6087735468859,
                   "FALSE", "Wine", "Negative", 1.35802704867387, 0.0550999999999995,
                   -14.5990420776466, -12, -9.20288430671126, "FALSE", "Water",
                   "Negative", 1.4412391684524, 0.0619999999999976, -12.0849185109411,
                   -9.15, -6.4)

  jaspTools::expect_equal_tables(table, refTable)
})

# Error handling
test_that("Analysis handles errors", {

  options <- initClassicalAnovaOptions("AnovaRepeatedMeasures")
  options[["betweenModelTerms"]] <- list(list(components = "Celebrity"))
  options[["betweenSubjectFactors"]] <- "Celebrity"
  options[["repeatedMeasuresCells"]] <- c("Stick Insect", "Kangaroo Testicle", "Fish Eye", "Witchetty Grub")
  options[["repeatedMeasuresFactors"]] <- list(list(
    levels = c("Stick Insect", "Kangaroo Testicle", "Fish Eye", "Witchetty Grub"),
    name = "Animal"
  ))

  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures", options = options, dataset = "Bush Tucker Food.csv")
  expect_identical(results$status, "validationError", label = "Duplicate variables in subject and betweenSubjectFactors")

})


# Mixed Effects
initOptsMixed <- function(){
  options <- initClassicalAnovaOptions("AnovaRepeatedMeasures")
  options$multivariateModelFollowup <- FALSE
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
  options <- initOptsMixed()
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
  options <- initOptsMixed()

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
  options <- initOptsMixed()
  options$poolErrorTermFollowup <- TRUE

  options$contrasts <- list(list(contrast = "repeated", variable = "Looks"),
                            list(contrast = "difference", variable = "Charisma"))

  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv",
                            options = options)

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
  options <- initOptsMixed()
  options$sphericityCorrections <- TRUE
  options$sphericityTests <- TRUE
  options$descriptivePlotHorizontalAxis <- "Charisma"
  options$descriptivePlotSeparateLines <- "gender"
  options$descriptivePlotErrorBar <- TRUE
  options$applyMoreyCorrectionErrorBars <- TRUE

  options$descriptivePlotErrorBarPooled <- FALSE
  options$descriptivePlotErrorBarType <- "ci"
  results <- jaspTools::runAnalysis(
    name    = "AnovaRepeatedMeasures",
    dataset = "AnovaMixedEffects.csv",
    options = options)
  descPlot <-  results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(descPlot, "mixedRMANOVA1")

  options$descriptivePlotErrorBarPooled <- TRUE
  options$descriptivePlotErrorBarType <- "ci"
  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv", options = options)
  descPlot <-  results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(descPlot, "mixedRMANOVA2")

  options$descriptivePlotErrorBarPooled <- FALSE
  options$descriptivePlotErrorBarType <- "se"
  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv", options = options)
  descPlot <-  results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(descPlot, "mixedRMANOVA3")

  options$descriptivePlotErrorBarPooled <- TRUE
  options$descriptivePlotErrorBarType <- "se"
  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaMixedEffects.csv", options = options)
  descPlot <-  results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(descPlot, "mixedRMANOVA4")

})

test_that("Bar Plots match", {
  options <- initOptsMixed()
  options$sphericityCorrections <- TRUE
  options$sphericityTests <- TRUE
  options$barPlotHorizontalAxis <- "Charisma"
  options$barPlotSeparatePlots <- "gender"
  options$barPlotHorizontalZeroFix <- TRUE
  options$barPlotErrorBars <- TRUE
  options$barPlotCiInterval <- 0.95

  options$usePooledStandErrorCITwo <- FALSE
  options$barPlotErrorBarType <- "confidenceInterval"
  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                                    dataset = "AnovaMixedEffects.csv", options = options)
  barPlot <-  results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(barPlot, "mixedRMANOVA1Bar")

  options$usePooledStandErrorCITwo <- TRUE
  options$barPlotErrorBarType <- "confidenceInterval"
  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                                    dataset = "AnovaMixedEffects.csv", options = options)
  barPlot <-  results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(barPlot, "mixedRMANOVA2Bar")

  options$usePooledStandErrorCITwo <- FALSE
  options$barPlotErrorBarType <- "standardError"
  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                                    dataset = "AnovaMixedEffects.csv", options = options)
  barPlot <-  results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(barPlot, "mixedRMANOVA3Bar")

  options$usePooledStandErrorCITwo <- TRUE
  options$barPlotErrorBarType <- "standardError"
  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                                    dataset = "AnovaMixedEffects.csv", options = options)
  barPlot <-  results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(barPlot, "mixedRMANOVA4Bar")

})

test_that("Raincloud Plots match", {
  options <- initOptsMixed()
  options$sphericityCorrections <- TRUE
  options$sphericityTests <- TRUE
  options$rainCloudHorizontalAxis <- "Charisma"
  options$rainCloudSeparatePlots <- "gender"
  set.seed(1)
  results <- jaspTools::runAnalysis(
    name    = "AnovaRepeatedMeasures",
    dataset = "AnovaMixedEffects.csv",
    options = options)
  testPlot <-  results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(testPlot, "raincloud-plots")
})

test_that("Raincloud Plots match for between subjects", {
  options <- initOptsMixed()
  options$sphericityCorrections <- TRUE
  options$sphericityTests <- TRUE
  options$rainCloudHorizontalAxis <- "gender"
  options$rainCloudSeparatePlots <- "Charisma"
  set.seed(1)
  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                                    dataset = "AnovaMixedEffects.csv", options = options)
  testPlot <-  results$state$figures[[1]]$obj
  jaspTools::expect_equal_plots(testPlot, "raincloud-plots-between-subjects")
})


test_that("Effect Size Calculation correct", {
  options <- initClassicalAnovaOptions("AnovaRepeatedMeasures")

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
  options$effectSizePartialOmegaSquared <- TRUE

  options$effectSizeGeneralEtaSquared <- TRUE

  results <- jaspTools::runAnalysis(name = "AnovaRepeatedMeasures",
                            dataset = "AnovaRepeatedMeasuresOneWay.csv",
                            options = options)

  refTable <- list(1, 1, 1, 1, 1, 1, "TRUE", 3.79380603096984, 27.7083333333333,
                   83.1249999999999, "Animal", 0.351479915433403, 0.327424913835549,
                   3, 0.238785176929506, 0.0255702968630395, 0.351479915433403,
                   0.238785176929506, "TRUE", "", 7.30357142857143, 153.375, "Residuals",
                   "", "", 21, "", "")

  table <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_withinAnovaTable$data
  jaspTools::expect_equal_tables(table, refTable)
})


test_that("Simple Effects table match", {
  options <- initOptsMixed()

  options$betweenSubjectFactors <- "gender"
  options$betweenModelTerms <- list(
    list(components = "gender")
  )

  options$simpleMainEffectFactor <- "Looks"
  options$simpleMainEffectModeratorFactorOne <- "gender"
  options$simpleMainEffectModeratorFactorTwo <- "Charisma"

  results <- jaspTools::runAnalysis(name    = "AnovaRepeatedMeasures",
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

# Andy Field tests ----
# should we put this in verification?
test_that("Field - Chapter 8 results match", {
  options <- initClassicalAnovaOptions("AnovaRepeatedMeasures")

  options$repeatedMeasuresFactors <- list(
    list(name = "Animal", levels = c("Stick", "Kangaroo", "Fish", "Grub"))
  )
  options$repeatedMeasuresCells <- c("Stick Insect", "Kangaroo Testicle",
                                     "Fish Eye", "Witchetty Grub")

  options$withinModelTerms <- list(
    list(components = "Animal")
  )

  options$sphericityTests <- TRUE
  options$sphericityCorrectionNone <- TRUE
  options$sphericityCorrectionHuynhFeldt <- TRUE
  options$sphericityCorrectionGreenhouseGeisser <- TRUE

  options$postHocTerms <- "Animal"
  options$postHocPooledError <- FALSE
  options$postHocCorrectionBonferroni <- TRUE
  options$postHocCi <- TRUE
  options$postHocEffectSize <- TRUE

  options$friedmanWithinFactor <- "Animal"
  options$conoverTest <- TRUE

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
                   -0.367925917593048, 3.50626858994993, "Stick", "Kangaroo", 3.875,
                   0.0101164143794359, 0.924654528093316, 4.77528950227193, 6.82534547190668,
                   "FALSE", 0.7319250547114, 0.00564485983507569, 1.61978976637775,
                   -0.287731746731882, 3.52731127948737, "Stick", "Fish", 4, 0.00564485983507569,
                   1.33886145376625, 5.46504040851178, 6.66113854623374, "FALSE",
                   1.79222029098785, 1, 0.961750173786787, -1.83755985804869, 3.76106020562226,
                   "Stick", "Grub", 2.375, 0.906918466006363, -4.14116783575006,
                   1.32517191772833, 8.89116783575006, "TRUE", 1.20174723988509,
                   1, 0.0506184301993042, -1.71941068744157, 1.82064754784018,
                   "Kangaroo", "Fish", 0.124999999999999, 0.920074728368497, -4.24432153408686,
                   0.104015217053423, 4.49432153408686, "FALSE", 1.33630620956212,
                   1, -0.607421162391655, -2.66150784457404, 1.44666551979073,
                   "Kangaroo", "Grub", -1.5, 0.906918466006363, -6.35855203472909,
                   -1.12249721603218, 3.35855203472909, "TRUE", 1.82186619392628,
                   1, -0.65803959259096, -3.41555353532226, 2.09947435014034, "Fish",
                   "Grub", -1.625, 0.906918466006363, -8.24895462968413, -0.891942561653215,
                   4.99895462968413)

  jaspTools::expect_equal_tables(table, refTable)


  # Friedman Test
  # Kendall W now also verified by case presented in
  # https://github.com/jasp-stats/jasp-issues/issues/1473
  table <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_nonparametricContainer$collection$rmAnovaContainer_nonparametricContainer_friedmanTable$data

  refTable <- list("Animal", 11.5263157894737, 3, 0.480263157894737, 0.00919516147593829)
  jaspTools::expect_equal_tables(table, refTable)

  # Connover Test
  table <- results[["results"]]$rmAnovaContainer$collection$rmAnovaContainer_nonparametricContainer$collection$rmAnovaContainer_nonparametricContainer_conoverContainer$collection$rmAnovaContainer_nonparametricContainer_conoverContainer_Animal$data
  refTable <- list("Stick", "Kangaroo", 0.0053568257851033, 21, 0.0053568257851033,
                   0.000892804297517217, 1, 3.86685035210436, 29, 14, "Stick",
                   "Fish", 0.00988691933819582, 21, 0.00823909944849651, 0.0016478198896993,
                   1, 3.60906032863074, 29, 15, "Stick", "Grub", 0.513063268547429,
                   21, 0.256531634273715, 0.0855105447579049, 0.523809523809524,
                   1.80453016431537, 29, 22, "Kangaroo", "Fish", 1, 21, 0.799078360641179,
                   0.799078360641179, 0.0277777777777777, 0.257790023473624, 14,
                   15, "Kangaroo", "Grub", 0.310592694973537, 21, 0.207061796649025,
                   0.0517654491622562, -0.523809523809524, 2.06232018778899, 14,
                   22, "Fish", "Grub", 0.513063268547429, 21, 0.256531634273715,
                   0.0855105447579049, -0.333333333333333, 1.80453016431537, 15,
                   22)
  jaspTools::expect_equal_tables(table, refTable)
})


test_that("Field - Chapter 9 match",  {
  options <- initOptsMixed()

  options$sphericityTests <- TRUE
  options$poolErrorTermFollowup <- FALSE

  options$marginalMeanTerms <- list(
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
  refTable <- list("TRUE", "High", 1.00998105348321, 79.9781085445112, 82.1, 84.2218914554887,
                   "FALSE", "Some", 0.73156352095029, 67.7630420749992, 69.3, 70.8369579250008,
                   "FALSE", "None", 0.573003781255703, 53.0961637268184, 54.3,
                   55.5038362731816)

  jaspTools::expect_equal_tables(table, refTable)

  # gender (strategy) * looks interaction
  table <- results$results$rmAnovaContainer$collection$rmAnovaContainer_marginalMeansContainer$collection[[6]]$data
  refTable <- list("TRUE", "Attractive", 0.922757542088647, "Female", 74.228025008694,
                   76.1666666666667, 78.1053083246393, "FALSE", "Average", 1.15910246247626,
                   "Female", 65.664816089686, 68.1, 70.535183910314, "FALSE", "Ugly",
                   0.920580868692409, "Female", 59.3992646964732, 61.3333333333333,
                   63.2674019701935, "TRUE", "Attractive", 0.922757542088647, "Male",
                   86.0946916753607, 88.0333333333333, 89.971974991306, "FALSE",
                   "Average", 1.15910246247626, "Male", 65.0314827563526, 67.4666666666667,
                   69.9018505769807, "FALSE", "Ugly", 0.920580868692409, "Male",
                   48.3659313631399, 50.3, 52.2340686368601)

  jaspTools::expect_equal_tables(table, refTable)

  # gender (strategy) * charisma interaction
  table <- results$results$rmAnovaContainer$collection$rmAnovaContainer_marginalMeansContainer$collection[[3]]$data
  refTable <- list("TRUE", "High", 1.42832890357583, "Female", 85.2325256590976,
                   88.2333333333333, 91.2341410075691, "FALSE", "Some", 1.03458705306531,
                   "Female", 66.8930799243337, 69.0666666666667, 71.2402534089996,
                   "FALSE", "None", 0.810349718742881, "Female", 46.59751841559,
                   48.3, 50.00248158441, "TRUE", "High", 1.42832890357583, "Male",
                   72.9658589924309, 75.9666666666667, 78.9674743409024, "FALSE",
                   "Some", 1.03458705306531, "Male", 67.3597465910004, 69.5333333333333,
                   71.7069200756663, "FALSE", "None", 0.810349718742881, "Male",
                   58.59751841559, 60.3, 62.00248158441)

  jaspTools::expect_equal_tables(table, refTable)

  # looks * charisma interaction
  table <- results$results$rmAnovaContainer$collection$rmAnovaContainer_marginalMeansContainer$collection[[5]]$data
  refTable <- list("TRUE", "High", "Attractive", 1.38293166859393, 86.0445683773036,
                   88.95, 91.8554316226963, "FALSE", "High", "Average", 1.72078793321871,
                   81.98475870452, 85.6, 89.21524129548, "FALSE", "High", "Ugly",
                   1.24911079483678, 69.1257156004244, 71.75, 74.3742843995756,
                   "TRUE", "Some", "Attractive", 1.40771682758524, 84.8424966905079,
                   87.8, 90.757503309492, "FALSE", "Some", "Average", 1.17201156611661,
                   67.8876950693282, 70.35, 72.8123049306718, "FALSE", "Some",
                   "Ugly", 1.2107160415776, 47.2063799837762, 49.75, 52.2936200162238,
                   "TRUE", "None", "Attractive", 1.01885011437186, 67.409475339014,
                   69.55, 71.6905246609859, "FALSE", "None", "Average", 0.887568463712957,
                   45.5352878523626, 47.4, 49.2647121476374, "FALSE", "None", "Ugly",
                   0.746287105907937, 44.3821089708503, 45.95, 47.5178910291497)
  jaspTools::expect_equal_tables(table, refTable)

  # gender (strategy) * looks * charisma interaction
  table <- results$results$rmAnovaContainer$collection$rmAnovaContainer_marginalMeansContainer$collection[[2]]$data
  refTable <- list("TRUE", "High", "Attractive", 1.9557607215608, "Female", 85.4910991946352,
                   89.6, 93.7089008053648, "FALSE", "Some", "Attractive", 1.99081222955188,
                   "Female", 82.917458708953, 87.1, 91.2825412910469, "FALSE",
                   "None", "Attractive", 1.44087164977007, "Female", 48.7728409938396,
                   51.8, 54.8271590061604, "TRUE", "High", "Average", 2.43356163312586,
                   "Female", 83.2872767286809, 88.4, 93.5127232713191, "FALSE",
                   "Some", "Average", 1.65747465206024, "Female", 65.4177749723458,
                   68.9, 72.3822250276542, "FALSE", "None", "Average", 1.25521135891752,
                   "Female", 44.3628987908893, 47, 49.6371012091107, "TRUE", "High",
                   "Ugly", 1.76650942696481, "Female", 82.988701410596, 86.7, 90.4112985894039,
                   "FALSE", "Some", "Ugly", 1.71221104618171, "Female", 47.6027780755327,
                   51.2, 54.7972219244673, "FALSE", "None", "Ugly", 1.05540934659917,
                   "Female", 43.8826672422534, 46.1, 48.3173327577466, "TRUE",
                   "High", "Attractive", 1.9557607215608, "Male", 84.1910991946352,
                   88.3, 92.4089008053648, "FALSE", "Some", "Attractive", 1.99081222955188,
                   "Male", 84.317458708953, 88.5, 92.6825412910469, "FALSE", "None",
                   "Attractive", 1.44087164977007, "Male", 84.2728409938396, 87.3,
                   90.3271590061604, "TRUE", "High", "Average", 2.43356163312586,
                   "Male", 77.6872767286809, 82.8, 87.9127232713191, "FALSE", "Some",
                   "Average", 1.65747465206024, "Male", 68.3177749723458, 71.8,
                   75.2822250276542, "FALSE", "None", "Average", 1.25521135891752,
                   "Male", 45.1628987908893, 47.8, 50.4371012091107, "TRUE", "High",
                   "Ugly", 1.76650942696481, "Male", 53.0887014105961, 56.8, 60.511298589404,
                   "FALSE", "Some", "Ugly", 1.71221104618171, "Male", 44.7027780755327,
                   48.3, 51.8972219244673, "FALSE", "None", "Ugly", 1.05540934659917,
                   "Male", 43.5826672422534, 45.8, 48.0173327577466)

  jaspTools::expect_equal_tables(table, refTable)
})

# test model without interaction effect
options <- initClassicalAnovaOptions("AnovaRepeatedMeasures")
options$contrasts <- list(list(contrast = "none", variable = "Drink"))
options$customContrasts <- list()
options$descriptivePlotYAxisLabel <- "Alcohol Attitudes"
options$simpleMainEffectModeratorFactorOne <- "Drink"
options$descriptivePlotErrorBar <- TRUE
options$descriptivePlotHorizontalAxis <- "Drink"
options$descriptivePlotSeparateLines <- "Imagery"
options$poolErrorTermFollowup <- FALSE
options$postHocCorrectionBonferroni <- TRUE
options$postHocCorrectionHolm <- FALSE
options$repeatedMeasuresCells <- c("beerpos", "beerneut", "beerneg", "winepos", "wineneut", "wineneg", "waterpos", "waterneu", "waterneg")
options$repeatedMeasuresFactors <- list(list(levels = c("Beer", "Wine", "Water"), name = "Drink"),
    list(levels = c("Positive", "Neutral", "Negative"), name = "Imagery"))
options$simpleMainEffectFactor <- "Imagery"
options$sphericityCorrectionGreenhouseGeisser <- TRUE
options$sphericityCorrectionHuynhFeldt <- TRUE
options$sphericityTests <- TRUE
options$withinModelTerms <- list(list(components = "Drink"))
set.seed(1)
results <- jaspTools::runAnalysis("AnovaRepeatedMeasures", "Alcohol Attitudes.csv", options)


test_that("Test of Sphericity table results match", {
  errorMessage <- results[["results"]][["rmAnovaContainer"]][["collection"]][["rmAnovaContainer_simpleEffectsContainer"]][["collection"]][["rmAnovaContainer_simpleEffectsContainer_simpleEffectsTable"]][["error"]][["errorMessage"]]
  testthat::expect_equal(object = errorMessage, expected = "Moderator factors must also appear in the model terms!")
})

# test model without interaction effect
options <- initClassicalAnovaOptions("AnovaRepeatedMeasures")
options$withinModelTerms <- list(list(components = "Drink"), list(components = "Imagery"))
options$sphericityCorrectionGreenhouseGeisser <- TRUE
options$sphericityCorrectionHuynhFeldt <- TRUE
options$postHocPooledError <- FALSE
options$postHocCorrectionHolm <- FALSE
options$postHocCorrectionBonferroni <- TRUE
options$descriptivePlotYAxisLabel <- "Alcohol Attitudes"
options$descriptivePlotErrorBar <- TRUE
options$contrasts <- list(list(contrast = "none", variable = "Drink"), list(contrast = "none", variable = "Imagery"))
options$customContrasts <- list()
options$repeatedMeasuresCells <- c("beerpos", "beerneut", "beerneg", "winepos", "wineneut", "wineneg", "waterpos", "waterneu", "waterneg")
options$repeatedMeasuresFactors <- list(list(levels = c("Beer", "Wine", "Water"), name = "Drink"), list(levels = c("Positive", "Neutral", "Negative"), name = "Imagery"))
set.seed(1)
results <- jaspTools::runAnalysis("AnovaRepeatedMeasures", "AnovaRepeatedMeasures.csv", options)


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
options <- initClassicalAnovaOptions("AnovaRepeatedMeasures")
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
options$repeatedMeasuresCells <- c("contNormal", "contGamma", "contcor1", "contcor2")
options$repeatedMeasuresFactors <- list(list(levels = c("l1", "l2"), name = "fac1"), list(levels = c("a",
                                                                                                     "b"), name = "fac2"))
options$restrictedBootstrap <- TRUE
options$restrictedBootstrapCiLevel <- 0.95
options$restrictedBootstrapSamples <- 100
options$restrictedInterceptInclusion <- TRUE
options$restrictedInformedHypothesisTestForAllModels <- FALSE
options$restrictedMarginalMeanForAllModels <- TRUE
options$restrictedModelComparison <- "complement"
options$restrictedModelComparisonCoefficients <- FALSE
options$restrictedModelComparisonCoefficientsHighlight <- TRUE
options$restrictedModelComparisonMatrix <- FALSE
options$restrictedModelComparisonReference <- "complement"
options$restrictedModelComparisonWeights <- TRUE
options$restrictedMarginalMeanTerms <- list(list(variable = c("fac1", "fac2")),
                                            list(variable = c("facGender", "facExperim")))
options$restrictedAvailableCoefficients <- TRUE
options$restrictedModelSummaryForAllModels <- TRUE
options$restrictedModels <- list(list(informedHypothesisTest = FALSE, marginalMean = TRUE,
                                      name = "Model 1", summary = TRUE, syntax = "contNormal..Intercept. < 0\ncontcor2.facGenderm.facExperimexperimental > 0"))
options$restrictedHeterogeneityCorrection <- "none"
options$multivariateModelFollowup <- FALSE
options$withinModelTerms <- list(list(components = "fac1"), list(components = "fac2"), list(
  components = c("fac1", "fac2")))
set.seed(1)
results <- jaspTools::runAnalysis("AnovaRepeatedMeasures", "test.csv", options)


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
