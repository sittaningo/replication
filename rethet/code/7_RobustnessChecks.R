# 4 depth of causal tree ----

### Causal Tree ----
CausalTree_subjhealth <- htetree::causalTree(formula = `Subjective Health` ~ `Birth cohort` + Age + `Survey year` + Female + `Marital status` + `Number of living siblings` + `Self-employment` + `Weekly working hours` + Education + `Number of Living Parents` + `Age(father)` + `Age(mother)` + `Net worth` + `Household income` + `Transfer to grandchildren` + `Total household number` + `Number of living children`, 
                                             data = harmonized_target, 
                                             weights = ipw,
                                             treatment = harmonized_target$is_retirement, 
                                             split.Rule = "CT",
                                             cv.option = "CT",
                                             split.Honest = TRUE, 
                                             cv.Honest = TRUE, 
                                             split.Bucket = FALSE, 
                                             maxdepth = 11,
                                             minsplit = 3000,
                                             minbucket = 1000)

### Data Viz ----

opcp <- CausalTree_subjhealth$cptable[,1][which.min((CausalTree_subjhealth$cptable[,4]))]
opfit <- prune(CausalTree_subjhealth, opcp)

rpart.plot(opfit, box.palette = met.brewer("OKeeffe2", direction = 1, type = "continuous"), family = "Noto Sans", digits = -3, cex = 0.5)


# Benchmark ----

## Causal Tree ----
CausalTree_subjhealth <- htetree::causalTree(formula = `Subjective Health` ~ `Birth cohort` + Age + Female + `Marital status` + `Number of living siblings` + `Survey year` + `Self-employment` + `Weekly working hours` + `Education` + `Number of Living Parents` + `Age(father)` + `Age(mother)` + `Net worth` + `Household income` + `Transfer to grandchildren` + `Total household number` + `Number of living children`, 
                                             data = harmonized_target, 
                                             weights = ipw,
                                             treatment = harmonized_target$is_retirement, 
                                             split.Rule = "CT", 
                                             cv.option = "CT",
                                             split.Honest = TRUE, cv.Honest = TRUE, split.Bucket = FALSE, 
                                             maxdepth = 15,
                                             minsplit = 2400, 
                                             minbucket = 800)


## Data Visularization ----

opcp <- CausalTree_subjhealth$cptable[,1][which.min((CausalTree_subjhealth$cptable[,4]))]
opfit <- prune(CausalTree_subjhealth, opcp)

## Feature Importance ----

opcp <- CausalTree_subjhealth$cptable[,1][which.min((CausalTree_subjhealth$cptable[,4]))]
opfit <- prune(CausalTree_subjhealth, opcp)

importance_subjehealth <- tidyr::tibble(value = opfit$variable.importance, 
                                        feature = attr(opfit$variable.importance, "names"), 
                                        scenario = "Benchmark")


# No IPW and NA ----

harmonized_robust_na <- harmonized_robust |> tidyr::drop_na()


## Causal Tree ----
CausalTree_subjhealth_na <- htetree::causalTree(formula = `Subjective Health` ~ `Birth cohort` + Age + Female + `Marital status` + `Number of living siblings` + `Survey year` + `Self-employment` + `Weekly working hours` + `Education` + `Number of Living Parents` + `Age(father)` + `Age(mother)` + `Net worth` + `Household income` + `Transfer to grandchildren` + `Total household number` + `Number of living children`, 
                                                    data = harmonized_robust_na, 
                                                    na.action = na.causalTree,
                                                    treatment = harmonized_robust_na$is_retirement, 
                                                    split.Rule = "CT", 
                                                    cv.option = "CT",
                                                    split.Honest = TRUE, cv.Honest = TRUE, split.Bucket = FALSE, 
                                                    maxdepth = 15,
                                                    minsplit = 2400, 
                                                    minbucket = 800)

## Feature Importance ----

opcp <- CausalTree_subjhealth_na$cptable[,1][which.min((CausalTree_subjhealth_na$cptable[,4]))]
opfit <- prune(CausalTree_subjhealth_na, opcp)

importance_subjehealth <- tidyr::tibble(value = opfit$variable.importance, 
                                        feature = attr(opfit$variable.importance, "names"), 
                                        scenario = "Incl. predictors' NA") |> 
  dplyr::bind_rows(importance_subjehealth)


# Retirement definition ----

## IPW ----

harmonized_robust_altdef <- harmonized_robust |> tidyr::drop_na()

harmonized_robust_altdef <-
  glm(is_retirement_alt ~ `Birth cohort` + Age + Female + `Marital status` + `Survey year` + `Self-employment` + `Weekly working hours` + `Education` + `Number of Living Parents` + `Age(father)` + `Age(mother)` + `Net worth` + `Household income` + `Transfer to grandchildren` + `Total household number` + `Number of living children` + `Number of living siblings`,
      family=binomial(link = "logit"), 
      data = harmonized_robust_altdef) |> 
  predict(newx = predict, 
          type = "response") |> 
  tidyr::as_tibble() |> 
  (\(.) dplyr::bind_cols(harmonized_robust_altdef, .))()

harmonized_robust_altdef <- harmonized_robust_altdef |> 
  dplyr::mutate(ipw = dplyr::if_else(is_retirement == 1, 1 / value, 1 / (1 - value)))


## Causal Tree ----
CausalTree_subjhealth_altdef <- htetree::causalTree(formula = `Subjective Health` ~ `Birth cohort` + Age + Female + `Marital status` + `Number of living siblings` + `Survey year` + `Self-employment` + `Weekly working hours` + `Education` + `Number of Living Parents` + `Age(father)` + `Age(mother)` + `Net worth` + `Household income` + `Transfer to grandchildren` + `Total household number` + `Number of living children`, 
                                             data = harmonized_robust_altdef, 
                                             weights = ipw,
                                             treatment = harmonized_robust_altdef$is_retirement_alt, 
                                             split.Rule = "CT", 
                                             cv.option = "CT",
                                             split.Honest = TRUE, cv.Honest = TRUE, split.Bucket = FALSE, 
                                             maxdepth = 15,
                                             minsplit = 2400, 
                                             minbucket = 800)

## Feature Importance ----

opcp <- CausalTree_subjhealth_altdef$cptable[,1][which.min((CausalTree_subjhealth_altdef$cptable[,4]))]
opfit <- prune(CausalTree_subjhealth_altdef, opcp)

importance_subjehealth <- tidyr::tibble(value = opfit$variable.importance, 
                                        feature = attr(opfit$variable.importance, "names"), 
                                        scenario = "Alt. definition of retirement") |> 
  dplyr::bind_rows(importance_subjehealth)

# Incl. country ----

## IPW ----

harmonized_robust_country <- harmonized_robust |> tidyr::drop_na()

harmonized_robust_country <-
  glm(is_retirement ~ `Birth cohort` + Age + Female + `Marital status` + `Survey year` + `Self-employment` + `Weekly working hours` + `Education` + `Number of Living Parents` + `Age(father)` + `Age(mother)` + `Net worth` + `Household income` + `Transfer to grandchildren` + `Total household number` + `Number of living children` + `Number of living siblings` + country,
      family=binomial(link = "logit"), 
      data = harmonized_robust_country) |> 
  predict(newx = predict, 
          type = "response") |> 
  tidyr::as_tibble() |> 
  (\(.) dplyr::bind_cols(harmonized_robust_country, .))()

harmonized_robust_country <- harmonized_robust_country |> 
  dplyr::mutate(ipw = dplyr::if_else(is_retirement == 1, 1 / value, 1 / (1 - value)))


## Causal Tree ----
CausalTree_subjhealth_country <- htetree::causalTree(formula = `Subjective Health` ~ `Birth cohort` + Age + Female + `Marital status` + `Number of living siblings` + `Survey year` + `Self-employment` + `Weekly working hours` + `Education` + `Number of Living Parents` + `Age(father)` + `Age(mother)` + `Net worth` + `Household income` + `Transfer to grandchildren` + `Total household number` + `Number of living children` + country, 
                                                    data = harmonized_robust_country, 
                                                    weights = ipw,
                                                    treatment = harmonized_robust_country$is_retirement, 
                                                    split.Rule = "CT", 
                                                    cv.option = "CT",
                                                    split.Honest = TRUE, cv.Honest = TRUE, split.Bucket = FALSE, 
                                                    maxdepth = 15,
                                                    minsplit = 2400, 
                                                    minbucket = 800)

## Feature Importance ----

opcp <- CausalTree_subjhealth_country$cptable[,1][which.min((CausalTree_subjhealth_country$cptable[,4]))]
opfit <- prune(CausalTree_subjhealth_country, opcp)

importance_subjehealth <- tidyr::tibble(value = opfit$variable.importance, 
                                        feature = attr(opfit$variable.importance, "names"), 
                                        scenario = "Incl. country") |> 
  dplyr::bind_rows(importance_subjehealth)

# Data Viz: Importance ----

importance_subjehealth |> 
  dplyr::mutate(feature = fct_reorder(feature, desc(-value)), 
                scenario = fct_relevel(factor(scenario), "Benchmark", "Alt. definition of retirement", "Incl. country", "Incl. predictors' NA")) |> 
  ggplot2::ggplot(aes(x = feature, y = (value) * 10^-11)) + 
  ggplot2::geom_point(size = 2) + 
  ggplot2::coord_flip() + 
  ggplot2::facet_wrap(~ scenario, scales = "free") + 
  ggplot2::theme_minimal(base_size = 18) + 
  ggplot2::ylab("")

# Data Viz: Retirement definition ----

CausalTree_subjhealth_altdef <- htetree::causalTree(formula = `Subjective Health` ~ `Birth cohort` + Age + Female + `Marital status` + `Number of living siblings` + `Survey year` + `Self-employment` + `Weekly working hours` + `Education` + `Number of Living Parents` + `Age(father)` + `Age(mother)` + `Total wealth` + `Household income` + `Transfer to grandchildren` + `Total household number` + `Number of living children`, 
                                                    data = harmonized_robust_altdef, 
                                                    weights = ipw,
                                                    treatment = harmonized_robust_altdef$is_retirement_alt, 
                                                    split.Rule = "CT", 
                                                    cv.option = "CT",
                                                    split.Honest = TRUE, cv.Honest = TRUE, split.Bucket = FALSE, 
                                                    maxdepth = 5,
                                                    minsplit = 2400, 
                                                    minbucket = 800)


opcp <- CausalTree_subjhealth_altdef$cptable[,1][which.min((CausalTree_subjhealth_altdef$cptable[,4]))]
opfit <- prune(CausalTree_subjhealth_altdef, opcp)

rpart.plot(opfit, box.palette = met.brewer("OKeeffe2", direction = 1, type = "continuous"), family = "Noto Sans", digits = -3, cex = 1)

# Data viz: country ----

## Causal Tree ----
CausalTree_subjhealth_country <- htetree::causalTree(formula = `Subjective Health` ~ `Birth cohort` + Age + Female + `Marital status` + `Number of living siblings` + `Survey year` + `Self-employment` + `Weekly working hours` + `Education` + `Number of Living Parents` + `Age(father)` + `Age(mother)` + `Net worth` + `Household income` + `Transfer to grandchildren` + `Total household number` + `Number of living children` + country, 
                                                     data = harmonized_robust_country, 
                                                     weights = ipw,
                                                     treatment = harmonized_robust_country$is_retirement, 
                                                     split.Rule = "CT", 
                                                     cv.option = "CT",
                                                     split.Honest = TRUE, cv.Honest = TRUE, split.Bucket = FALSE, 
                                                     maxdepth = 3,
                                                     minsplit = 2400, 
                                                     minbucket = 800)

## Feature Importance ----

opcp <- CausalTree_subjhealth_country$cptable[,1][which.min((CausalTree_subjhealth_country$cptable[,4]))]
opfit <- prune(CausalTree_subjhealth_country, opcp)

rpart.plot(opfit, box.palette = met.brewer("OKeeffe2", direction = 1, type = "continuous"), family = "Noto Sans", digits = -3, cex = 0.7)
