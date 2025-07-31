# Person-year ----

harmonized_population <- harmonized_longer |> 
  dplyr::filter((survey == "RAND_HRS" & wave <= 15) | 
                  (survey == "MHAS" & wave >= 3 & wave <= 5) | 
                  (survey == "SHARE" & wave <= 10) | 
                  (survey == "ELSA" & wave <= 9) | 
                  (survey == "CHARLS" & wave <= 4) | 
                  (survey == "KLoSA" & wave <= 7))

harmonized_population <- harmonized_population |> 
  dplyr::filter(is_participate == 1)

harmonized_population <- harmonized_population |> 
  dplyr::filter(Age >= 50 & Age <= 79)

# Construct an time level ----

harmonized_population <- harmonized_population |> 
  dplyr::group_by(id) |> 
  dplyr::mutate(lagged_work = lag(work)) |> 
  dplyr::mutate(is_retirement = dplyr::case_when(lagged_work == 1 & work == 0 ~ 1, 
                                                 lagged_work == 1 & work == 1 ~ 0, 
                                                 TRUE ~ 2), 
                is_retirement_alt = dplyr::case_when(lagged_work == 1 & (work == 0 & subjective_retirement == 1) ~ 1, 
                                                 lagged_work == 1 & work == 1 ~ 0, 
                                                 TRUE ~ 2)) |> 
  dplyr::select(-lagged_work) |> 
  dplyr::mutate(subjective_health_lead = dplyr::lead(subjective_health), 
                is_participate_t1 = dplyr::lead(is_participate),
                across(.cols = c(subjective_health, rabyear, Age, survey_year, ragender, raeducl, survey, marital_status, living_siblings, is_selfemployed, weekly_working_hours, num_livparents, father_age, mother_age, networth, total_household_income, transfer_to_grandchildren, household_number, living_children), 
                       .fns = ~ dplyr::lag(.x))
  ) |> 
  dplyr::ungroup()

harmonized_population <- harmonized_population |> 
  dplyr::filter(is_retirement != 2)

# Restrict the TP to those who retired from t-1 to t+1
harmonized_target <- harmonized_population |> 
  dplyr::select(subjective_health_lead, is_retirement, subjective_health, rabyear, Age, survey_year, ragender, raeducl, survey, marital_status, living_siblings, is_selfemployed, weekly_working_hours, num_livparents, father_age, mother_age, networth, total_household_income, transfer_to_grandchildren, household_number, living_children, country, networth, is_participate_t1)

harmonized_target <- harmonized_target |>
  dplyr::filter(is.na(is_participate_t1) == FALSE) |> 
  dplyr::select(-is_participate_t1)

harmonized_target <- harmonized_target |> 
  dplyr::group_by(country) |> 
  dplyr::mutate(income_percentile = ntile(total_household_income, 100),
                wealth_percentile = ntile(networth, 100)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::filter((wealth_percentile > 1 & wealth_percentile < 100) & (income_percentile > 1 & income_percentile < 100)) |>
  dplyr::filter((total_household_income >= 0))

harmonized_target |> 
  dplyr::mutate(surveys = survey) |> 
  dplyr::select(-country, -survey, -subjective_health) |> 
  dplyr::rename(`Subjective Health` = subjective_health_lead, 
                Female = ragender, 
                `Birth cohort` = rabyear, 
                `Marital status` = marital_status, 
                `Number of living siblings` = living_siblings, 
                `Survey year` = survey_year, 
                `Self-employment` = is_selfemployed, 
                `Weekly working hours` = weekly_working_hours, 
                `Education` = raeducl, 
                `Number of Living Parents` = num_livparents,
                `Age(father)` = father_age, 
                `Age(mother)` = mother_age, 
                `Household income` = total_household_income, 
                `Net worth` = networth, 
                `Transfer to grandchildren` = transfer_to_grandchildren, 
                `Total household number` = household_number, 
                `Number of living children` = living_children) |> 
  tidyr::pivot_longer(`Subjective Health`:`Number of living children`, 
                      names_to = "features", 
                      values_to = "values") |> 
  dplyr::reframe(across(.cols = everything(), 
                        .fns = ~ sum(is.na(.x) / length(.x))), 
                 .by = c(features, surveys)) |> 
  dplyr::mutate(not_missing = 1 - values) |> 
  dplyr::rename(is_missing = values) |> 
  tidyr::pivot_longer(c(is_missing, not_missing), 
                      names_to = "missing", 
                      values_to = "rate") |> 
  dplyr::mutate(missing = forcats::fct_relevel(missing, c("not_missing", "is_missing"))) |> 
  ggplot2::ggplot(aes(x = features, y = rate, fill = missing)) + 
  ggplot2::geom_col() + 
  ggplot2::coord_flip() + 
  ggplot2::facet_wrap(~ surveys) + 
  ggplot2::theme_minimal() + 
  ggplot2::theme(legend.position = "none") + 
  ggplot2::scale_fill_manual(values = c("grey", "#d8527c"))

harmonized_target |> 
  dplyr::reframe(n = n() / 133862, .by = survey) |> 
  dplyr::mutate(survey = forcats::fct_reorder(survey, n)) |> 
  ggplot2::ggplot(aes(x = n, y = survey)) + 
  ggplot2::geom_col()

harmonized_target <- harmonized_target |> 
  dplyr::mutate(ragender = fct_recode(factor(ragender), "Female" = "2", "Male" = "1"), 
                marital_status = fct_recode(factor(marital_status), "Married" = "1", "Partnered" = "2", "Separated/Divorced" = "3", "Widowed" = "4", "Never married" = "5"), 
                raeducl = fct_recode(factor(raeducl), "Less than upper secondary" = "1", "Upper secondary and vocational training" = "2", "Tertiary" = "3")) |> 
  dplyr::rename(`Subjective Health` = subjective_health_lead, 
                Female = ragender, 
                `Birth cohort` = rabyear, 
                `Marital status` = marital_status, 
                `Number of living siblings` = living_siblings, 
                `Survey year` = survey_year, 
                `Self-employment` = is_selfemployed, 
                `Weekly working hours` = weekly_working_hours, 
                `Education` = raeducl, 
                `Number of Living Parents` = num_livparents,
                `Age(father)` = father_age, 
                `Age(mother)` = mother_age, 
                `Household income` = total_household_income, 
                `Net worth` = networth, 
                `Transfer to grandchildren` = transfer_to_grandchildren, 
                `Total household number` = household_number, 
                `Number of living children` = living_children)

harmonized_target <- harmonized_target |> tidyr::drop_na()

# Construct IPW ----

harmonized_target <-
  glm(is_retirement ~ `Birth cohort` + Age + Female + `Marital status` + `Survey year` + `Self-employment` + `Weekly working hours` + `Education` + `Number of Living Parents` + `Age(father)` + `Age(mother)` + `Net worth` + `Household income` + `Transfer to grandchildren` + `Total household number` + `Number of living children` + `Number of living siblings` + subjective_health + country,
      family = binomial(link = "logit"), 
      data = harmonized_target) |> 
  predict(newx = predict, 
          type = "response") |> 
  tidyr::as_tibble() |> 
  (\(.) dplyr::bind_cols(harmonized_target, .))()

harmonized_target <-
  glm(is_retirement ~ 1,
      family = binomial(link = "logit"), 
      data = harmonized_target) |> 
  predict(newx = predict, 
          type = "response") |> 
  tidyr::as_tibble() |> 
  dplyr::rename(denom = value) |> 
  (\(.) dplyr::bind_cols(harmonized_target, .))()

harmonized_target <- harmonized_target |> 
  dplyr::mutate(ipw = dplyr::if_else(is_retirement == 1, denom / value, (1 - denom) / (1 - value)), 
                ps = 1 / value)



# Sample for robustness checks ----

harmonized_robust <- harmonized_population |> 
  dplyr::select(subjective_health_lead, is_retirement, is_retirement_alt, subjective_health, rabyear, Age, survey_year, ragender, raeducl, survey, marital_status, living_siblings, is_selfemployed, weekly_working_hours, num_livparents, father_age, mother_age, networth, total_household_income, transfer_to_grandchildren, household_number, living_children, country)

harmonized_robust <- harmonized_robust |> 
  dplyr::filter(is_retirement_alt != 2)

harmonized_robust <- harmonized_robust |>
  drop_na(subjective_health_lead)

harmonized_robust <- harmonized_robust |> 
  dplyr::mutate(ragender = fct_recode(factor(ragender), "Female" = "2", "Male" = "1"), 
                marital_status = fct_recode(factor(marital_status), "Married" = "1", "Partnered" = "2", "Separated/Divorced" = "3", "Widowed" = "4", "Never married" = "5"), 
                raeducl = fct_recode(factor(raeducl), "Less than upper secondary" = "1", "Upper secondary and vocational training" = "2", "Tertiary" = "3"),
                income_percentile = ntile(total_household_income, 100),
                wealth_percentile = ntile(networth, 100)
  ) |> 
  dplyr::filter(income_percentile > 1 & income_percentile < 100 & wealth_percentile > 1 & wealth_percentile < 100) |>
  dplyr::rename(`Subjective Health` = subjective_health_lead, 
                Female = ragender, 
                `Birth cohort` = rabyear, 
                `Marital status` = marital_status, 
                `Number of living siblings` = living_siblings, 
                `Survey year` = survey_year, 
                `Self-employment` = is_selfemployed, 
                `Weekly working hours` = weekly_working_hours, 
                `Education` = raeducl, 
                `Number of Living Parents` = num_livparents,
                `Age(father)` = father_age, 
                `Age(mother)` = mother_age, 
                `Net worth` = networth, 
                `Household income` = total_household_income, 
                `Transfer to grandchildren` = transfer_to_grandchildren, 
                `Total household number` = household_number, 
                `Number of living children` = living_children)

