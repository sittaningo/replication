# Constract panel data ----

harmonized_longer <- harmonized |> 
  dplyr::select(id, survey, inw1, inw2, inw3, inw4, inw5, inw6, inw7, inw8, inw9, inw10, inw11, inw12, inw13, inw14, inw15) |> 
  tidyr::pivot_longer(cols = inw1:inw15, 
                      names_to = "wave",
                      values_to = "is_participate"
  ) |> 
  dplyr::mutate(wave = readr::parse_number(wave)) |> 
  tidyr::drop_na(wave)

# Outcome----

## Subjective Health ----
harmonized_longer <- harmonized |> 
  dplyr::select(id, (starts_with("r") & ends_with("shlt"))) |> 
  tidyr::pivot_longer(cols = (starts_with("r") & ends_with("shlt")), 
                      names_to = "wave",
                      values_to = "subjective_health"
  ) |> 
  dplyr::mutate(wave = readr::parse_number(wave)) |> 
  tidyr::drop_na(wave) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("id", "wave")))()

# Survey year ----

harmonized_longer <- harmonized_longer |> 
  dplyr::full_join(wave_year_correspondence, by = c("survey", "wave"))

# Country ----
# Parents' age/age at death: These variables indicate the respondent’s parents’ current age in years if the parents are still alive or the respondent’s parents’ age at death
# RwDADAGE

harmonized_longer <- harmonized |> 
  dplyr::select(id, survey, isocountry) |> 
  dplyr::mutate(country = dplyr::case_when(survey == "RAND_HRS" ~ "US", 
                                           survey == "ELSA" ~ "UK", 
                                           survey == "MHAS" ~ "Mexico", 
                                           survey == "KLoSA" ~ "Korea", 
                                           survey == "CHARLS" ~ "China", 
                                           isocountry == 40L ~ "Austria", 
                                           isocountry == 56L ~ "Belgium", 
                                           isocountry == 100L ~ "Bulgaria", 
                                           isocountry == 191L ~ "Croatia", 
                                           isocountry == 196L ~ "Cyprus", 
                                           isocountry == 203L ~ "Czech Republic", 
                                           isocountry == 208L ~ "Denmark", 
                                           isocountry == 233L ~ "Estonia", 
                                           isocountry == 246L ~ "Finland", 
                                           isocountry == 250L ~ "France", 
                                           isocountry == 276L ~ "Germany", 
                                           isocountry == 300L ~ "Greece", 
                                           isocountry == 348L ~ "Hungary", 
                                           isocountry == 372L ~ "Ireland", 
                                           isocountry == 376L ~ "Israel", 
                                           isocountry == 380L ~ "Italy", 
                                           isocountry == 428L ~ "Latvia", 
                                           isocountry == 440L ~ "Lithuania", 
                                           isocountry == 442L ~ "Luxembourg", 
                                           isocountry == 470L ~ "Malta", 
                                           isocountry == 528L ~ "Netherlands", 
                                           isocountry == 616L ~ "Poland", 
                                           isocountry == 620L ~ "Portugal", 
                                           isocountry == 642L ~ "Romania", 
                                           isocountry == 703L ~ "Slovakia", 
                                           isocountry == 705L ~ "Slovenia", 
                                           isocountry == 724L ~ "Spain", 
                                           isocountry == 752L ~ "Sweden", 
                                           isocountry == 756L ~ "Switzerland")) |> 
  dplyr::select(-survey, -isocountry) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("id")))()

# Treatment ----

## Retirement ----
# Retirement status and timing: These variables indicate whether the respondent is currently retired or not and the timing of their retirement.
# RwRETEMP

harmonized_longer <- harmonized |> 
  dplyr::select(id, (starts_with("r") & ends_with("work"))) |> 
  tidyr::pivot_longer(cols = (starts_with("r") & ends_with("work")), 
                      names_to = "wave",
                      values_to = "work") |> 
  dplyr::mutate(wave = readr::parse_number(wave)) |> 
  tidyr::drop_na(wave) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("id", "wave")))()

harmonized_longer <- harmonized |> 
  dplyr::select(id, (starts_with("r") & ends_with("retemp"))) |> 
  tidyr::pivot_longer(cols = (starts_with("r") & ends_with("retemp")), 
                      names_to = "wave",
                      values_to = "subjective_retirement") |> 
  dplyr::mutate(wave = readr::parse_number(wave)) |> 
  tidyr::drop_na(wave) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("id", "wave")))()


# Modifier ----

## Birth date ----
# Birth date variables capture the respondent’s birth year and month
# RABYEAR

harmonized_longer <- harmonized |> 
  dplyr::select(id, rabyear) |> 
  (\(.) dplyr::full_join(harmonized_longer, ., by = c("id")))()

## Age at Interview ----

harmonized_longer <- harmonized_longer |> 
  dplyr::mutate(Age = survey_year - rabyear)

## Gender ----

harmonized_longer <- harmonized |> 
  dplyr::select(id, ragender) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("id")))()

## Harmonized Education ----
# This variable indicates the harmonized education level
# RAEDUCL

harmonized_longer <- harmonized |> 
  dplyr::select(id, raeducl) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("id")))()


## Current marital status ----
# Current marital status variables capture the current marital status and length of the current marriage.
# RwMSTAT

harmonized_longer <- harmonized |> 
  dplyr::select(id, (starts_with("r") & ends_with("mstat"))) |> 
  tidyr::pivot_longer(cols = (starts_with("r") & ends_with("mstat")), 
                      names_to = "wave",
                      values_to = "marital_status") |> 
  dplyr::mutate(wave = readr::parse_number(wave)) |> 
  tidyr::drop_na(wave) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("id", "wave")))()

harmonized_longer <- harmonized_longer |> 
  dplyr::mutate(marital_status = case_match(marital_status, 
                                            1:2 ~ 1L, 
                                            3 ~ 2L, 
                                            4:6 ~ 3L, 
                                            7 ~ 4L, 
                                            8 ~ 5L, 
                                            .default = NA_integer_))

## Living siblings ----
# These variables indicate the number of the respondent’s living siblings.
# RwLIVSIB

harmonized_longer <- harmonized |> 
  dplyr::select(id, (starts_with("r") & ends_with("livsib"))) |> 
  tidyr::pivot_longer(cols = (starts_with("r") & ends_with("livsib")), 
                      names_to = "wave",
                      values_to = "living_siblings") |> 
  dplyr::mutate(wave = readr::parse_number(wave)) |> 
  tidyr::drop_na(wave) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("id", "wave")))()

## Whether self-employed ----
# These variables indicate whether the respondent is currently self-employed as their current main job
# RwSLFEMP

harmonized_longer <- harmonized |> 
  dplyr::select(id, (starts_with("r") & ends_with("slfemp"))) |> 
  tidyr::pivot_longer(cols = (starts_with("r") & ends_with("slfemp")), 
                      names_to = "wave",
                      values_to = "is_selfemployed") |> 
  dplyr::mutate(wave = readr::parse_number(wave)) |> 
  tidyr::drop_na(wave) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("id", "wave")))()

## Hours of work per week at main job ----
# These variables represent the usual number of hours per week the respondent works at their main job.
# RwJHOURS

harmonized_longer <- harmonized |> 
  dplyr::select(id, (starts_with("r") & ends_with("jhours"))) |> 
  tidyr::pivot_longer(cols = (starts_with("r") & ends_with("jhours")), 
                      names_to = "wave",
                      values_to = "weekly_working_hours_a") |> 
  dplyr::mutate(wave = readr::parse_number(wave)) |> 
  tidyr::drop_na(wave) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("id", "wave")))()

harmonized_longer <- harmonized |> 
  dplyr::select(id, (starts_with("r") & ends_with("jhours_c"))) |> 
  tidyr::pivot_longer(cols = (starts_with("r") & ends_with("jhours_c")), 
                      names_to = "wave",
                      values_to = "weekly_working_hours_b") |> 
  dplyr::mutate(wave = readr::parse_number(wave)) |> 
  tidyr::drop_na(wave) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("id", "wave")))()

harmonized_longer <- harmonized_longer |>
  dplyr::mutate(weekly_working_hours = dplyr::if_else(survey == "CHARLS", weekly_working_hours_b, weekly_working_hours_a)) |>
  dplyr::select(-weekly_working_hours_a, -weekly_working_hours_b)


## Number of living parents ----
# Living parents: These variables indicate the respondent’s number of the living parents
# RwLIVPAR

harmonized_longer <- harmonized |> 
  dplyr::select(id, (starts_with("r") & ends_with("livpar"))) |> 
  tidyr::pivot_longer(cols = (starts_with("r") & ends_with("livpar")), 
                      names_to = "wave",
                      values_to = "num_livparents") |> 
  dplyr::mutate(wave = readr::parse_number(wave)) |> 
  tidyr::drop_na(wave) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("id", "wave")))()

## Father's age - current/at death ----
# Parents' age/age at death: These variables indicate the respondent’s parents’ current age in years if the parents are still alive or the respondent’s parents’ age at death
# RwDADAGE

harmonized_longer <- harmonized |> 
  dplyr::select(id, (starts_with("r") & ends_with("dadage"))) |> 
  tidyr::pivot_longer(cols = (starts_with("r") & ends_with("dadage")), 
                      names_to = "wave",
                      values_to = "father_age") |> 
  dplyr::mutate(wave = readr::parse_number(wave)) |> 
  tidyr::drop_na(wave) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("id", "wave")))()

## Mother's age - current/at death ----
# Parents' age/age at death: These variables indicate the respondent’s parents’ current age in years if the parents are still alive or the respondent’s parents’ age at death
# RwDADAGE

harmonized_longer <- harmonized |> 
  dplyr::select(id, (starts_with("r") & ends_with("momage"))) |> 
  tidyr::pivot_longer(cols = (starts_with("r") & ends_with("momage")), 
                      names_to = "wave",
                      values_to = "mother_age") |> 
  dplyr::mutate(wave = readr::parse_number(wave)) |> 
  tidyr::drop_na(wave) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("id", "wave")))()

## Household ----

harmonized_longer <- harmonized |>
  dplyr::select(id, survey, hhid) |>
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("id", "survey")))()

### Housing Wealth ----

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("h") & ends_with("ahous"))) |>
  tidyr::pivot_longer(cols = (starts_with("h") & ends_with("ahous")),
                      names_to = "wave",
                      values_to = "housingwealth_a") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, housingwealth_a) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("hh") & ends_with("ahous"))) |>
  tidyr::pivot_longer(cols = (starts_with("hh") & ends_with("ahous")),
                      names_to = "wave",
                      values_to = "housingwealth_b") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, housingwealth_b) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("r") & ends_with("ahous"))) |>
  tidyr::pivot_longer(cols = (starts_with("r") & ends_with("ahous")),
                      names_to = "wave",
                      values_to = "housingwealth_c") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, housingwealth_c) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("s") & ends_with("ahous"))) |>
  tidyr::pivot_longer(cols = (starts_with("s") & ends_with("ahous")),
                      names_to = "wave",
                      values_to = "housingwealth_d") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, housingwealth_d) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized_longer |> 
  tidyr::replace_na(list(housingwealth_c = 0, housingwealth_d = 0)) |> 
  dplyr::mutate(housingwealth_e = housingwealth_d + housingwealth_d) |> 
  dplyr::select(-housingwealth_c, -housingwealth_d)

harmonized_longer <- harmonized_longer |>
  dplyr::mutate(housingwealth = dplyr::case_when(survey == "CHARLS" & wave < 4 ~ housingwealth_b,
                                               survey == "SHARE" ~ housingwealth_b,
                                               survey == "KLoSA" ~ housingwealth_e, 
                                               TRUE ~ housingwealth_a)) |>
  dplyr::select(-housingwealth_a, -housingwealth_b, -housingwealth_e)

### Other Real Estate ----

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("h") & ends_with("arles"))) |>
  tidyr::pivot_longer(cols = (starts_with("h") & ends_with("arles")),
                      names_to = "wave",
                      values_to = "otherhousingwealth_a") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, otherhousingwealth_a) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("hh") & ends_with("arles"))) |>
  tidyr::pivot_longer(cols = (starts_with("hh") & ends_with("arles")),
                      names_to = "wave",
                      values_to = "otherhousingwealth_b") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, otherhousingwealth_b) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("r") & ends_with("arles"))) |>
  tidyr::pivot_longer(cols = (starts_with("r") & ends_with("arles")),
                      names_to = "wave",
                      values_to = "otherhousingwealth_c") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, otherhousingwealth_c) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("s") & ends_with("arles"))) |>
  tidyr::pivot_longer(cols = (starts_with("s") & ends_with("arles")),
                      names_to = "wave",
                      values_to = "otherhousingwealth_d") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, otherhousingwealth_d) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized_longer |> 
  tidyr::replace_na(list(otherhousingwealth_c = 0, otherhousingwealth_d = 0)) |> 
  dplyr::mutate(otherhousingwealth_e = otherhousingwealth_c + otherhousingwealth_d) |> 
  dplyr::select(-otherhousingwealth_c, -otherhousingwealth_d)

harmonized_longer <- harmonized_longer |>
  dplyr::mutate(otherhousingwealth = dplyr::case_when(survey == "CHARLS" & wave < 3 ~ otherhousingwealth_b,
                                               survey == "SHARE" ~ otherhousingwealth_b,
                                               survey == "KLoSA" ~ otherhousingwealth_e, 
                                               TRUE ~ otherhousingwealth_a)) |>
  dplyr::select(-otherhousingwealth_a, -otherhousingwealth_b, -otherhousingwealth_e)

### Financial Wealth ----

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("h") & ends_with("astck"))) |>
  tidyr::pivot_longer(cols = (starts_with("h") & ends_with("astck")),
                      names_to = "wave",
                      values_to = "financialwealth_a") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, financialwealth_a) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("r") & ends_with("astck"))) |>
  tidyr::pivot_longer(cols = (starts_with("r") & ends_with("astck")),
                      names_to = "wave",
                      values_to = "financialwealth_b") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, financialwealth_b) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("s") & ends_with("astck"))) |>
  tidyr::pivot_longer(cols = (starts_with("s") & ends_with("astck")),
                      names_to = "wave",
                      values_to = "financialwealth_c") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, financialwealth_c) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized_longer |> 
  tidyr::replace_na(list(financialwealth_b = 0, financialwealth_c = 0)) |> 
  dplyr::mutate(financialwealth_d = financialwealth_b + financialwealth_c) |> 
  dplyr::select(-financialwealth_b, -financialwealth_c)

harmonized_longer <- harmonized_longer |>
  dplyr::mutate(financialwealth_1 = dplyr::case_when(survey == "SHARE" | survey == "MHAS" ~ NA_real_, 
                                                     survey == "KLoSA" ~ financialwealth_d, 
                                                     TRUE ~ financialwealth_a)) |>
  dplyr::select(-financialwealth_a, -financialwealth_d)

### Financial Wealth 2 ----

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("h") & ends_with("abond"))) |>
  tidyr::pivot_longer(cols = (starts_with("h") & ends_with("abond")),
                      names_to = "wave",
                      values_to = "financialwealth_a") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, financialwealth_a) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("r") & ends_with("abond"))) |>
  tidyr::pivot_longer(cols = (starts_with("r") & ends_with("abond")),
                      names_to = "wave",
                      values_to = "financialwealth_b") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, financialwealth_b) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("s") & ends_with("abond"))) |>
  tidyr::pivot_longer(cols = (starts_with("s") & ends_with("abond")),
                      names_to = "wave",
                      values_to = "financialwealth_c") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, financialwealth_c) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized_longer |> 
  tidyr::replace_na(list(housingwealth_c = 0, financialwealth_c = 0)) |> 
  dplyr::mutate(financialwealth_d = financialwealth_b + financialwealth_c) |> 
  dplyr::select(-financialwealth_b, -financialwealth_c)

harmonized_longer <- harmonized_longer |>
  dplyr::mutate(financialwealth_2 = dplyr::case_when(survey == "SHARE" | survey == "MHAS" ~ NA_real_, 
                                                     survey == "KLoSA" ~ financialwealth_d, 
                                                     TRUE ~ financialwealth_a)) |>
  dplyr::select(-financialwealth_a, -financialwealth_d)

### Financial Wealth 3 ----

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("hh") & ends_with("abdstk"))) |>
  tidyr::pivot_longer(cols = (starts_with("hh") & ends_with("abdstk")),
                      names_to = "wave",
                      values_to = "financialwealth_a") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, financialwealth_a) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("h") & ends_with("abdstk"))) |>
  tidyr::pivot_longer(cols = (starts_with("h") & ends_with("abdstk")),
                      names_to = "wave",
                      values_to = "financialwealth_b") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, financialwealth_b) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized_longer |>
  dplyr::mutate(financialwealth_3 = dplyr::case_when(survey == "SHARE" ~ financialwealth_a,
                                                     survey == "MHAS" ~ financialwealth_b, 
                                                     TRUE ~ NA_real_)) |>
  dplyr::select(-financialwealth_a, -financialwealth_b)

### Financial Wealth 4 ----

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("h") & ends_with("achck"))) |>
  tidyr::pivot_longer(cols = (starts_with("h") & ends_with("achck")),
                      names_to = "wave",
                      values_to = "financialwealth_a") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, financialwealth_a) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("hh") & ends_with("achck"))) |>
  tidyr::pivot_longer(cols = (starts_with("hh") & ends_with("achck")),
                      names_to = "wave",
                      values_to = "financialwealth_b") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, financialwealth_b) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("r") & ends_with("achck"))) |>
  tidyr::pivot_longer(cols = (starts_with("r") & ends_with("achck")),
                      names_to = "wave",
                      values_to = "financialwealth_c") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, financialwealth_c) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("s") & ends_with("achck"))) |>
  tidyr::pivot_longer(cols = (starts_with("s") & ends_with("achck")),
                      names_to = "wave",
                      values_to = "financialwealth_d") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, financialwealth_d) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized_longer |> 
  tidyr::replace_na(list(financialwealth_c = 0, financialwealth_d = 0)) |> 
  dplyr::mutate(financialwealth_e = financialwealth_c + financialwealth_d) |> 
  dplyr::select(-financialwealth_c, -financialwealth_d)

harmonized_longer <- harmonized_longer |>
  dplyr::mutate(financialwealth_4 = dplyr::case_when(survey == "SHARE" ~ financialwealth_b, 
                                                     survey == "KLoSA" ~ financialwealth_e, 
                                                     TRUE ~ financialwealth_a)) |>
  dplyr::select(-financialwealth_a, -financialwealth_b, -financialwealth_e)

### Combine all financial wealth ----

harmonized_longer <- harmonized_longer |> 
  dplyr::mutate(financialwealth = dplyr::case_when(is.na(financialwealth_1) == FALSE & is.na(financialwealth_2) == TRUE & is.na(financialwealth_4) == TRUE ~ financialwealth_1,
                                                   is.na(financialwealth_1) == TRUE & is.na(financialwealth_2) == FALSE & is.na(financialwealth_4) == TRUE ~  financialwealth_2,
                                                   is.na(financialwealth_1) == TRUE & is.na(financialwealth_2) == TRUE & is.na(financialwealth_4) == FALSE ~  financialwealth_4,
                                                   is.na(financialwealth_1) == FALSE & is.na(financialwealth_2) == FALSE & is.na(financialwealth_4) == TRUE ~ financialwealth_1 + financialwealth_2, 
                                                   is.na(financialwealth_1) == FALSE & is.na(financialwealth_2) == TRUE & is.na(financialwealth_4) == FALSE ~ financialwealth_1 + financialwealth_4, 
                                                   is.na(financialwealth_1) == TRUE & is.na(financialwealth_2) == FALSE & is.na(financialwealth_4) == FALSE ~ financialwealth_2 + financialwealth_4, 
                                                   is.na(financialwealth_1) == FALSE & is.na(financialwealth_2) == FALSE & is.na(financialwealth_4) == FALSE ~ financialwealth_1 + financialwealth_2 + financialwealth_4, 
                                                   is.na(financialwealth_3) == FALSE & is.na(financialwealth_4) == FALSE ~ financialwealth_3 + financialwealth_4, 
                                                   is.na(financialwealth_3) == TRUE & is.na(financialwealth_4) == FALSE ~ financialwealth_4, 
                                                   is.na(financialwealth_3) == FALSE & is.na(financialwealth_4) == TRUE ~ financialwealth_3, 
                                                   is.na(financialwealth_3) == TRUE & is.na(financialwealth_4) == TRUE ~ NA_real_, 
                                                   is.na(financialwealth_1) == TRUE & is.na(financialwealth_2) == TRUE & is.na(financialwealth_4) == TRUE ~  NA_real_))

### Debt ----

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("h") & ends_with("adebt"))) |>
  tidyr::pivot_longer(cols = (starts_with("h") & ends_with("adebt")),
                      names_to = "wave",
                      values_to = "debt_a") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, debt_a) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("hh") & ends_with("adebt"))) |>
  tidyr::pivot_longer(cols = (starts_with("hh") & ends_with("adebt")),
                      names_to = "wave",
                      values_to = "debt_b") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, debt_b) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("r") & ends_with("adebt"))) |>
  tidyr::pivot_longer(cols = (starts_with("r") & ends_with("adebt")),
                      names_to = "wave",
                      values_to = "debt_c") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, debt_c) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("s") & ends_with("adebt"))) |>
  tidyr::pivot_longer(cols = (starts_with("s") & ends_with("adebt")),
                      names_to = "wave",
                      values_to = "debt_d") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  dplyr::select(survey, hhid, wave, debt_d) |> 
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("hhid", "survey", "wave")))()

harmonized_longer <- harmonized_longer |> 
  tidyr::replace_na(list(debt_c = 0, debt_d = 0)) |> 
  dplyr::mutate(debt_e = debt_c + debt_d) |> 
  dplyr::select(-debt_c, -debt_d)

harmonized_longer <- harmonized_longer |>
  dplyr::mutate(debt = dplyr::case_when(survey == "SHARE" ~ debt_b,
                                                      survey == "KLoSA" ~ debt_e, 
                                                      TRUE ~ debt_a)) |>
  dplyr::select(-debt_a, -debt_b, -debt_e)

### Total wealth ----

harmonized_longer <- harmonized_longer |> 
  dplyr::mutate(networth = housingwealth + otherhousingwealth + financialwealth - debt
                # , networth = networth * currency
                )

### Total Household Income ----
# Hwitot

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("h") & ends_with("itot"))) |>
  tidyr::pivot_longer(cols = (starts_with("h") & ends_with("itot")),
                      names_to = "wave",
                      values_to = "total_household_income_a") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::select(survey, hhid, wave, total_household_income_a) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  (\(.) dplyr::left_join(harmonized_longer, ., 
                         by = c( "hhid", "survey", "wave"), 
                         na_matches = "never"))()

harmonized_longer <- harmonized |>
  dplyr::select(-(starts_with("hh") & ends_with("ittot"))) |>
  dplyr::select(id, (starts_with("h") & ends_with("ittot"))) |>
  tidyr::pivot_longer(cols = (starts_with("h") & ends_with("ittot")),
                      names_to = "wave",
                      values_to = "total_household_income_b") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("id", "wave")))()

harmonized_longer <- harmonized |>
  dplyr::select(hhid, survey, (starts_with("hh") & ends_with("itot"))) |>
  tidyr::pivot_longer(cols = (starts_with("hh") & ends_with("itot")),
                      names_to = "wave",
                      values_to = "total_household_income_c") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::select(hhid, survey, wave, total_household_income_c) |>
  dplyr::distinct(pick(hhid, survey, wave), .keep_all = TRUE) |>
  (\(.) dplyr::left_join(harmonized_longer, ., 
                         by = c( "hhid", "survey", "wave"), 
                         na_matches = "never"))()

harmonized_longer <- harmonized |>
  dplyr::select(id, (starts_with("r") & ends_with("itot"))) |>
  tidyr::pivot_longer(cols = (starts_with("r") & ends_with("itot")),
                      names_to = "wave",
                      values_to = "total_household_income_d") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  tidyr::drop_na(wave) |>
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("id", "wave")))()

harmonized_longer <- harmonized |>
  dplyr::select(id, (starts_with("s") & ends_with("itot"))) |>
  tidyr::pivot_longer(cols = (starts_with("s") & ends_with("itot")),
                      names_to = "wave",
                      values_to = "total_household_income_e") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  tidyr::drop_na(wave) |>
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("id", "wave")))()

harmonized_longer <- harmonized_longer |>
  tidyr::replace_na(list(total_household_income_d = 0, total_household_income_e = 0)) |> 
  dplyr::mutate(total_household_income_f = total_household_income_d + total_household_income_e) |> 
  dplyr::select(-total_household_income_d, -total_household_income_e) |> 
  dplyr::mutate(total_household_income = dplyr::case_when(survey == "SHARE" & wave >= 2 ~ total_household_income_b,
                                                          survey == "CHARLS" ~ total_household_income_c,
                                                            survey == "KLoSA" ~ total_household_income_f,
                                                            TRUE ~ total_household_income_a)) |>
  dplyr::select(-total_household_income_a, -total_household_income_b, -total_household_income_c, -total_household_income_f)

# harmonized_longer <- harmonized_longer |>
#   dplyr::mutate(total_household_income = total_household_income * currency)

### Household Capital Income ----
# These variables represent the capital incomes of the respondents’ households.
# HwICAP

# harmonized_longer <- harmonized |>
#   dplyr::select(survey, hhid, (starts_with("h") & ends_with("icap"))) |>
#   tidyr::pivot_longer(cols = (starts_with("h") & ends_with("icap")),
#                       names_to = "wave",
#                       values_to = "household_capital_income_a") |>
#   dplyr::mutate(wave = readr::parse_number(wave)) |>
#   dplyr::select(survey, hhid, wave, household_capital_income_a) |>
#   dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
#   (\(.) dplyr::left_join(harmonized_longer, ., 
#                          by = c( "hhid", "survey", "wave"), 
#                          na_matches = "never"))()
# 
# harmonized_longer <- harmonized |>
#   dplyr::select(id, (starts_with("r") & ends_with("icap"))) |>
#   tidyr::pivot_longer(cols = (starts_with("r") & ends_with("icap")),
#                       names_to = "wave",
#                       values_to = "household_capital_income_b") |>
#   dplyr::mutate(wave = readr::parse_number(wave)) |>
#   (\(.) dplyr::left_join(harmonized_longer, ., by = c("id", "wave")))()
# 
# harmonized_longer <- harmonized |>
#   dplyr::select(hhid, survey, (starts_with("hh") & ends_with("icap"))) |>
#   tidyr::pivot_longer(cols = (starts_with("hh") & ends_with("icap")),
#                       names_to = "wave",
#                       values_to = "household_capital_income_c") |>
#   dplyr::mutate(wave = readr::parse_number(wave)) |>
#   dplyr::select(hhid, survey, wave, household_capital_income_c) |>
#   dplyr::distinct(pick(hhid, survey, wave), .keep_all = TRUE) |>
#   (\(.) dplyr::left_join(harmonized_longer, ., 
#                          by = c( "hhid", "survey", "wave"), 
#                          na_matches = "never"))()
# 
# harmonized_longer <- harmonized |>
#   dplyr::select(hhid, survey, (starts_with("h") & ends_with("ittot"))) |>
#   tidyr::pivot_longer(cols = (starts_with("h") & ends_with("ittot")),
#                       names_to = "wave",
#                       values_to = "household_capital_income_d") |>
#   dplyr::mutate(wave = readr::parse_number(wave)) |>
#   dplyr::select(hhid, survey, wave, household_capital_income_d) |>
#   dplyr::distinct(pick(hhid, survey, wave), .keep_all = TRUE) |>
#   (\(.) dplyr::left_join(harmonized_longer, ., 
#                          by = c( "hhid", "survey", "wave"), 
#                          na_matches = "never"))()
# 
# harmonized_longer <- harmonized_longer |>
#   dplyr::mutate(household_capital_income = dplyr::case_when(survey == "KLoSA" ~ household_capital_income_b,
#                                                             survey == "CHARLS" ~ household_capital_income_c,
#                                                             survey == "SHARE" ~ household_capital_income_d,
#                                                             TRUE ~ household_capital_income_a)) |>
#   dplyr::select(-household_capital_income_a, -household_capital_income_b, -household_capital_income_c, -household_capital_income_d)

### Transfers to children/grandchildren ----
# These variables provide information about any financial help the respondent gave their children or grandchildren.
# HwTCANY

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("h") & ends_with("tcany"))) |>
  tidyr::pivot_longer(cols = (starts_with("h") & ends_with("tcany")),
                      names_to = "wave",
                      values_to = "transfer_to_grandchildren") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::select(survey, hhid, wave, transfer_to_grandchildren) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  (\(.) dplyr::left_join(harmonized_longer, ., 
                         by = c( "hhid", "survey", "wave"), 
                         na_matches = "never"))()

### Number of people ----
# These variables indicate the number of people living in the household, including the respondents.
# HwHHRES

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("h") & ends_with("hhres"))) |>
  tidyr::pivot_longer(cols = (starts_with("h") & ends_with("hhres")),
                      names_to = "wave",
                      values_to = "household_number_a") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::select(survey, hhid, wave, household_number_a) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  (\(.) dplyr::left_join(harmonized_longer, ., 
                         by = c( "hhid", "survey", "wave"), 
                         na_matches = "never"))()

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("hh") & ends_with("hhres"))) |>
  tidyr::pivot_longer(cols = (starts_with("hh") & ends_with("hhres")),
                      names_to = "wave",
                      values_to = "household_number_b") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::select(survey, hhid, wave, household_number_b) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  (\(.) dplyr::left_join(harmonized_longer, ., 
                         by = c( "hhid", "survey", "wave"), 
                         na_matches = "never"))()

harmonized_longer <- harmonized_longer |>
  dplyr::mutate(household_number = dplyr::if_else(survey == "SHARE", household_number_b, household_number_a)) |>
  dplyr::select(-household_number_a, -household_number_b)

### Living children ----
# These variables indicate the number of living children of the respondent and his/her spouse or partner.
# HwCHILD

harmonized_longer <- harmonized |>
  dplyr::select(survey, hhid, (starts_with("h") & ends_with("child"))) |>
  tidyr::pivot_longer(cols = (starts_with("h") & ends_with("child")),
                      names_to = "wave",
                      values_to = "living_children_a") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  dplyr::select(survey, hhid, wave, living_children_a) |>
  dplyr::distinct(pick(survey, hhid, wave), .keep_all = TRUE) |>
  (\(.) dplyr::left_join(harmonized_longer, ., 
                         by = c( "hhid", "survey", "wave"), 
                         na_matches = "never"))()

harmonized_longer <- harmonized |>
  dplyr::select(id, (starts_with("r") & ends_with("child"))) |>
  tidyr::pivot_longer(cols = (starts_with("r") & ends_with("child")),
                      names_to = "wave",
                      values_to = "living_children_b") |>
  dplyr::mutate(wave = readr::parse_number(wave)) |>
  tidyr::drop_na(wave) |>
  (\(.) dplyr::left_join(harmonized_longer, ., by = c("id", "wave")))()

harmonized_longer <- harmonized_longer |>
  dplyr::mutate(living_children = dplyr::if_else(survey == "ELSA", living_children_b, living_children_a)) |>
  dplyr::select(-living_children_a, -living_children_b)

