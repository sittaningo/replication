Py <- Py |> dplyr::filter(Age >= 20)

Py_filtered <- Py |> 
  dplyr::select(id, Age, Anal, Female, Edu, GDP, UnempRate) |> 
  dplyr::mutate(Age = labelled::unlabelled(Age)) |> 
  tidyr::drop_na() |> 
  jtools::gscale(vars = "Anal", n.sd = 1)

# Descriptive Statistics----
DescriptiveStatistics <- Py_filtered |> 
  dplyr::rename(`Analytical Skill Usage` = Anal, 
                Gender = Female, 
                Education = Edu, 
                `Unemployment Rate` = UnempRate) |> 
  (\(.) modelsummary::datasummary(`Analytical Skill Usage` + Age + Gender + Education + GDP + `Unemployment Rate` + 1 ~ N + Percent() + Mean + SD,
                                  data = .,
                                  output = "markdown"))()

# ONET Item----

OnetItem <- tibble(Skill = c("Analytical", "", "", ""), 
                   Item = c("Active Learning", "Critical Thinking", "Learning Strategies", "Complex Problem Solving")) |> 
  knitr::kable(format = "markdown")

# Skill Ranking----

SkillRank_Prepare <- Py |> 
  dplyr::select(occ, Anal) |>
  dplyr::group_by(occ) |>
  dplyr::mutate(n = n()) |> 
  dplyr::ungroup() |> 
  dplyr::distinct(occ, .keep_all = TRUE)

# Analytical
SkillRank <- SkillRank_Prepare |> 
  dplyr::select(occ, Anal) |> 
  dplyr::slice_max(Anal, n = 5) |> 
  dplyr::mutate(Rank = 1:5)

SkillRank <- SkillRank_Prepare |> 
  dplyr::select(occ, Anal) |> 
  dplyr::slice_min(Anal, n = 5) |> 
  dplyr::mutate(Rank = 10:6) |> 
  dplyr::bind_rows(SkillRank)

# Tidy

ssm_occ <- ssm_occ |> 
  dplyr::rename(ssm_occnum = ssm)

onet_ssm_occ <- ssm_occ |> 
  (\(.) dplyr::left_join(onet_ssm_occ, ., by = "ssm_occnum"))()

SkillRank <- onet_ssm_occ |> 
  dplyr::select(ssm_occnum, title_en) |> 
  dplyr::rename(occ = ssm_occnum, Occ = title_en) |>
  (\(.) dplyr::inner_join(., SkillRank, by = "occ"))()

SkillRank_out <- SkillRank |> 
  dplyr::select(-occ) |> 
  dplyr::mutate(Occ = stringr::str_to_title(Occ, locale = "en"), 
                Score = round(Anal, 2)
                ) |>
  dplyr::select(Occ, Score) |> 
  dplyr::arrange(-Score) |> 
  tidyr::drop_na() |> 
  dplyr::distinct(.keep_all = TRUE)

SkillRank_out |>
  knitr::kable(format = "markdown")
