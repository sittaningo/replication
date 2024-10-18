
# Esimate overall --------------------------------------------------------------

estimate_overall <- ssm_filtered |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "gam",
    outcome_algorithm = "gam",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

## Extract tibble of factural and counterfactual estimates----------------------
Result <- estimate_overall$factual_disparities |> 
  dplyr::mutate(Estimates = "Factural")

Result <- estimate_overall$counterfactual_disparities |> 
  dplyr::mutate(Estimates = "Counteractural") |> 
  dplyr::bind_rows(Result)

Prop_GapClosing <- estimate_overall$change_disparities |> 
  dplyr::filter(change_type == "proportional") |> 
  dplyr::select(estimate) |> 
  dplyr::rename(Prop_Gc = estimate)

Prop_GapClosing <- estimate_overall$change_disparities |> 
  dplyr::filter(change_type == "proportional") |> 
  dplyr::select(estimate) |> 
  dplyr::rename(Prop_Gc = estimate) |> 
  dplyr::bind_rows(Prop_GapClosing)

Result <- Prop_GapClosing |> 
  dplyr::bind_cols(Result)

## Visualize the results -------------------------------------------------------

Disparityplot <- Result |> 
  dplyr::filter(ParentHighEdu == "High - Low") |> 
  dplyr::mutate(Estimates = forcats::fct_relevel(Estimates, "Factural", "Counteractural"), 
                Prop_Gc = round(Prop_Gc * 100, 2)) |> 
  ggplot2::ggplot() + 
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::geom_pointrange(aes(x = Estimates, y = estimate, ymin = ci.min, ymax = ci.max)) + 
  ggplot2::geom_line(ggplot2::aes(x = 1.75,
                                  y = estimate),
                     position = ggplot2::position_dodge(width = .1),
                     linewidth = .5) + 
  ggplot2::geom_segment(aes(x = Estimates, xend = 1.75, 
                            y = estimate, yend = estimate), 
                        linetype = "dashed",
                        position = ggplot2::position_dodge(width = .1),
                        size = .5) +
  ggplot2::geom_text(aes(x = 1.75, label = paste0(Prop_Gc, "%"),
                         y = mean(estimate), hjust = 2, vjust = -3),
                     position = ggplot2::position_dodge(width = .1),
                     size = 6,
                     show.legend = FALSE) +
  ggplot2::labs(x = "Settings", y = "High - Low") + 
  ggplot2::theme_minimal(base_family = "SourceHanSans-Regular", 
                         base_size = 18)

Disparityplot

## Save the results ------------------------------------------------------------
ggsave("../fig/GapClosingEstimate_Overall.png", Disparityplot)


# Esimate by sex ---------------------------------------------------------------

estimate_male <- ssm_filtered |> 
  dplyr::filter(Female == "Male") |> 
  gapclosing(
    counterfactual_assignments = 1,
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu * ChildCollege_Max),
  treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
  category_name = "ParentHighEdu",
  treatment_algorithm = "gam",
  outcome_algorithm = "gam",
  sample_split = "single_sample",
  se = TRUE,
  bootstrap_samples = 1000,
  parallel_cores = 10
)

estimate_female <- ssm_filtered |> 
  dplyr::filter(Female == "Female") |> 
  gapclosing(
    counterfactual_assignments = 1,
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu * ChildCollege_Max),
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    category_name = "ParentHighEdu",
    treatment_algorithm = "gam",
    outcome_algorithm = "gam",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

## Extract tibble of factural and counterfactual estimates----------------------
Result <- estimate_male$factual_disparities |> 
  dplyr::mutate(Estimates = "Factural", 
                Sex = "Male")

Result <- estimate_male$counterfactual_disparities |> 
  dplyr::mutate(Estimates = "Counteractural", 
                Sex = "Male") |> 
  dplyr::bind_rows(Result)

Result <- estimate_female$factual_disparities |> 
  dplyr::mutate(Estimates = "Factural", 
                Sex = "Female") |> 
  dplyr::bind_rows(Result)

Result <- estimate_female$counterfactual_disparities |> 
  dplyr::mutate(Estimates = "Counteractural", 
                Sex = "Female") |> 
  dplyr::bind_rows(Result)


Prop_GapClosing <- estimate_male$change_disparities |> 
  dplyr::filter(change_type == "proportional") |> 
  dplyr::select(estimate) |> 
  dplyr::rename(Prop_Gc = estimate)

Prop_GapClosing <- estimate_male$change_disparities |> 
  dplyr::filter(change_type == "proportional") |> 
  dplyr::select(estimate) |> 
  dplyr::rename(Prop_Gc = estimate) |> 
dplyr::bind_rows(Prop_GapClosing)

Prop_GapClosing <- estimate_female$change_disparities |> 
  dplyr::filter(change_type == "proportional") |> 
  dplyr::select(estimate) |> 
  dplyr::rename(Prop_Gc = estimate) |> 
  dplyr::bind_rows(Prop_GapClosing)

Prop_GapClosing <- estimate_female$change_disparities |> 
  dplyr::filter(change_type == "proportional") |> 
  dplyr::select(estimate) |> 
  dplyr::rename(Prop_Gc = estimate) |> 
  dplyr::bind_rows(Prop_GapClosing)

Result <- Prop_GapClosing |> 
  dplyr::bind_cols(Result)

## Visualize the results -------------------------------------------------------

Disparityplot <- Result |> 
  dplyr::filter(ParentHighEdu == "High - Low") |> 
  dplyr::mutate(Estimates = forcats::fct_relevel(Estimates, "Factural", "Counteractural"), 
                Sex = forcats::fct_relevel(Sex, "Male", "Female"), 
                Prop_Gc = round(Prop_Gc * 100, 2)) |> 
  ggplot2::ggplot() + 
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::geom_pointrange(aes(x = Estimates, y = estimate, ymin = ci.min, ymax = ci.max)) + 
  ggplot2::geom_line(ggplot2::aes(x = 1.75,
                                  y = estimate),
                     position = ggplot2::position_dodge(width = .1),
                     linewidth = .5) + 
  ggplot2::geom_segment(aes(x = Estimates, xend = 1.75, 
                            y = estimate, yend = estimate), 
                        linetype = "dashed",
                        position = ggplot2::position_dodge(width = .1),
                        size = .5) +
  ggplot2::geom_text(aes(x = 1.75, label = paste0(Prop_Gc, "%"),
                                  y = mean(estimate), hjust = 1.2, vjust = -1.5),
                     position = ggplot2::position_dodge(width = .1),
                     size = 6,
                     show.legend = FALSE) +
  ggplot2::facet_wrap(~Sex) +
  ggplot2::labs(x = "Settings", y = "High - Low") + 
  ggplot2::theme_minimal(base_family = "SourceHanSans-Regular", 
                         base_size = 18)

Disparityplot

## Save the results ------------------------------------------------------------
ggsave("../fig/GapClosingEstimate_Sex.png", Disparityplot)
