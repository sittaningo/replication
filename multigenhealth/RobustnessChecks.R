# Function-wide estimator ----

## Treatment Model ----

### GLM ----

est_tr_glm <- ssm_filtered |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    category_name = "ParentHighEdu",
    treatment_algorithm = "glm",
    outcome_name = "SubjHealth",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_tr_glm <- est_tr_glm$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GLM", 
                Outcome = "NULL")


### Ridge ----

est_tr_ridge <- ssm_filtered |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    category_name = "ParentHighEdu",
    treatment_algorithm = "ridge",
    outcome_name = "SubjHealth",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_tr_ridge <- est_tr_ridge$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "Ridge", 
                Outcome = "NULL")

### GAM ----

est_tr_gam <- ssm_filtered |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    category_name = "ParentHighEdu",
    treatment_algorithm = "gam",
    outcome_name = "SubjHealth",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_tr_gam <- est_tr_gam$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GAM", 
                Outcome = "NULL")


### Random Forests ----

est_tr_ranger <- ssm_filtered |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    category_name = "ParentHighEdu",
    treatment_algorithm = "ranger",
    outcome_name = "SubjHealth",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_tr_ranger <- est_tr_ranger$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "Random_Forest", 
                Outcome = "NULL")

## Outcome Model ----

### GLM ----

est_out_lm <- ssm_filtered |> 
  gapclosing(
    counterfactual_assignments = 1,
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    outcome_algorithm = "lm",
    treatment_name = "ChildCollege_Max",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_out_lm <- est_out_lm$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "NULL", 
                Outcome = "OLS")

### Ridge ----

est_out_ridge <- ssm_filtered |> 
  gapclosing(
    counterfactual_assignments = 1,
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    outcome_algorithm = "ridge",
    treatment_name = "ChildCollege_Max",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_out_ridge <- est_out_ridge$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "NULL", 
                Outcome = "Ridge")

### GAM ----

est_out_gam <- ssm_filtered |> 
  gapclosing(
    counterfactual_assignments = 1,
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    outcome_algorithm = "gam",
    treatment_name = "ChildCollege_Max",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_out_gam <- est_out_gam$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "NULL", 
                Outcome = "GAM")


## Doubly robust ----

### GLM ----

#### GLM/LM ----

est_dr_glm_lm <- ssm_filtered |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "glm",
    outcome_algorithm = "lm",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_glm_lm <- est_dr_glm_lm$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GLM", 
                Outcome = "OLS")

#### GLM/Ridge ----

est_dr_glm_ridge <- ssm_filtered |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "glm",
    outcome_algorithm = "ridge",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_glm_ridge <- est_dr_glm_ridge$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GLM", 
                Outcome = "Ridge")

#### GLM/GAM ----

est_dr_glm_gam <- ssm_filtered |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "glm",
    outcome_algorithm = "gam",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_glm_gam <- est_dr_glm_gam$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GLM", 
                Outcome = "GAM")


### Ridge ----

#### Ridge/LM----

est_dr_ridge_lm <- ssm_filtered |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "ridge",
    outcome_algorithm = "lm",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_ridge_lm <- est_dr_ridge_lm$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "Ridge", 
                Outcome = "OLS")

#### Ridge/Ridge----

est_dr_ridge_ridge <- ssm_filtered |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "ridge",
    outcome_algorithm = "ridge",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_ridge_ridge <- est_dr_ridge_ridge$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "Ridge", 
                Outcome = "Ridge")

#### Ridge/GAM----

est_dr_ridge_gam <- ssm_filtered |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "ridge",
    outcome_algorithm = "gam",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_ridge_gam <- est_dr_ridge_gam$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "Ridge", 
                Outcome = "GAM")


### GAM ----

#### GAM/LM ----

est_dr_gam_lm <- ssm_filtered |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "gam",
    outcome_algorithm = "lm",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_gam_lm <- est_dr_gam_lm$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GAM", 
                Outcome = "OLS")

#### GAM/Ridge ----

est_dr_gam_ridge <- ssm_filtered |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + Female + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "gam",
    outcome_algorithm = "ridge",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_gam_ridge <- est_dr_gam_ridge$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GAM", 
                Outcome = "Ridge")

#### GAM/GAM ----

est_dr_gam_gam <- ssm_filtered |> 
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

est_dr_gam_gam <- est_dr_gam_gam$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GAM", 
                Outcome = "GAM")

# Combine all estimator ----

function_multiverse <- dplyr::bind_rows(list(est_tr_glm, est_tr_ridge, est_tr_gam, est_out_lm, est_out_ridge, est_out_gam, est_dr_glm_lm, est_dr_glm_ridge, est_dr_glm_gam, est_dr_ridge_lm, est_dr_ridge_ridge, est_dr_ridge_gam, est_dr_gam_lm, est_dr_gam_ridge, est_dr_gam_gam))

function_multiverse <- function_multiverse |> 
  dplyr::mutate(estimate = round(estimate*100, 1), 
                Treatment = forcats::fct_relevel(Treatment, c("NULL", "GLM", "Ridge", "GAM")), 
                Outcome = forcats::fct_relevel(Outcome, c("NULL", "OLS", "Ridge", "GAM"))) |> 
  ggplot2::ggplot(ggplot2::aes(x = Treatment, y = Outcome, fill = estimate, label = estimate)) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
  ggplot2::geom_text(size = 10, color = "grey20") + 
  ggplot2::theme_minimal(base_family = "SourceHanSans-Regular", 
                         base_size = 18) + 
  ggplot2::theme(legend.position="none")

ggsave("../fig/multiverse_function.png", function_multiverse)

# MALE | Function-wide estimator ----

ssm_filtered_female <- ssm_filtered |> dplyr::filter(Female == "Male")

## Treatment Model ----

### GLM ----

est_tr_glm <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    category_name = "ParentHighEdu",
    treatment_algorithm = "glm",
    outcome_name = "SubjHealth",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_tr_glm <- est_tr_glm$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GLM", 
                Outcome = "NULL")


### Ridge ----

est_tr_ridge <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    category_name = "ParentHighEdu",
    treatment_algorithm = "ridge",
    outcome_name = "SubjHealth",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_tr_ridge <- est_tr_ridge$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "Ridge", 
                Outcome = "NULL")

### GAM ----

est_tr_gam <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    category_name = "ParentHighEdu",
    treatment_algorithm = "gam",
    outcome_name = "SubjHealth",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_tr_gam <- est_tr_gam$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GAM", 
                Outcome = "NULL")


### Random Forests ----

est_tr_ranger <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    category_name = "ParentHighEdu",
    treatment_algorithm = "ranger",
    outcome_name = "SubjHealth",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_tr_ranger <- est_tr_ranger$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "Random_Forest", 
                Outcome = "NULL")

## Outcome Model ----

### GLM ----

est_out_lm <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    outcome_algorithm = "lm",
    treatment_name = "ChildCollege_Max",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_out_lm <- est_out_lm$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "NULL", 
                Outcome = "OLS")

### Ridge ----

est_out_ridge <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    outcome_algorithm = "ridge",
    treatment_name = "ChildCollege_Max",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_out_ridge <- est_out_ridge$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "NULL", 
                Outcome = "Ridge")

### GAM ----

est_out_gam <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    outcome_algorithm = "gam",
    treatment_name = "ChildCollege_Max",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_out_gam <- est_out_gam$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "NULL", 
                Outcome = "GAM")


## Doubly robust ----

### GLM ----

#### GLM/LM ----

est_dr_glm_lm <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "glm",
    outcome_algorithm = "lm",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_glm_lm <- est_dr_glm_lm$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GLM", 
                Outcome = "OLS")

#### GLM/Ridge ----

est_dr_glm_ridge <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "glm",
    outcome_algorithm = "ridge",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_glm_ridge <- est_dr_glm_ridge$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GLM", 
                Outcome = "Ridge")

#### GLM/GAM ----

est_dr_glm_gam <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "glm",
    outcome_algorithm = "gam",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_glm_gam <- est_dr_glm_gam$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GLM", 
                Outcome = "GAM")


### Ridge ----

#### Ridge/LM----

est_dr_ridge_lm <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "ridge",
    outcome_algorithm = "lm",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_ridge_lm <- est_dr_ridge_lm$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "Ridge", 
                Outcome = "OLS")

#### Ridge/Ridge----

est_dr_ridge_ridge <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "ridge",
    outcome_algorithm = "ridge",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_ridge_ridge <- est_dr_ridge_ridge$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "Ridge", 
                Outcome = "Ridge")

#### Ridge/GAM----

est_dr_ridge_gam <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "ridge",
    outcome_algorithm = "gam",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_ridge_gam <- est_dr_ridge_gam$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "Ridge", 
                Outcome = "GAM")


### GAM ----

#### GAM/LM ----

est_dr_gam_lm <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "gam",
    outcome_algorithm = "lm",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_gam_lm <- est_dr_gam_lm$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GAM", 
                Outcome = "OLS")

#### GAM/Ridge ----

est_dr_gam_ridge <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "gam",
    outcome_algorithm = "ridge",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_gam_ridge <- est_dr_gam_ridge$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GAM", 
                Outcome = "Ridge")

#### GAM/GAM ----

est_dr_gam_gam <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "gam",
    outcome_algorithm = "gam",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_gam_gam <- est_dr_gam_gam$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GAM", 
                Outcome = "GAM")

# Combine all estimator ----

function_multiverse <- dplyr::bind_rows(list(est_tr_glm, est_tr_ridge, est_tr_gam, est_out_lm, est_out_ridge, est_out_gam, est_dr_glm_lm, est_dr_glm_ridge, est_dr_glm_gam, est_dr_ridge_lm, est_dr_ridge_ridge, est_dr_ridge_gam, est_dr_gam_lm, est_dr_gam_ridge, est_dr_gam_gam))

function_multiverse <- function_multiverse |> 
  dplyr::mutate(estimate = round(estimate*100, 1), 
                Treatment = forcats::fct_relevel(Treatment, c("NULL", "GLM", "Ridge", "GAM")), 
                Outcome = forcats::fct_relevel(Outcome, c("NULL", "OLS", "Ridge", "GAM"))) |> 
  ggplot2::ggplot(ggplot2::aes(x = Treatment, y = Outcome, fill = estimate, label = estimate)) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
  ggplot2::geom_text(size = 10, color = "grey20") + 
  ggplot2::theme_minimal(base_family = "SourceHanSans-Regular", 
                         base_size = 18) + 
  ggplot2::theme(legend.position="none") + 
  ggplot2::labs(title = "Male")

ggsave("../fig/functionalmultiverse_male.png", function_multiverse)

# FEMALE | Function-wide estimator ----

ssm_filtered_female <- ssm_filtered |> dplyr::filter(Female == "Female")

## Treatment Model ----

### GLM ----

est_tr_glm <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    category_name = "ParentHighEdu",
    treatment_algorithm = "glm",
    outcome_name = "SubjHealth",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_tr_glm <- est_tr_glm$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GLM", 
                Outcome = "NULL")


### Ridge ----

est_tr_ridge <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    category_name = "ParentHighEdu",
    treatment_algorithm = "ridge",
    outcome_name = "SubjHealth",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_tr_ridge <- est_tr_ridge$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "Ridge", 
                Outcome = "NULL")

### GAM ----

est_tr_gam <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    category_name = "ParentHighEdu",
    treatment_algorithm = "gam",
    outcome_name = "SubjHealth",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_tr_gam <- est_tr_gam$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GAM", 
                Outcome = "NULL")


### Random Forests ----

est_tr_ranger <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    category_name = "ParentHighEdu",
    treatment_algorithm = "ranger",
    outcome_name = "SubjHealth",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_tr_ranger <- est_tr_ranger$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "Random_Forest", 
                Outcome = "NULL")

## Outcome Model ----

### GLM ----

est_out_lm <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    outcome_algorithm = "lm",
    treatment_name = "ChildCollege_Max",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_out_lm <- est_out_lm$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "NULL", 
                Outcome = "OLS")

### Ridge ----

est_out_ridge <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    outcome_algorithm = "ridge",
    treatment_name = "ChildCollege_Max",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_out_ridge <- est_out_ridge$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "NULL", 
                Outcome = "Ridge")

### GAM ----

est_out_gam <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    outcome_algorithm = "gam",
    treatment_name = "ChildCollege_Max",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_out_gam <- est_out_gam$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "NULL", 
                Outcome = "GAM")


## Doubly robust ----

### GLM ----

#### GLM/LM ----

est_dr_glm_lm <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "glm",
    outcome_algorithm = "lm",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_glm_lm <- est_dr_glm_lm$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GLM", 
                Outcome = "OLS")

#### GLM/Ridge ----

est_dr_glm_ridge <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "glm",
    outcome_algorithm = "ridge",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_glm_ridge <- est_dr_glm_ridge$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GLM", 
                Outcome = "Ridge")

#### GLM/GAM ----

est_dr_glm_gam <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "glm",
    outcome_algorithm = "gam",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_glm_gam <- est_dr_glm_gam$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GLM", 
                Outcome = "GAM")


### Ridge ----

#### Ridge/LM----

est_dr_ridge_lm <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "ridge",
    outcome_algorithm = "lm",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_ridge_lm <- est_dr_ridge_lm$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "Ridge", 
                Outcome = "OLS")

#### Ridge/Ridge----

est_dr_ridge_ridge <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "ridge",
    outcome_algorithm = "ridge",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_ridge_ridge <- est_dr_ridge_ridge$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "Ridge", 
                Outcome = "Ridge")

#### Ridge/GAM----

est_dr_ridge_gam <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "ridge",
    outcome_algorithm = "gam",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_ridge_gam <- est_dr_ridge_gam$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "Ridge", 
                Outcome = "GAM")


### GAM ----

#### GAM/LM ----

est_dr_gam_lm <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "gam",
    outcome_algorithm = "lm",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_gam_lm <- est_dr_gam_lm$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GAM", 
                Outcome = "OLS")

#### GAM/Ridge ----

est_dr_gam_ridge <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "gam",
    outcome_algorithm = "ridge",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_gam_ridge <- est_dr_gam_ridge$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GAM", 
                Outcome = "Ridge")

#### GAM/GAM ----

est_dr_gam_gam <- ssm_filtered_female |> 
  gapclosing(
    counterfactual_assignments = 1,
    treatment_formula = formula(ChildCollege_Max ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu),
    outcome_formula = formula(SubjHealth ~ BirthCohort + Age + College + Jsei_Fj + EmpStatus_Fj + Unemp + MarStatus + PastHealthIssue + ParentHighEdu + ChildCollege_Max),
    category_name = "ParentHighEdu",
    treatment_algorithm = "gam",
    outcome_algorithm = "gam",
    sample_split = "single_sample",
    se = TRUE,
    bootstrap_samples = 1000,
    parallel_cores = 10
  )

est_dr_gam_gam <- est_dr_gam_gam$change_disparities |> 
  dplyr::filter(change_type == "proportional" & ParentHighEdu == "High - Low") |> 
  dplyr::select(estimate) |> 
  dplyr::mutate(Treatment = "GAM", 
                Outcome = "GAM")

# Combine all estimator ----

function_multiverse <- dplyr::bind_rows(list(est_tr_glm, est_tr_ridge, est_tr_gam, est_out_lm, est_out_ridge, est_out_gam, est_dr_glm_lm, est_dr_glm_ridge, est_dr_glm_gam, est_dr_ridge_lm, est_dr_ridge_ridge, est_dr_ridge_gam, est_dr_gam_lm, est_dr_gam_ridge, est_dr_gam_gam))

function_multiverse <- function_multiverse |> 
  dplyr::mutate(estimate = round(estimate*100, 1), 
                Treatment = forcats::fct_relevel(Treatment, c("NULL", "GLM", "Ridge", "GAM")), 
                Outcome = forcats::fct_relevel(Outcome, c("NULL", "OLS", "Ridge", "GAM"))) |> 
  ggplot2::ggplot(ggplot2::aes(x = Treatment, y = Outcome, fill = estimate, label = estimate)) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
  ggplot2::geom_text(size = 10, color = "grey20") + 
  ggplot2::theme_minimal(base_family = "SourceHanSans-Regular", 
                         base_size = 18) + 
  ggplot2::theme(legend.position="none") + 
  ggplot2::labs(title = "Female")

ggsave("../fig/functionalmultiverse_female.png", function_multiverse)
