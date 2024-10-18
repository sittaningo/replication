# Drop NA---------------------------------------------------------------
ssm_filtered <- ssm_filtered |> 
  dplyr::select(id, SubjHealth, ParentHighEdu, ChildCollege_Max, Female, BirthCohort, Age, College, Jsei_Fj, EmpStatus_Fj, Unemp, MarStatus, PastHealthIssue) |> 
  tidyr::drop_na()

# Descriptive Statistics------------------------------------------------
DescriptiveStatistics <- ssm_filtered |> 
  dplyr::mutate(`Subjective Health` = SubjHealth, 
                `Parental College` = ParentHighEdu, 
                `Child College` = forcats::fct_recode(factor(ChildCollege_Max), "Non-college" = "0", "College" = "1"), 
                `Birth Cohort` = BirthCohort,
                `JSEI at the first job` = Jsei_Fj, 
                `Employment status at the first job` = EmpStatus_Fj, 
                `Unemployment` = Unemp, 
                `Marital status` = MarStatus, 
                `Job left by health issue` = PastHealthIssue) |> 
  (\(.)modelsummary::datasummary(`Subjective Health` + `Parental College` + `Child College` + Female + `Birth Cohort` + Age + College + `JSEI at the first job` + `Employment status at the first job` + `Unemployment` + `Marital status` + `Job left by health issue` ~ 1 + Percent() + Mean + SD, data = ., output = 'markdown'))()

DescriptiveStatistics

# Heatmap by Parent's and Child's College on Health----------------------

ssm_filtered |> 
  dplyr::select(ParentHighEdu, ChildCollege_Max, SubjHealth, Female) |> 
  dplyr::mutate(ChildCollege_Max = fct_recode(factor(ChildCollege_Max), "Noncollege" = "0", "College or more" = "1")) |> 
  dplyr::group_by(ParentHighEdu, ChildCollege_Max, Female) |> 
  dplyr::summarise(Health = round(mean(SubjHealth), 2)) |> 
  ggplot2::ggplot(ggplot2::aes(x = ParentHighEdu, y = ChildCollege_Max, fill = Health, label = Health)) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
  ggplot2::geom_text(size = 10, color = "grey20") + 
  ggplot2::facet_wrap(~ Female) + 
  ggplot2::labs(x = "Parent's College", y = "Child's College", title = "Health by Parent's and Child's College") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))

DescriptiveStatistics

# ssm_filtered |> 
#   dplyr::select(-id) |> 
#   gtsummary::tbl_summary(label = list(SubjHealth ~ "Subjective Health", 
#                                       FatherCollege ~ "Father College", 
#                                       ChildCollege_Max ~ "Child College", 
#                                       Female ~ "Female", 
#                                       BirthCohort ~ "Birth Cohort", 
#                                       Age ~ "Age", 
#                                       Jsei_Fj ~ "Jsei at the First Job", 
#                                       EmpStatus_Fj ~ "Employment Status at the First Job", 
#                                       Unemp = "Ever Unemployed", 
#                                       MarStatus = "Marital Status", 
#                                       PastHealthIssue = "Ever Health Problems"),
#                          statistic = list(all_continuous() ~ "{mean} ({sd})", 
#                                           all_categorical() ~ "{p}"), 
#                          digits = list(all_continuous() ~ 2, 
#                                        all_categorical() ~ 2)) |> 
#   gtsummary::as_gt() |> 
#   gt::gtsave(filename = "fig/my_table_image.png")
#   gt::gtsave(filename = "fig/DiscriptiveStatistics.html")