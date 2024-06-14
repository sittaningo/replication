
# Preparation----

Analyze <- Py_filtered |> 
  dplyr::mutate(Age = factor(Age, levels = c(25, 20:24, 26:69))) |> 
  tidyr::drop_na()

Range <- c("20" = "20", 
           "21" = "", 
           "22" = "", 
           "23" = "", 
           "24" = "", 
           "25" = "25",
           "26" = "", 
           "27" = "", 
           "28" = "", 
           "29" = "", 
           "30" = "30", 
           "31" = "", 
           "32" = "", 
           "33" = "", 
           "34" = "", 
           "35" = "35", 
           "36" = "", 
           "37" = "", 
           "38" = "", 
           "39" = "", 
           "40" = "40", 
           "41" = "", 
           "42" = "", 
           "43" = "", 
           "44" = "", 
           "45" = "45", 
           "46" = "", 
           "47" = "", 
           "48" = "", 
           "49" = "", 
           "50" = "50", 
           "51" = "", 
           "52" = "", 
           "53" = "", 
           "54" = "", 
           "55" = "55", 
           "56" = "", 
           "57" = "", 
           "58" = "", 
           "59" = "", 
           "60" = "60", 
           "61" = "", 
           "62" = "", 
           "63" = "", 
           "64" = "", 
           "65" = "65",
           "66" = "", 
           "67" = "", 
           "68" = "", 
           "69" = "")

each <- c("20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69")

# Overall ----

## Fixed Effect Estimation ----

### Overall----
Overall_Coef <- Analyze |> 
  (\(.) fixest::feols(Anal ~ GDP + UnempRate + Age | id , 
                                   data = .,
                                   cluster = ~id, se = "cluster"))()
SkillTraj_Overall <- Overall_Coef |> 
  marginaleffects::predictions(by = "Age") |> 
  dplyr::mutate(Age = factor(Age, levels = each))

### Gender----
Gender_Coef <- Analyze |> 
  (\(.) fixest::feols(Anal ~ Age*Female + GDP + UnempRate | id, data = ., cluster = ~id + Age, se = "cluster"))()

SkillTraj_Gender <- Gender_Coef |> 
  marginaleffects::predictions(by = c("Age", "Female")) |> 
  dplyr::mutate(Age = factor(Age, levels = each))

### Education----
Edu_Coef <- Analyze |> 
  (\(.) fixest::feols(Anal ~ Age*Edu + GDP + UnempRate | id, data = ., cluster = ~id, se = "cluster"))()

SkillTraj_Edu <- Edu_Coef |> 
  marginaleffects::predictions(by = c("Age", "Edu")) |> 
  dplyr::mutate(Age = factor(Age, levels = each))

### Gender x Education----
EduGender_Coef <- Analyze |> 
  (\(.) fixest::feols(Anal ~ Age*Female*Edu + GDP + UnempRate | id, data = ., cluster = ~id + Age, se = "cluster"))()

SkillTraj_EduGender <- EduGender_Coef |> 
  marginaleffects::predictions(by = c("Age", "Female", "Edu")) |> 
  dplyr::mutate(Age = factor(Age, levels = each))
  
## Plot Volatility ----

### Overall----
Volatility_Overall <- SkillTraj_Overall |> 
  ggplot2::ggplot(aes(x = Age, y = std.error^2, group = 1)) + 
  ggplot2::geom_point(shape = 15) + 
  ggplot2::geom_line() + 
  ggplot2::theme_minimal(base_family = "SourceHanSans-Regular", 
                         base_size = 24) + 
  ggplot2::theme(legend.position = "top", 
                 legend.title = element_blank()) + 
  ggplot2::scale_x_discrete(labels = Range) + 
  ggplot2::labs(y = "Variance")

### Gender----
Volatility_Gender <- SkillTraj_Gender |> 
  ggplot2::ggplot(aes(x = Age, y = std.error^2, color = Female, group = Female)) + 
  ggplot2::geom_point(shape = 15) + 
  ggplot2::geom_line() + 
  ggplot2::scale_color_manual(values = met.brewer("Kandinsky", 2, type = "discrete")) +
  ggplot2::theme_minimal(base_family = "SourceHanSans-Regular", 
                         base_size = 24) + 
  ggplot2::theme(legend.position = "top", 
                 legend.title = element_blank()) + 
  ggplot2::scale_x_discrete(labels = Range) + 
  # ggplot2::scale_y_continuous(limits = c(-0.5, 1.5, 0.5)) + 
  ggplot2::labs(y = "Variance")

### Education----
Volatility_Edu <- SkillTraj_Edu |> 
  ggplot2::ggplot(aes(x = Age, y = std.error^2, shape = Edu, group = Edu)) + 
  ggplot2::geom_point() + 
  ggplot2::geom_line() + 
  ggplot2::theme_minimal(base_family = "SourceHanSans-Regular", 
                         base_size = 24) + 
  ggplot2::theme(legend.position = "top", 
                 legend.title = element_blank()) + 
  ggplot2::scale_x_discrete(labels = Range) + 
  # ggplot2::scale_y_continuous(limits = c(-0.5, 1.5, 0.5)) + 
  ggplot2::labs(y = "Variance")

### Gender x Education----
Volatility_EduGender <- SkillTraj_EduGender |> 
  ggplot2::ggplot(aes(x = Age, y = std.error^2, color = Female, shape = Edu, group = paste0(Female, Edu))) + 
  ggplot2::geom_point() + 
  ggplot2::geom_line() + 
  scale_shape_manual(name = "", values = c(17, 16)) +
  scale_color_manual(name = "", 
                     values = c("#3b7c70", "#ce9642")) + 
  guides(color = guide_legend(ncol=2), 
         shape = guide_legend(ncol=2)) +
  ggplot2::theme_minimal(base_family = "SourceHanSans-Regular", 
                         base_size = 24) + 
  ggplot2::theme(legend.position = "top", 
                 legend.title = element_blank()) + 
  ggplot2::scale_x_discrete(labels = Range) + 
  # ggplot2::scale_y_continuous(limits = c(-0.5, 1.5, 0.5)) + 
  ggplot2::labs(y = "Variance")

## Plot Coefficient ----

### Overall----
Coef_Overall <- SkillTraj_Overall |> 
  dplyr::mutate(Age = factor(Age, levels = each)) |>
  ggplot2::ggplot(aes(x = Age, y = estimate, ymin = conf.low, ymax = conf.high)) + 
  ggplot2::geom_pointrange(shape = 15) + 
  ggplot2::geom_line() + 
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::scale_color_manual(values = met.brewer("Kandinsky", 2, type = "discrete")) + 
  ggplot2::theme_minimal(base_family = "SourceHanSans-Regular", 
                         base_size = 24) + 
  ggplot2::theme(legend.position = "top", 
                 legend.title = element_blank()) + 
  ggplot2::scale_x_discrete(labels = Range)

### Gender----
Coef_Gender <- SkillTraj_Gender |> 
  ggplot2::ggplot(aes(x = Age, y = estimate, ymin = conf.low, ymax = conf.high, color = Female, group = Female)) + 
  ggplot2::geom_pointrange(shape = 15) + 
  ggplot2::geom_line() + 
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::scale_color_manual(values = met.brewer("Kandinsky", 2, type = "discrete")) + 
  ggplot2::theme_minimal(base_family = "SourceHanSans-Regular", 
                         base_size = 24) + 
  ggplot2::theme(legend.position = "top", 
                 legend.title = element_blank()) + 
  ggplot2::scale_x_discrete(labels = Range)
# + 
#   ggplot2::labs(title = "Gender")

### Education----

Coef_Edu <- SkillTraj_Edu |> 
  ggplot2::ggplot(aes(x = Age, y = estimate, ymin = conf.low, ymax = conf.high, shape = Edu, group = Edu)) + 
  ggplot2::geom_pointrange() + 
  ggplot2::geom_line() + 
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::theme_minimal(base_family = "SourceHanSans-Regular", 
                         base_size = 24) + 
  ggplot2::theme(legend.position = "top", 
                 legend.title = element_blank()) + 
  ggplot2::scale_x_discrete(labels = Range) + 
  ggplot2::scale_shape_manual(values=c(17, 16))
# + 
#   ggplot2::labs(title = "Education")

### Gender x Education----

Coef_EduGender <- SkillTraj_EduGender |> 
  ggplot2::ggplot(aes(x = Age, y = estimate, ymin = conf.low, ymax = conf.high, color = Female, shape = Edu, group = paste0(Female, Edu))) + 
  ggplot2::geom_pointrange() + 
  ggplot2::geom_line() + 
  scale_shape_manual(name = "", values = c(17, 16)) +
  scale_color_manual(name = "", 
                     values = c("#3b7c70", "#ce9642")) + 
  guides(color = guide_legend(ncol=2), 
         shape = guide_legend(ncol=2)) +
  ggplot2::theme_minimal(base_family = "SourceHanSans-Regular", 
                         base_size = 24) + 
  ggplot2::theme(legend.position = "top", 
                 legend.title = element_blank()) + 
  ggplot2::scale_x_discrete(labels = Range) + 
  ggplot2::scale_y_continuous(limits = c(-0.5, 1.5, 0.5))
# + 
#   ggplot2::labs(title = "Gender and Education")

# Modelsummary

Fetable <- modelsummary::modelsummary(list(Overall_Coef, Gender_Coef, Edu_Coef, EduGender_Coef),
                                    stars = c("*" = .05, "**" = .01),
                                    output = "markdown")

Coef_EduGender
