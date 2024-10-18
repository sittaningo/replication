
# Y/Outcome: Respondent health ----

ssm <- ssm |> 
  dplyr::mutate(SubjHealth = dplyr::case_match(dq20, 
                                               9:99 ~ NA_integer_, 
                                               .default = dq20), 
                SubjHealth = 6 - SubjHealth)

# A/Category: Parental education ----

ssm <- ssm |> 
  dplyr::mutate(FatherHighEdu = dplyr::case_match(q22_a,
                                                  c(1:5, 8:9) ~ 0,
                                                  c(6:7, 10:13) ~ 1,
                                                  c(14:99) ~ NA_integer_), 
                MotherHighEdu = dplyr::case_match(q22_b,
                                                  c(1:5, 8:9) ~ 0,
                                                  c(6:7, 10:13) ~ 1,
                                                  c(14:99) ~ NA_integer_), 
                ParentHighEdu = dplyr::case_when(FatherHighEdu == 1 & MotherHighEdu == 1 ~ 1, 
                                                 FatherHighEdu == 1 & MotherHighEdu == 0 ~ 1, 
                                                 FatherHighEdu == 0 & MotherHighEdu == 1 ~ 1, 
                                                 FatherHighEdu == 0 & MotherHighEdu == 0 ~ 0, 
                                                 FatherHighEdu == NA_integer_ & MotherHighEdu != NA_integer_ ~ MotherHighEdu, 
                                                 FatherHighEdu != NA_integer_ & MotherHighEdu == NA_integer_ ~ FatherHighEdu,
                                                 FatherHighEdu == NA_integer_ & MotherHighEdu == NA_integer_ ~ NA_integer_,
                                                 TRUE ~ NA_integer_), 
                ParentHighEdu = fct_recode(factor(ParentHighEdu), "Low" = "0", "High" = "1"))

# C/Treatment: Child education ----

## Child sex ----

ssm <- ssm |> 
  dplyr::select(id, dq13_1_1, dq13_2_1, dq13_3_1, dq13_4_1) |> 
  dplyr::rename(Sex_1 = dq13_1_1, 
                Sex_2 = dq13_2_1, 
                Sex_3 = dq13_3_1, 
                Sex_4 = dq13_4_1) |>
  tidyr::pivot_longer(-id, 
                      names_to = c("Sex", "BirthOrder"), 
                      names_sep = "_") |> 
  tidyr::pivot_wider(names_from = Sex, 
                     values_from = value) |>
  dplyr::mutate(ChildSex = dplyr::case_match(Sex,
                                        1 ~ 0L, 
                                        2 ~ 1L,
                                        8:9 ~ NA_integer_)) |>
  dplyr::select(id, ChildSex, BirthOrder) |>
  dplyr::mutate(Names = "ChildSex") |> 
  tidyr::pivot_wider(names_from = c("Names", "BirthOrder"), 
                     values_from = ChildSex) |> 
  dplyr::inner_join(ssm, ., by = "id")


## Child age ----

ssm <- ssm |> 
  dplyr::select(id, dq13_1_2a, dq13_2_2a, dq13_3_2a, dq13_4_2a, dq13_1_2b, dq13_2_2b, dq13_3_2b, dq13_4_2b) |> 
  dplyr::rename(Gengo_1 = dq13_1_2a, 
                Gengo_2 = dq13_2_2a, 
                Gengo_3 = dq13_3_2a, 
                Gengo_4 = dq13_4_2a, 
                Year_1 = dq13_1_2b, 
                Year_2 = dq13_2_2b, 
                Year_3 = dq13_3_2b, 
                Year_4 = dq13_4_2b) |>
  tidyr::pivot_longer(-id, 
                      names_to = c("Date", "BirthOrder"), 
                      names_sep = "_") |> 
  tidyr::pivot_wider(names_from = Date, 
                     values_from = value) |>
  dplyr::mutate(Gengo = dplyr::case_match(Gengo,
                                          1 ~ 1926,
                                          2 ~ 1989,
                                          c(8, 9) ~ NA_integer_), 
                Year = dplyr::case_match(Year,
                                         88:99 ~ NA_integer_, 
                                         .default = Year)) |>
  # tidyr::drop_na(Gengo, Year) |>
  dplyr::mutate(ChildBirth = Gengo + Year) |> 
  dplyr::mutate(ChildAge = 2015 - ChildBirth) |>
  dplyr::select(id, ChildAge, BirthOrder) |>
  dplyr::mutate(Names = "ChildAge") |> 
  tidyr::pivot_wider(names_from = c("Names", "BirthOrder"), 
                     values_from = ChildAge) |> 
  dplyr::inner_join(ssm, ., by = "id")

## Child age over 25
ssm <- ssm |> 
  dplyr::mutate(Child1_25 = dplyr::if_else(ChildAge_1 >= 25, 1, 0),
                Child2_25 = dplyr::if_else(ChildAge_2 >= 25, 1, 0),
                Child3_25 = dplyr::if_else(ChildAge_3 >= 25, 1, 0),
                Child4_25 = dplyr::if_else(ChildAge_4 >= 25, 1, 0))
# |> 
#   tidyr::replace_na(list(Child1_25 = 0, Child2_25 = 0, Child3_25 = 0, Child4_25 = 0))

## Education ----

ssm <- ssm |> 
  dplyr::select(id, dq13_1_5, dq13_2_5, dq13_3_5, dq13_4_5) |> 
  dplyr::rename(Education_1 = dq13_1_5, 
                Education_2 = dq13_2_5, 
                Education_3 = dq13_3_5, 
                Education_4 = dq13_4_5) |>
  tidyr::pivot_longer(-id, 
                      names_to = c("Edu", "BirthOrder"), 
                      names_sep = "_", 
                      values_to = "Education") |> 
  dplyr::select(-Edu) |>
  dplyr::mutate(Education = as.integer(Education), 
                College = dplyr::case_match(Education,
                                            0:5 ~ 0L,
                                            6:7 ~ 1L,
                                            8:99 ~ NA_integer_)) |>
  dplyr::select(id, College, BirthOrder) |>
  dplyr::mutate(Names = "ChildCollege") |> 
  tidyr::pivot_wider(names_from = c("Names", "BirthOrder"), 
                     values_from = College) |> 
  dplyr::inner_join(ssm, ., by = "id")

## The Highest Education of Children over 25 ----
### 25歳以上の子どもの学歴、複数いる場合は最高学歴を採用

ssm <- ssm |> 
  dplyr::mutate(ChildCollege_Max = dplyr::case_when(
    (ChildAge_4 >= 25 & ChildCollege_4 == 1L) | (ChildAge_3 >= 25 & ChildCollege_3 == 1L) | (ChildAge_2 >= 25 & ChildCollege_2 == 1L) | (ChildAge_1 >= 25 & ChildCollege_1 == 1L) ~ 1L, 
    TRUE ~ 0L))

# , 
#     CollegeMax_Child_BirthOrder = dplyr::case_when(
#       ChildCollege_Max == 1 & ChildCollege_4 == 1 & ChildAge_4 >= 25 ~ 4L,
#       ChildCollege_Max == 1 & ChildCollege_3 == 1 & ChildAge_3 >= 25 ~ 3L,
#       ChildCollege_Max == 1 & ChildCollege_2 == 1 & ChildAge_2 >= 25 ~ 2L,
#       ChildCollege_Max == 1 & ChildCollege_1 == 1 & ChildAge_1 >= 25 ~ 1L, 
#       TRUE ~ 0), 
#     CollegeMax_Child_Sex = dplyr::case_when(
#       CollegeMax_Child_BirthOrder == 1 ~ ChildSex_1, 
#       CollegeMax_Child_BirthOrder == 2 ~ ChildSex_2, 
#       CollegeMax_Child_BirthOrder == 3 ~ ChildSex_3, 
#       CollegeMax_Child_BirthOrder == 4 ~ ChildSex_4, 
#       ChildCollege_Max == 0 ~ ChildSex_1))

# Z_a/Treatment-mediator confounder: Respondent socdem ----

## Sex

ssm <- ssm |> 
  dplyr::mutate(Female = dplyr::case_match(q1_1, 
                                           1 ~ 1, 
                                           2 ~ 0, 
                                           .default = NA_integer_), 
                Female = forcats::fct_recode(factor(Female), 
                                             "Female" = "1", 
                                             "Male" = "0"))

## Birth cohort

ssm <- ssm |> 
  dplyr::mutate(BirthCohort = 2015 - q1_2_5)

## Age

ssm <- ssm |> 
  dplyr::mutate(Age = q1_2_5)

# R_a/Treatment-induced confounder: Respondent education ----

ssm <- ssm |> 
  dplyr::mutate(College = dplyr::case_match(edssm, 
                                              4:9 ~ "0", 
                                              10:11 ~ "1",  
                                              .default = NA_character_), 
                College = forcats::fct_recode(factor(College),  
                                              "College" = "1", 
                                              "Non-college" = "0"))

# R_b/Mediator-outcome confounder: Respondent life course ----

## Occupation at first job (JSEI)

ssm <- ssm |> 
  dplyr::mutate(Occ = q8_f) |> 
  dplyr::left_join(jsei, by = "Occ") |> 
  dplyr::rename(Jsei_Fj = sei) |> 
  dplyr::select(-ssi, -Occ)

## Employment Status at first job

ssm <- ssm |> 
  dplyr::mutate(EmpStatus_Fj = dplyr::case_match(q8_a, 
                                                2 ~ 0, 
                                                3:6 ~ 1,
                                                c(1, 7:8) ~ 2,
                                                .default = NA_integer_), 
                EmpStatus_Fj = forcats::fct_recode(factor(EmpStatus_Fj), 
                                                "Self-employed" = "2",
                                                "NonStandard" = "1", 
                                                "Standard" = "0"))

## Unemployment

ssm <- ssm |> 
  mutate(Unemp2 = dplyr::if_else(q9_2_b == 2 & (q9_2_c_7 < 60 & q9_2_c_8< 60), 1, 0), 
         Unemp3 = dplyr::if_else(q9_3_b == 2 & (q9_3_c_7 < 60 & q9_3_c_8< 60), 1, 0), 
         Unemp4 = dplyr::if_else(q9_4_b == 2 & (q9_4_c_7 < 60 & q9_4_c_8< 60), 1, 0), 
         Unemp5 = dplyr::if_else(q9_5_b == 2 & (q9_5_c_7 < 60 & q9_5_c_8< 60), 1, 0), 
         Unemp6 = dplyr::if_else(q9_6_b == 2 & (q9_6_c_7 < 60 & q9_6_c_8< 60), 1, 0), 
         Unemp7 = dplyr::if_else(q9_7_b == 2 & (q9_7_c_7 < 60 & q9_7_c_8< 60), 1, 0), 
         Unemp8 = dplyr::if_else(q9_8_b == 2 & (q9_8_c_7 < 60 & q9_8_c_8< 60), 1, 0), 
         Unemp9 = dplyr::if_else(q9_9_b == 2 & (q9_9_c_7 < 60 & q9_9_c_8< 60), 1, 0), 
         Unemp10 = dplyr::if_else(q9_10_b == 2 & (q9_10_c_7 < 60 & q9_10_c_8< 60), 1, 0), 
         Unemp11 = dplyr::if_else(q9_11_b == 2 & (q9_11_c_7 < 60 & q9_11_c_8< 60), 1, 0), 
         Unemp12 = dplyr::if_else(q9_12_b == 2 & (q9_12_c_7 < 60 & q9_12_c_8< 60), 1, 0), 
         Unemp13 = dplyr::if_else(q9_13_b == 2 & (q9_13_c_7 < 60 & q9_13_c_8< 60), 1, 0), 
         Unemp14 = dplyr::if_else(q9_14_b == 2 & (q9_14_c_7 < 60 & q9_14_c_8< 60), 1, 0), 
         Unemp15 = dplyr::if_else(q9_15_b == 2 & (q9_15_c_7 < 60 & q9_15_c_8< 60), 1, 0), 
         Unemp16 = dplyr::if_else(q9_16_b == 2 & (q9_16_c_7 < 60 & q9_16_c_8< 60), 1, 0), 
         Unemp17 = dplyr::if_else(q9_17_b == 2 & (q9_17_c_7 < 60 & q9_17_c_8< 60), 1, 0), 
         Unemp18 = dplyr::if_else(q9_18_b == 2 & (q9_18_c_7 < 60 & q9_18_c_8< 60), 1, 0), 
         Unemp22 = dplyr::if_else(q9_22_b == 2 & (q9_22_c_7 < 60 & q9_22_c_8< 60), 1, 0)) |> 
  tidyr::replace_na(list(Unemp2 = 0,
                    Unemp3 = 0,
                    Unemp4 = 0,
                    Unemp5 = 0,
                    Unemp6 = 0,
                    Unemp7 = 0,
                    Unemp8 = 0,
                    Unemp9 = 0,
                    Unemp10 = 0,
                    Unemp11 = 0,
                    Unemp12 = 0,
                    Unemp13 = 0,
                    Unemp14 = 0,
                    Unemp15 = 0,
                    Unemp16 = 0,
                    Unemp17 = 0,
                    Unemp18 = 0,
                    Unemp22 = 0)) |> 
  dplyr::mutate(SumUnemp = Unemp2 + Unemp3 + Unemp4 + Unemp5 + Unemp6 + Unemp7 + Unemp8 + Unemp9 + Unemp10 + Unemp11 + Unemp12 + Unemp13 + Unemp14 + Unemp15 + Unemp16 + Unemp17 + Unemp18 + Unemp22,
                Unemp = dplyr::if_else(SumUnemp >= 1, 1, 0), 
                Unemp = forcats::fct_recode(factor(Unemp), 
                                            "Ever-Unemployment" = "1", 
                                            "Never-Unemployment" = "0"))

## Divorce

ssm <- ssm |> 
  dplyr::mutate(MarStatus = dplyr::case_when(q25 == 1 ~ 0,
                                             q25 == 2 & q33 == 1 ~ 0,
                                             q25 == 2 & q33 == 2 ~ 1,
                                             q25 == 3 ~ 1,
                                             q25 == 4 ~ 1,
                                             TRUE ~ NA_integer_), 
                MarStatus = forcats::fct_recode(factor(MarStatus), 
                                            "Ever-DivorceWidow" = "1", 
                                            "Never-DivorceWidow" = "0"))


## Num. of children

ssm <- ssm |> 
  dplyr::mutate(ChildNum = dq12)

## Health problem

ssm <- ssm |> 
  dplyr::mutate(PastHealthIssue2 = dplyr::if_else(q9_2_b_1 == 7 | q9_2_b_1_9 == 13, 1, 0), 
                PastHealthIssue3 = dplyr::if_else(q9_3_b_1 == 7 | q9_3_b_1_9 == 13, 1, 0), 
                PastHealthIssue4 = dplyr::if_else(q9_4_b_1 == 7 | q9_4_b_1_9 == 13, 1, 0), 
                PastHealthIssue5 = dplyr::if_else(q9_5_b_1 == 7 | q9_5_b_1_9 == 13, 1, 0), 
                PastHealthIssue6 = dplyr::if_else(q9_6_b_1 == 7 | q9_6_b_1_9 == 13, 1, 0), 
                PastHealthIssue7 = dplyr::if_else(q9_7_b_1 == 7 | q9_7_b_1_9 == 13, 1, 0), 
                PastHealthIssue8 = dplyr::if_else(q9_8_b_1 == 7 | q9_8_b_1_9 == 13, 1, 0), 
                PastHealthIssue9 = dplyr::if_else(q9_9_b_1 == 7 | q9_9_b_1_9 == 13, 1, 0), 
                PastHealthIssue10 = dplyr::if_else(q9_10_b_1 == 7 | q9_10_b_1_9 == 13, 1, 0), 
                PastHealthIssue11 = dplyr::if_else(q9_11_b_1 == 7 | q9_11_b_1_9 == 13, 1, 0), 
                PastHealthIssue12 = dplyr::if_else(q9_12_b_1 == 7 | q9_12_b_1_9 == 13, 1, 0), 
                PastHealthIssue13 = dplyr::if_else(q9_13_b_1 == 7 | q9_13_b_1_9 == 13, 1, 0), 
                PastHealthIssue14 = dplyr::if_else(q9_14_b_1 == 7 | q9_14_b_1_9 == 13, 1, 0), 
                PastHealthIssue15 = dplyr::if_else(q9_15_b_1 == 7 | q9_15_b_1_9 == 13, 1, 0), 
                PastHealthIssue16 = dplyr::if_else(q9_16_b_1 == 7 | q9_16_b_1_9 == 13, 1, 0), 
                PastHealthIssue17 = dplyr::if_else(q9_17_b_1 == 7 | q9_17_b_1_9 == 13, 1, 0), 
                PastHealthIssue18 = dplyr::if_else(q9_18_b_1 == 7 | q9_18_b_1_9 == 13, 1, 0), 
                PastHealthIssue22 = dplyr::if_else(q9_22_b_1 == 7 | q9_22_b_1_9 == 13, 1, 0)) |> 
  tidyr::replace_na(list(PastHealthIssue2 = 0, 
                         PastHealthIssue3 = 0, 
                         PastHealthIssue4 = 0, 
                         PastHealthIssue5 = 0, 
                         PastHealthIssue6 = 0, 
                         PastHealthIssue7 = 0, 
                         PastHealthIssue8 = 0, 
                         PastHealthIssue9 = 0, 
                         PastHealthIssue10 = 0, 
                         PastHealthIssue11 = 0, 
                         PastHealthIssue12 = 0,
                         PastHealthIssue13 = 0,
                         PastHealthIssue14 = 0,
                         PastHealthIssue15 = 0,
                         PastHealthIssue16 = 0,
                         PastHealthIssue17 = 0,
                         PastHealthIssue18 = 0,
                         PastHealthIssue22 = 0)) |>
  dplyr::mutate(Sum_PastHealthIssue = PastHealthIssue2 + PastHealthIssue3 + PastHealthIssue4 + PastHealthIssue5 + PastHealthIssue6 + PastHealthIssue7 + PastHealthIssue8 + PastHealthIssue9 + PastHealthIssue10 + PastHealthIssue11 + PastHealthIssue12 + PastHealthIssue13 + PastHealthIssue14 + PastHealthIssue15 + PastHealthIssue16 + PastHealthIssue17 + PastHealthIssue18 + PastHealthIssue22, 
                PastHealthIssue = dplyr::if_else(Sum_PastHealthIssue >= 1, 1, 0), 
                PastHealthIssue = forcats::fct_recode(factor(PastHealthIssue),
                                                      "Ever-HealthIssue" = "1",
                                                      "Never-HealthIssue" = "0"))
