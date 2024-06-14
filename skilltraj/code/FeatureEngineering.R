# Y / Skill----

## (1) Constracting Person-Year Data----
age_dansu <- c(paste0("q9_", seq(1, 22, by = 1), "_c_7"))

Py <- d |> 
  dplyr::select(-dansu) |> 
  dplyr::rename(q9_1_c_7 = q8_h_1) |> 
  tidyr::pivot_longer(age_dansu, 
                      names_to = c("dansu", "suffix"), 
                      names_prefix = "q9_", 
                      names_sep = "_c_7", 
                      values_to = "num") |> 
  dplyr::group_by(id) |> 
  dplyr::select(id, num, dansu, age) |> 
  tidyr::complete(num = full_seq(10:81, 1)) |> 
  dplyr::mutate(dansu = as.numeric(dansu)) |> 
  tidyr::fill(dansu, .direction="down") |> 
  tidyr::fill(age, .direction = "down") |> 
  dplyr::ungroup() |> 
  group_by(id) |> 
  dplyr::distinct(num, .keep_all = TRUE) |>
  dplyr::filter(num >= 20 & num <= 69)

## (2) Occupation in each spell----

occ_dansu <- c(paste0("q9_", seq(1, 22, by = 1), "_c_5"))

Py <- d |> 
  dplyr::select(-dansu) |> 
  dplyr::rename(q9_1_c_5 = q8_f) |> 
  tidyr::pivot_longer(occ_dansu, 
                      names_to = c("dansu", "suffix"), 
                      names_prefix = "q9_", 
                      names_sep = "_c_5", 
                      values_to = "occ") |> 
  dplyr::select(id, dansu, occ) |> 
  dplyr::mutate(dansu = as.numeric(dansu)) |> # 型をexpandしたデータと一致させる
  (\(.) dplyr::left_join(Py, ., by = c("id", "dansu")))()

## (3) Skill in each spell----

### a. Extract items----

item <- onet |> 
  dplyr::filter(`1` == 16) |> 
  dplyr::select(-`1`, -`...2`) |> 
  unlist(use.names = FALSE)

# IPDの抽出
IPD <- onet |> 
  dplyr::filter(`1` == 17) |> 
  dplyr::select(-`1`, -`...2`) |> 
  unlist(use.names = FALSE)

onet_item <- tibble(item, IPD) # 項目とIPDの対応表

onet <- onet |>   dplyr::filter(`1` >= 18) |>
  dplyr::select(-`1`, -`...2`)


colname <- stats::setNames(names(onet), IPD)

onet <- onet |> 
  dplyr::rename(!!!colname)

# remove values
rm(IPD, item, colname)


### b. Matching SSM Occ - ONETJ Occ----

onet <- onet_ssm_occ |> 
  select(-onet_occlabel) |> 
  mutate(IPD_01_01_001 = as.character(onet_occnum))  |> 
  select(-onet_occnum) |> 
  inner_join(onet, ., by = "IPD_01_01_001")

### c. Constract skill measurement----

onet_komugi <- onet |> 
  dplyr::select(IPD_04_03_01_001, IPD_04_03_01_002, IPD_04_03_01_003, IPD_04_03_01_004, IPD_04_03_01_009, IPD_04_04_01_015, IPD_04_03_01_011, IPD_04_03_01_012, IPD_04_03_01_013, IPD_04_03_01_021, IPD_04_03_01_026, IPD_04_03_01_022, IPD_04_03_01_023, IPD_04_03_01_015, IPD_04_03_01_020, IPD_04_03_01_037, IPD_04_03_01_039, IPD_04_04_01_001, IPD_04_04_01_011, IPD_04_04_01_010, IPD_04_04_01_012, IPD_04_04_01_014, IPD_04_04_01_016, IPD_04_04_01_017, IPD_04_04_01_018, IPD_04_10_011, IPD_04_10_020, IPD_04_10_029, IPD_04_10_034, IPD_04_10_036, ssm_occnum) |> 
  dplyr::mutate(dplyr::across(everything(), as.numeric)) |> 
  dplyr::group_by(ssm_occnum) |> 
  dplyr::summarise(dplyr::across(
    c(IPD_04_03_01_001, IPD_04_03_01_002, IPD_04_03_01_003, IPD_04_03_01_004, IPD_04_03_01_009, IPD_04_04_01_015, IPD_04_03_01_011, IPD_04_03_01_012, IPD_04_03_01_013, IPD_04_03_01_021, IPD_04_03_01_026, IPD_04_03_01_022, IPD_04_03_01_023, IPD_04_03_01_015, IPD_04_03_01_020, IPD_04_03_01_037, IPD_04_03_01_039, IPD_04_04_01_001, IPD_04_04_01_011, IPD_04_04_01_010, IPD_04_04_01_012, IPD_04_04_01_014, IPD_04_04_01_016, IPD_04_04_01_017, IPD_04_04_01_018, IPD_04_10_011, IPD_04_10_020, IPD_04_10_029, IPD_04_10_034, IPD_04_10_036), 
    \(.) mean(., na.rm = TRUE)) ,.groups = "keep") |> 
  dplyr::ungroup() |> 
  tidyr::drop_na() |> 
  tibble::add_row(ssm_occnum = c(8888, 9999))
  
# komugiとssmをマッチング

Py <- onet_komugi |> 
  dplyr::rename(occ = ssm_occnum) |> 
  dplyr::left_join(Py, ., by = "occ")


# komugiを標準化

Py <- Py |> 
  dplyr::mutate(dplyr::across(IPD_04_03_01_001:IPD_04_10_036, \(.) scale(.)), 
                dplyr::across(IPD_04_03_01_001:IPD_04_10_036, \(.) as.list(.)),
                dplyr::across(IPD_04_03_01_001:IPD_04_10_036, \(.) unlist(.))) |> 
  dplyr::mutate(Anal = (IPD_04_03_01_011 + IPD_04_03_01_012 + IPD_04_03_01_013 + IPD_04_03_01_021)/4) |> 
  jtools::gscale(vars = "Anal", n.sd = 1) 

# X / Age----

Py <- Py |> 
  dplyr::rename(Age = num)

# A / Female, Education----

Py <- d |> 
  dplyr::mutate(Female = dplyr::case_match(q1_1, 
                                           1 ~ 0, 
                                           2 ~ 1, 
                                           .default = NA_integer_), 
                Female = fct_recode(factor(Female), "Female" = "1", "Male" = "0"), 
                Edu = dplyr::case_match(edssm, 
                                        4:5 ~ 0, 
                                        8:11 ~ 1, 
                                        .default = NA_integer_), 
                Edu = fct_recode(factor(Edu), "Low" = "0", "High" = "1")) |> 
  dplyr::select(id, Female, Edu) |> 
  (\(.) dplyr::left_join(Py, ., by = "id"))()

# Z / GDP, Unemployment----

Py <- Py |> 
  dplyr::mutate(Cohort = 2015 - age, 
                Period = Cohort + Age) |> 
  (\(.) dplyr::left_join(., YearAttributes, by = c("Period" = "Year")))()

