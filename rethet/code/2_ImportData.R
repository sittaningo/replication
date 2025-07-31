# Import Original Harmonized Data ----

## HRS ----

hrs <- haven::read_dta(file = "../data/HRS/H_HRS_d.dta", 
                       col_select = (c(
                         hhidpn, # household / person-year
                         hhid,
                         raeducl
                       ))) |> 
  haven::zap_labels() |> 
  dplyr::mutate(hhid = as.numeric(hhid))

## HRS Family ----

hrs_family <- haven::read_dta(file = "../data/HRS_family/randhrsfamr1992_2018v2.dta", 
                              col_select = (c(
                                hhidpn, # household / person-year
                                (starts_with("h") & ends_with("tcany")) # Transfers to children/grandchildren
                              ))) |> 
  haven::zap_labels()

## RandHRS ----
rand_hrs <- haven::read_dta(file = "../data/RAND_HRS/randhrs1992_2020v2.dta", 
                            col_select = (c(
                              hhidpn, # household / person-year
                              (starts_with("r") & ends_with("wtresp")), # weight
                              (starts_with("inw")), # attrition
                              (starts_with("r") & ends_with("shlt")), # subjective health
                              (starts_with("r") & ends_with("retemp")), # retirement
                              (starts_with("r") & ends_with("work")), # working status
                              rabyear, ragender, # sociodemographic attributes
                              (starts_with("r") & ends_with("dadage")), # father age
                              (starts_with("r") & ends_with("momage")), # mother age
                              (starts_with("r") & ends_with("agey_b")), # age at interview
                              (starts_with("r") & ends_with("mstat")), # marital status
                              (starts_with("h") & ends_with("ahous")), # housing wealth
                              (starts_with("h") & ends_with("arles")), # other housing wealth
                              (starts_with("h") & ends_with("astck")), # financial wealth_a
                              (starts_with("h") & ends_with("abond")), # financial wealth_b
                              (starts_with("h") & ends_with("achck")), # financial wealth_c
                              (starts_with("h") & ends_with("adebt")), # debt
                              (starts_with("h") & ends_with("itot")), # total household income
                              (starts_with("h") & ends_with("hhres")), # Number of people
                              (starts_with("h") & ends_with("child")), # Living children
                              (starts_with("r") & ends_with("livsib")), # Living siblings
                              (starts_with("r") & ends_with("slfemp")), # Whether self-employed
                              (starts_with("r") & ends_with("jhours")), # Hours of work per week at main job
                              (starts_with("r") & ends_with("livpar")) # number of living parents
                              ))
                            ) |> 
  haven::zap_labels() |> 
  dplyr::mutate(survey = "RAND_HRS")

### Combine Rand HRS / HRS ----

rand_hrs <- rand_hrs |> dplyr::inner_join(hrs, by = "hhidpn")
rand_hrs <- rand_hrs |> dplyr::inner_join(hrs_family, by = "hhidpn")

## SHARE ----
share <- readr::read_csv("../data/SHARE/shareintegrate_g.csv", 
                         col_select = (c(
                           (starts_with("r") & ends_with("wtresp")), # weight
                           (starts_with("inw")), # attrition
                           (starts_with("r") & ends_with("shlt")), # subjective health
                           (starts_with("r") & ends_with("retemp")), # retirement
                           (starts_with("r") & ends_with("work")), # working status
                           hhid, rabyear, ragender, raeducl, isocountry, # sociodemographic attributes
                           (starts_with("r") & ends_with("dadage")), # father age
                           (starts_with("r") & ends_with("momage")), # mother age
                           (starts_with("r") & ends_with("agey_b")), # age at interview
                           (starts_with("r") & ends_with("mstat")), # marital status
                           (starts_with("hh") & ends_with("ahous")), # housing wealth
                           (starts_with("hh") & ends_with("arles")), # other housing wealth
                           (starts_with("hh") & ends_with("abdstk")), # financial wealth_a
                           (starts_with("hh") & ends_with("achck")), # financial wealth_b
                           (starts_with("hh") & ends_with("adebt")), # debt
                           (starts_with("h") & ends_with("ittot")), # total household income
                           (starts_with("hh") & ends_with("ittot")), # total household income
                           (starts_with("h") & ends_with("tcany")), # Transfers to children/grandchildren
                           (starts_with("hh") & ends_with("hhres")), # Number of people
                           (starts_with("h") & ends_with("child")), # Living children
                           (starts_with("r") & ends_with("livsib")), # Living siblings
                           (starts_with("r") & ends_with("slfemp")), # Whether self-employed
                           (starts_with("r") & ends_with("jhours")), # Hours of work per week at main job
                           (starts_with("r") & ends_with("livpar")) # number of living parents
                         ))
) |> 
  haven::zap_labels() |> 
  dplyr::mutate(survey = "SHARE", 
                hhid = as.numeric(hhid))

## ELSA ----
elsa <- haven::read_dta("../data/ELSA/h_elsa_g3.dta", 
                        col_select = (c(
                          (starts_with("r") & ends_with("lwtresp")), # weight
                          (starts_with("inw")), # attrition
                          (starts_with("r") & ends_with("shlt")), # subjective health
                          (starts_with("r") & ends_with("retemp")), # retirement
                          (starts_with("r") & ends_with("work")), # working status
                          hh1hhid, rabyear, ragender, raeducl, # sociodemographic attributes
                          (starts_with("r") & ends_with("dadage")), # father age
                          (starts_with("r") & ends_with("momage")), # mother age
                          (starts_with("r") & ends_with("agey_b")), # age at interview
                          (starts_with("r") & ends_with("mstat")), # marital status
                          (starts_with("h") & ends_with("ahous")), # housing wealth
                          (starts_with("h") & ends_with("arles")), # other housing wealth
                          (starts_with("h") & ends_with("astck")), # financial wealth_a
                          (starts_with("h") & ends_with("abond")), # financial wealth_b
                          (starts_with("h") & ends_with("achck")), # financial wealth_c
                          (starts_with("h") & ends_with("adebt")), # debt
                          (starts_with("h") & ends_with("itot")), # total household income
                          (starts_with("h") & ends_with("tcany")), # Transfers to children/grandchildren
                          (starts_with("h") & ends_with("hhres")), # Number of people
                          (starts_with("r") & ends_with("child")), # Living children
                          (starts_with("r") & ends_with("livsib")), # Living siblings
                          (starts_with("r") & ends_with("slfemp")), # Whether self-employed
                          (starts_with("r") & ends_with("jhours")), # Hours of work per week at main job
                          (starts_with("r") & ends_with("livpar")) # number of living parents
                        ))) |> 
  haven::zap_labels() |> 
  dplyr::mutate(survey = "ELSA") |> 
  dplyr::rename(hhid = hh1hhid)

## MHAS ----
mhas <- haven::read_dta("../data/MHAS/H_MHAS_c2.dta", 
                        col_select = (c(
                          (starts_with("r") & ends_with("wtresp")), # weight
                          (starts_with("inw")), # attrition
                          (starts_with("r") & ends_with("shlt")), # subjective health
                          (starts_with("r") & ends_with("retemp")), # retirement
                          (starts_with("r") & ends_with("work")), # working status
                          unhhid, rabyear, ragender, raeducl, # sociodemographic attributes
                          (starts_with("r") & ends_with("dadage")), # father age
                          (starts_with("r") & ends_with("momage")), # mother age
                          (starts_with("r") & ends_with("agey_b")), # age at interview
                          (starts_with("r") & ends_with("mstat")), # marital status
                          (starts_with("h") & ends_with("ahous")), # housing wealth
                          (starts_with("h") & ends_with("arles")), # other housing wealth
                          (starts_with("h") & ends_with("abdstk")), # financial wealth_a
                          (starts_with("h") & ends_with("achck")), # financial wealth_b
                          (starts_with("h") & ends_with("adebt")), # debt
                          (starts_with("h") & ends_with("itot")), # total household income
                          (starts_with("h") & ends_with("tcany")), # Transfers to children/grandchildren
                          (starts_with("h") & ends_with("hhres")), # Number of people
                          (starts_with("h") & ends_with("child")), # Living children
                          (starts_with("r") & ends_with("livsib")), # Living siblings
                          (starts_with("r") & ends_with("slfemp")), # Whether self-employed
                          (starts_with("r") & ends_with("jhours")), # Hours of work per week at main job
                          (starts_with("r") & ends_with("livpar")) # number of living parents
                        ))) |> 
  haven::zap_labels() |> 
  dplyr::mutate(survey = "MHAS") |> 
  dplyr::rename(hhid = unhhid)

## CHARLS ----
charls <- haven::read_dta("../data/CHARLS/H_CHARLS_D_Data.dta", 
                          col_select = (c(
                            (starts_with("r") & ends_with("wtrespl")), # weight
                            (starts_with("inw")), # attrition
                            (starts_with("r") & ends_with("shlt")), # subjective health
                            (starts_with("r") & ends_with("retemp")), # retirement
                            (starts_with("r") & ends_with("work")), # working status
                            hhid, rabyear, ragender, raeducl, # sociodemographic attributes
                            (starts_with("r") & ends_with("dadage")), # father age
                            (starts_with("r") & ends_with("momage")), # mother age
                            (starts_with("r") & ends_with("agey_b")), # age at interview
                            (starts_with("r") & ends_with("mstat")), # marital status
                            (starts_with("h") & ends_with("ahous")), # primary housing
                            (starts_with("h") & ends_with("ahous")), # housing wealth
                            (starts_with("hh") & ends_with("ahous")), # housing wealth
                            (starts_with("h") & ends_with("arles")), # other housing wealth
                            (starts_with("hh") & ends_with("arles")), # other housing wealth
                            (starts_with("h") & ends_with("astck")), # financial wealth_a
                            (starts_with("h") & ends_with("abond")), # financial wealth_b
                            (starts_with("h") & ends_with("achck")), # financial wealth_c
                            (starts_with("h") & ends_with("adebt")), # debt
                            (starts_with("hh") & ends_with("itot")), # total household income
                            (starts_with("h") & ends_with("tcany")), # Transfers to children/grandchildren
                            (starts_with("h") & ends_with("hhres")), # Number of people
                            (starts_with("h") & ends_with("child")), # Living children
                            (starts_with("r") & ends_with("livsib")), # Living siblings
                            (starts_with("r") & ends_with("slfemp")), # Whether self-employed
                            (starts_with("r") & ends_with("jhours_c")), # Hours of work per week at main job
                            (starts_with("r") & ends_with("livpar")) # number of living parents
                          ))) |> 
  haven::zap_labels() |> 
  dplyr::mutate(survey = "CHARLS")

## KLoSA ----
klosa <- haven::read_dta("../data/KLoSA/H_KLoSA_e2.dta", 
                         col_select = (c(
                           (starts_with("r") & ends_with("lwtresp")), # weight
                           (starts_with("inw")), # attrition
                           (starts_with("r") & ends_with("shlt")), # subjective health
                           (starts_with("r") & ends_with("retemp")), # retirement
                           (starts_with("r") & ends_with("work")), # working status
                           hhid, rabyear, ragender, raeducl, # sociodemographic attributes
                           (starts_with("r") & ends_with("dadage")), # father age
                           (starts_with("r") & ends_with("momage")), # mother age
                           (starts_with("r") & ends_with("agey_b")), # age at interview
                           (starts_with("r") & ends_with("mstat")), # marital status
                           (starts_with("r") & ends_with("ahous")), # housing wealth
                           (starts_with("s") & ends_with("ahous")), # housing wealth
                           (starts_with("r") & ends_with("arles")), # other housing wealth
                           (starts_with("s") & ends_with("arles")), # other housing wealth
                           (starts_with("r") & ends_with("astck")), # financial wealth_a
                           (starts_with("s") & ends_with("astck")), # financial wealth_a
                           (starts_with("r") & ends_with("abond")), # financial wealth_b
                           (starts_with("s") & ends_with("abond")), # financial wealth_b
                           (starts_with("r") & ends_with("achck")), # financial wealth_c
                           (starts_with("s") & ends_with("achck")), # financial wealth_c
                           (starts_with("r") & ends_with("adebt")), # debt
                           (starts_with("s") & ends_with("adebt")), # debt
                           (starts_with("r") & ends_with("itot")), # total household income
                           (starts_with("s") & ends_with("itot")), # total household income
                           (starts_with("h") & ends_with("tcany")), # Transfers to children/grandchildren
                           (starts_with("h") & ends_with("hhres")), # Number of people
                           (starts_with("h") & ends_with("child")), # Living children
                           (starts_with("r") & ends_with("livsib")), # Living siblings
                           (starts_with("r") & ends_with("slfemp")), # Whether self-employed
                           (starts_with("r") & ends_with("jhours")), # Hours of work per week at main job
                           (starts_with("r") & ends_with("livpar")) # number of living parents
                         ))) |> 
  haven::zap_labels() |> 
  dplyr::mutate(survey = "KLoSA")

# Integrate Data ----

harmonized <- rand_hrs |> 
  dplyr::bind_rows(share) |> 
  dplyr::bind_rows(mhas) |> 
  dplyr::bind_rows(elsa) |> 
  dplyr::bind_rows(charls) |> 
  dplyr::bind_rows(klosa) |> 
  dplyr::mutate(id = row_number())

# Sums of # must be 272352

# Period ----

wave_year_correspondence <- readr::read_csv("../data/wave_survey.csv")
