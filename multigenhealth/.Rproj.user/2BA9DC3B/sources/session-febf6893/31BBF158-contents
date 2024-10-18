ssm <- haven::read_sav("../data/SSM2015.sav") |> 
  haven::zap_labels()

jsei <- readr::read_csv("https://raw.githubusercontent.com/ShoFujihara/OccupationalScales/master/SSM_sei_ssi_v1.0.csv")
jsei <- jsei |> dplyr::rename(Occ = ssm)

