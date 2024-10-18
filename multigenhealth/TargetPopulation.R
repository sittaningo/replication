# Those who have children who is over 25 years old

ssm_filtered <- ssm |> 
  dplyr::filter(ChildNum >= 1 & Child1_25 == 1 & Age >= 50)
