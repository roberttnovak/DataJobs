library(tidyverse)
library(readr)

raw_data <- read_csv("Data/Raw/uncleaned_data.csv")

# cat(uncleaned_data[1,"Job Description"] %>% as_vector)

gsub_vectorised <- function(vector_pattern,replacement,x,...){
  vector_pattern %>% 
    imap_chr(function(x_,i) gsub(x_,replacement,x[i],...) ) -> vector_replaced
  return(vector_replaced)
}

raw_data %>% 
  select(-index) %>% #La columna index no la necesitamos
  #Eliminamos la redundancia en la columna `Company Name` (aparece el rating al final)
  mutate(`Company Name`= trimws(gsub_vectorised(`Rating`,"", `Company Name`) ))   %>% 
  separate(`Salary Estimate`,sep="-", into=c("Salary Estimate Inf","Salary Estimate Sup")) %>%
  mutate(
    `Salary Estimate Inf` = gsub("K|\\$","",`Salary Estimate Inf`),
    `Salary Estimate Sup` = gsub("K|\\$","",`Salary Estimate Sup`) 
  ) %>% 
  #Lo separamos de nuevo para evitar poner todas las variaciones posibles Glassdoor est., Employer Est. etc
  separate(`Salary Estimate Sup`, sep="\\(", into=c("Salary Estimate Sup", "drop")) %>% 
  select(-drop ) %>% 
  mutate(
    `Salary Estimate Inf` = as.double(`Salary Estimate Inf`),
    `Salary Estimate Sup` = as.double(`Salary Estimate Sup`),
    `Salary Estimate Med` = (`Salary Estimate Inf` + `Salary Estimate Sup`)/2
  ) -> test

  