


write.csv(nchsur,
          "nchsur_Categorized.csv",row.names = FALSE, na = "")

#Run the below lines of codes in place of same
#lines in the 'RaceDisparity_Prelim_Prep to get the
#columns for non_NHWA ('not non-Hispanic White'):

ctypop.bakup<-ctypop

ctypop <- ctypop %>%
  mutate(NHWA_POP = NHWA_MALE + NHWA_FEMALE,
         NHBA_POP = NHBA_MALE + NHBA_FEMALE,
         NHIA_POP = NHIA_MALE + NHIA_FEMALE,
         NHAA_POP = NHAA_MALE + NHAA_FEMALE,
         NHNA_POP = NHNA_MALE + NHNA_FEMALE,
         NHTOM_POP = NHTOM_MALE + NHTOM_FEMALE,
         non_NHWA_POP = NHBA_MALE + NHBA_FEMALE +
           NHIA_MALE + NHIA_FEMALE +
           NHAA_MALE + NHAA_FEMALE +
           NHNA_MALE + NHNA_FEMALE +
           NHTOM_MALE + NHTOM_FEMALE +
           H_MALE + H_FEMALE,
         HISP_POP = H_MALE + H_FEMALE) %>%
  select(STATE, COUNTY, STNAME, CTYNAME, YEAR, AGEGRP,
         TOT_POP,
         NHWA_POP, NHBA_POP, NHIA_POP, NHAA_POP, NHNA_POP, 
         NHTOM_POP, non_NHWA_POP, HISP_POP) %>%
  mutate(NHWA_POP_PCT = round(100*NHWA_POP/TOT_POP,1),
         NHBA_POP_PCT = round(100*NHBA_POP/TOT_POP,1),
         NHIA_POP_PCT = round(100*NHIA_POP/TOT_POP,1),
         NHAA_POP_PCT = round(100*NHAA_POP/TOT_POP,1),
         NHNA_POP_PCT = round(100*NHNA_POP/TOT_POP,1),
         NHTOM_POP_PCT = round(100*NHTOM_POP/TOT_POP,1),
         non_NHWA_POP_PCT = round(100*non_NHWA_POP/TOT_POP,1),
         HISP_POP_PCT = round(100*HISP_POP/TOT_POP,1))


rm(ctytraj_a)

ctytraj_a <- ctytraj_s %>%
  # right join because we want 3,142 counties
  right_join(select(ctypop_s, -c("STATE", "COUNTY", "STNAME", "YEAR", "AGEGRP")), by= "FIPS") %>%
  # left join urbanization level
  left_join(select(nchsur, c("FIPS.code", "X2013.code")), by=c("FIPS"="FIPS.code"))




natlpop <- read.csv("./data/raw/nc-est2019-alldata-r-file22.csv", stringsAsFactors=F) %>%
  subset(YEAR==2020 & AGE==999 & UNIVERSE=="R" & MONTH==12) %>%
  select(TOT_POP,
         NHWA_MALE, NHWA_FEMALE,
         NHBA_MALE, NHBA_FEMALE,
         NHIA_MALE, NHIA_FEMALE,
         NHAA_MALE, NHAA_FEMALE,
         NHNA_MALE, NHNA_FEMALE,
         NHTOM_MALE, NHTOM_FEMALE,
         H_MALE, H_FEMALE) %>%
  mutate(NHWA_POP = NHWA_MALE + NHWA_FEMALE,
         NHBA_POP = NHBA_MALE + NHBA_FEMALE,
         NHIA_POP = NHIA_MALE + NHIA_FEMALE,
         NHAA_POP = NHAA_MALE + NHAA_FEMALE,
         NHNA_POP = NHNA_MALE + NHNA_FEMALE,
         non_NHWA_POP = NHBA_MALE + NHBA_FEMALE +
           NHIA_MALE + NHIA_FEMALE +
           NHAA_MALE + NHAA_FEMALE +
           NHNA_MALE + NHNA_FEMALE +
           NHTOM_MALE + NHTOM_FEMALE +
           H_MALE + H_FEMALE,
         NHTOM_POP = NHTOM_MALE + NHTOM_FEMALE,
         HISP_POP = H_MALE + H_FEMALE) %>%
  select(TOT_POP,
         NHWA_POP, NHBA_POP, NHIA_POP, NHAA_POP,
         NHNA_POP, NHTOM_POP, HISP_POP, non_NHWA_POP) %>%
  mutate(NHWA_POP_PCT = round(100*NHWA_POP/TOT_POP,1),
         NHBA_POP_PCT = round(100*NHBA_POP/TOT_POP,1),
         NHIA_POP_PCT = round(100*NHIA_POP/TOT_POP,1),
         NHAA_POP_PCT = round(100*NHAA_POP/TOT_POP,1),
         NHNA_POP_PCT = round(100*NHNA_POP/TOT_POP,1),
         NHTOM_POP_PCT = round(100*NHTOM_POP/TOT_POP,1),
         non_NHWA_POP_PCT = round(100*non_NHWA_POP/TOT_POP,1),
         HISP_POP_PCT = round(100*HISP_POP/TOT_POP,1))

grp <- "non_NHWA"