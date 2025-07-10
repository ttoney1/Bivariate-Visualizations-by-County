# --------------------------------------------------------- #
# Author:       Florence Lee (kwn5)                         #
# Team:         CDC COVID-19 Data On-Call                   #
# Purpose:      Respond to Representative Lee's request     #
#               to identify Top 10 counties with the        #
#               largest racial and ethnic disparities       #
# Details:      This uses a revised approach -- a moving    #
#               threshold for overrepresentation across     #
#               all race-ethnicity groups. Code was         #
#               revised on 7/29 to address Policy Unit      #
#               presentation concerns. Now showing four     #
#               classes instead of nine.                    #
# Instructions: Run RaceDisparity_Prelim_Prep.R to          #
#               create the analytic dataset if needed.      #
# --------------------------------------------------------- #


# ======================= SET UP ======================= #

# === Read in libraries === #

#Changed wd from Share Drive to SharePoint
#setwd('//cdc.gov/locker/DDPHSS_CDHShare/data-on-call/Race_disparity/Prelim-Analysis')
user <- Sys.getenv("USERNAME")
path <- paste0("C:/Users/", user, "/CDC/Data, Analytics, Visualization Task Force - Data On-Call Team/1. Data Requests/00_Race Disparity Analysis/Race_disparity/Prelim-Analysis/")
setwd(path)

library(data.table)
library(xlsx)

# for cleaning data
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(forcats)

# for figures
library(ggplot2)
# for county maps
library(usmap)
# for dumbbell plot
#library(ggalt)
# for bivariate map
#library(grid)
library(pals)
library(latticeExtra)
library(classInt)
# for table output
library(tinytex)
library(knitr)
library(kableExtra)


# ================ PREPARATION (if needed) ================ #

# source("./R/RaceDisparity_Prelim_Prep.R")

# ========================================================= #

########## HISTOGRAM OF RACIAL COMPOSITION ACROSS COUNTIES ##########

# ctypop_t <- ctypop_s %>%
#   select("FIPS", "CTYNAME", "STNAME",
#          "NHWA_POP_PCT", "NHBA_POP_PCT", "NHIA_POP_PCT", "NHAA_POP_PCT", "NHNA_POP_PCT", "NHTOM_POP_PCT", "HISP_POP_PCT") %>%
#   pivot_longer(cols = c("NHWA_POP_PCT", "NHBA_POP_PCT", "NHIA_POP_PCT", "NHAA_POP_PCT", "NHNA_POP_PCT", "NHTOM_POP_PCT", "HISP_POP_PCT"),
#                names_to = "Race.Ethnicity",
#                values_to = "Pct.Dist.Pop")
# 
# ctypop_t %>%
#   ggplot(aes(x=Pct.Dist.Pop)) +
#   geom_histogram(binwidth=10, color="white", fill="#47596e") +
#   stat_bin(binwidth = 10, geom = "text", aes(label=..count..), vjust=-1) +
#   scale_x_continuous(breaks=seq(0,100,10)) +
#   theme_classic() +
#   facet_wrap(~ Race.Ethnicity) +
#   ggtitle("Distribution of counties by % race-ethnicity group", subtitle="Includes 3,142 counties")

###################################################################

# === Create functions needed === #

re_recode <- function(d) {
  
  # Enters in abbreviate race-ethnicity acronyms and prepares them 
  # for display (e.g., in labels)
  
  ifelse(d=="NHBA", "non-Hispanic Black", 
         ifelse(d=="HISP", "Hispanic",
                ifelse(d=="NHIA", "non-Hispanic American Indian or Alaska Native",
                       ifelse(d=="Total", "Total",
                              ifelse(d=="NHAA", "non-Hispanic Asian", 
                                     ifelse(d=="NHNA", "non-Hispanic Native Hawaiian or Other Pacific Islander", NA))))))
  
}

bdn_recode <- function(d) {
  
  # Enters in numeric value for classGrp and prepares them 
  # for display in the data table
  
  ifelse(d==1, "Below-average population, not high burden", 
         ifelse(d==2, "Large population, not high burden",
                ifelse(d==3, "Largest population, not high burden",
                       ifelse(d==4, "Below-average population, high burden",
                              ifelse(d==5, "Large population, high burden", 
                                     ifelse(d==6, "Largest population, high burden", 
                                            ifelse(d==7, "Below-average population, highest burden",
                                                   ifelse(d==8, "Large population, highest burden",
                                                          ifelse(d==9, "Largest population, highest burden", NA)))))))))
  
}


# === Read in datasets for analysis

load("./data/clean/ctytraj.RData") 
# --- ctytraj_a
load("./data/clean/natlpop.RData")
# --- natlpop_t

# ================ ANALYSIS: High burden ================ #

# High burden is defined as > 100 new cases in the past two
# weeks per 100,000 population among counties with more than 
# 5 new cases in the past two weeks.

# === Map: High burden counties === #

cols <- stevens.pinkblue
nbins <- 3

# Jenks Natural Breaks for recent burden (no longer used for maps as of 8/24/20, but here for reference)
# --- Group one: <= 100 new cases per 100,000 in the past two weeks or <= 5 new cases in the past two weeks
# --- Group two and three: Use Jenks Natural Breaks to determine "high" and "highest"

brksc <- classIntervals(ctytraj_a$ci_2wk[which(ctytraj_a$hb_cat=="high")], n=nbins-1, style='jenks')$brks

#Below code no longer used for maps as of 8/24/20, but here for reference)
#classCOVID <- with(ctytraj_a, ifelse(ci_2wk<=100|(ci_2wk>100 & n_2wk<=5), 1, 
                                        #ifelse(ci_2wk>100 & n_2wk>5 & ci_2wk<=brksc[2], 2, 3)))





classCOVID <- with(ctytraj_a, ifelse(ci_2wk<=100|(ci_2wk>100 & n_2wk<=5), 1, 
                                     ifelse(ci_2wk>100 & n_2wk>5 & ci_2wk <500, 2, 3)))

ctytraj_a$classCOVID <- classCOVID

# range(subset(ctytraj_a, classCOVID==2)$ci_2wk)
# range(subset(ctytraj_a, classCOVID==3)$ci_2wk)

# table(ctytraj_a$classCOVID)

hbchoro <- ctytraj_a
hbchoro <- select(hbchoro, FIPS, classCOVID) 
hbchoro$classCOVID <- factor(hbchoro$classCOVID)
colnames(hbchoro) <- c("fips", "value")

covid_cols <- cols()[c(1, 4, 7)]

plot_usmap(data = hbchoro, values = "value", color="#ffffff") +
  scale_fill_manual(values=c('1'= covid_cols[1], '2'= covid_cols[2], '3' = covid_cols[3]),
                    name="", labels=c("Not high recent burden", "High recent burden", "Highest recent burden")) +
  ggtitle("U.S. counties with high recent burden of COVID-19", subtitle = "Based on new cases per 100,000 in the past two weeks") +
  theme(legend.position="top")


ggsave("./output/high-burden-map.png", width=11, units="in")


# === Table: Distribution of high burden counties (all U.S. counties)  === #

tall_hb <- table(ctytraj_a$classCOVID) %>% 
  as.data.frame() %>%
  rename(Num=Freq) %>%
  left_join(prop.table(table(ctytraj_a$classCOVID)) %>% 
              as.data.frame() %>% 
              rename(Pct=Freq) %>%
              mutate(Pct=round(100*Pct,1))) %>%
  rename(Burden=Var1)

# === Table: Distribution of high burden counties among counties with large pops (by group) === #

# Look at how burden is distribution among high population areas

for (i in c("NHWA", "NHBA", "HISP", "NHAA", "NHIA", "NHNA")) {
  # High pop threshold will vary by group
  highpop <- subset(natlpop_t, Race.Ethnicity==i)$Pct.Dist.Pop   
  # Frequency
  tbl <- eval(parse(text=paste0('table(subset(ctytraj_a, ', i, '_POP_PCT > highpop)$classCOVID)'))) %>% 
    as.data.frame() %>%
    rename("Num"="Freq", "Burden"="Var1")
  # Percentages
  tbl2 <- eval(parse(text=paste0('prop.table(table(subset(ctytraj_a, ', i, '_POP_PCT > highpop)$classCOVID))'))) %>%
    as.data.frame() %>%
    rename("Pct"="Freq", "Burden"="Var1")
  # Merge into one table
  tbl3 <- left_join(tbl, tbl2) %>% 
    mutate(Pct=round(100*Pct,1))
  assign(paste0('t', i, 'cty_hb'), tbl3)
  
}

tNHWActy_hb
tNHBActy_hb
tHISPcty_hb
tNHAActy_hb
tNHIActy_hb
tNHNActy_hb


# === Barplots: Distribution of high burden counties among counties with large pops (by group) === #

# Put all distributions for each group into a single table and format for the stacked bar graph.
# Removed White, non-Hispanic group from analysis.
barhb <- tall_hb %>% select(-c("Num")) %>% rename(Total_Pct=Pct) %>%
  left_join(tNHBActy_hb %>% select(-c("Num")) %>% rename(NHBA_Pct=Pct)) %>%
  left_join(tHISPcty_hb %>% select(-c("Num")) %>% rename(HISP_Pct=Pct)) %>%
  left_join(tNHAActy_hb %>% select(-c("Num")) %>% rename(NHAA_Pct=Pct)) %>%
  left_join(tNHIActy_hb %>% select(-c("Num")) %>% rename(NHIA_Pct=Pct)) %>%
  left_join(tNHNActy_hb %>% select(-c("Num")) %>% rename(NHNA_Pct=Pct)) %>%
  pivot_longer(cols = -c("Burden"),
               names_to = "Race.Ethnicity", 
               values_to = "Pct_HB")
barhb$Race.Ethnicity <- sub("_Pct", "", barhb$Race.Ethnicity)
# ------ Format label for stacked bar chart
barhb$Race.Ethnicity <- re_recode(barhb$Race.Ethnicity)
# Text wrap for bar graph display
barhb$Race.Ethnicity <- str_wrap(barhb$Race.Ethnicity, width = 10)
# ------ Reorder race-ethnicity groups by Burden and Percent
barhb$Race.Ethnicity <- forcats::fct_reorder2(barhb$Race.Ethnicity, -as.numeric(barhb$Burden), -barhb$Pct_HB) 
# ------ Reverse so that "Not high recent burden" is on top of stacked bar
ggplot(barhb, aes(fill=Burden, y=Pct_HB, x=Race.Ethnicity)) +
  geom_bar(position="stack", colour="#000000", stat="identity") +
  geom_text(aes(label=Pct_HB), color = "#000000", size = 3, position = position_stack(vjust = 0.5)) + 
  xlab("") + ylab("Percent of counties") +
  # Label order follows forcats::fct_rev(Burden)
  scale_fill_manual(values=c('1'= covid_cols[1], '2'= covid_cols[2], '3' = covid_cols[3]),
                    name="", labels=c("Not high recent burden", "High recent burden", "Highest recent burden")) +
  ggtitle("Distribution of counties with recent high COVID-19 burden") +
  theme_classic() + 
  theme(legend.position="top")
ggsave("./output/high-burden-by-re-0921.png", width=11, units="in")


# ================== Bivariate mapping ================== #

# --- Commented out because colors previously assigned
# cols <- stevens.pinkblue
# nbins <- 3

# --- Group thresholds for large population

# NHWA_POP_PCT NHBA_POP_PCT NHIA_POP_PCT NHAA_POP_PCT NHNA_POP_PCT NHTOM_POP_PCT HISP_POP_PCT
#        60.4         12.5          0.7          5.7          0.2           2.2         18.3

# COLORS HAVE BEEN ADJUSTED FOR RTC
# --- Only "high burden" and "large population" groups have colors assigned.
# --- Others are gray (#e8e8e8).
# --- Can revert to 9-color scheme by using cols() instead of cols_rtc.

cols_rtc <- c("#e8e8e8", "#e8e8e8", "#e8e8e8", "#e8e8e8", "#bbb9e1", "#5d9cbb", "#e8e8e8", "#9d6ab2", "#292373")

# ---- CHANGE 'grp' VALUE TO RENDER RESULTS FOR DIFFERENT GROUPS ---- #
# Options: HISP, NHWA, NHBA, NHIA, NHAA, NHNA

grp <- "HISP"  #uncomment below to run for desired race / ethnicity group
#grp <-"NHIA"
#grp <-"NHBA"
#grp <-"NHAA"
#grp <-"NHNA"
grp_pct<- paste0(grp, '_POP_PCT')
grp_predom <- paste0(grp, '_HIGH')

bichoro <- ctytraj_a
highpop <- subset(natlpop_t, Race.Ethnicity==grp)$Pct.Dist.Pop   

# Jenks Natural Breaks for population composition
# --- Group one: <= National composition
# --- Group two and three: Use Jenks Natural Breaks for "large" and "largest"
brksp <- eval(parse(text=paste0("classIntervals(bichoro$", grp_pct, "[which(bichoro$", 
                                grp_pct, ">", highpop, ")], n=nbins-1, style='jenks')$brks")))

classPOP <- eval(parse(text=paste0("with(bichoro, ifelse(", 
                                   grp_pct, "<=", highpop, ", 1, ifelse(", grp_pct, ">", highpop, " & ", grp_pct, "<=", brksp[2], 
                                   ", 2, 3)))")))
bichoro$classPOP <- classPOP

# table(classCOVID)
# table(classPOP)

# Create nine-category group class that combines 3-class classPOP and 3-class classCOVID
bichoro <- bichoro %>%
  mutate(classGrp = classPOP + nbins*(classCOVID-1))

eval(parse(text=paste0("plot(ci_2wk ~ ", grp_pct, ", data=bichoro, col=cols_rtc[bichoro$classGrp], pch=19)")))

# 9-class colors
# cols_u <- cols()[cols() %in% unique(cols()[bichoro$classGrp])]

# 4-class colors
cols_u <- cols_rtc[cols_rtc %in% unique(cols_rtc[bichoro$classGrp])]

tbl <- eval(parse(text=paste0("
bichoro %>%
  subset(classGrp==9|classGrp==6|classGrp==8|classGrp==5) %>%
  arrange(factor(classGrp, levels=c(9, 6, 8, 5)), -ci_2wk) %>%
  slice(1:10) %>%
  select(cName, size, ", grp_pct, ", ", grp_predom, ", ci_2wk, classGrp) %>%
  mutate(ci_2wk = round(ci_2wk,2))")))

tbl[,grp_predom] <- ifelse(tbl[,grp_predom]==1, "*", "")
tbl <- tbl %>%
  rename("County"="cName",
         "Urbanization level" = "size",
         "New cases per 100,000 in the past 2 weeks"="ci_2wk",
         "Percent of population"=grp_pct,
         "Predominant group (*)"=grp_predom)
 
if (length(unique(tbl$classGrp)) %in% c(0,1)) { 
  knitr::kable(select(tbl, -classGrp)) %>% kable_styling() %>% pack_rows(bdn_recode(unique(tbl$classGrp)), 1, nrow(tbl))
  } else { 
    knitr::kable(select(tbl, -classGrp)) %>% pack_rows(index=table(fct_inorder(factor(bdn_recode(tbl$classGrp), levels=c(bdn_recode(9), bdn_recode(6), bdn_recode(8), bdn_recode(5))
                                                                                      [c(bdn_recode(9),bdn_recode(6),bdn_recode(8),bdn_recode(5)) %in% unique(bdn_recode(tbl$classGrp))])))) %>% kable_styling()
  }


bichoro_u <- bichoro %>%
  select("FIPS", "classGrp") %>%
  rename(fips="FIPS", "value"="classGrp")
bichoro_u$value <- factor(bichoro_u$value)

bichoro_map <- plot_usmap(data = bichoro_u, values = "value", color="#ffffff") +
  scale_fill_manual(values=cols_u) +
  ggtitle(paste0("Recent COVID-19 burden and percent of population: ", re_recode(grp))) +
  theme(legend.position="none")
bichoro_map
ggsave(paste0("./output/", grp, "-bivariate-map.png"), width=11, units="in")


# bichoro_legend <- levelplot(matrix(1:(nbins*nbins), nrow=nbins), axes=FALSE, col.regions=cols_rtc,
#                                  xlab="Pop >", ylab="Burden >", cuts=8, colorkey=FALSE,
#                                  scales=list(draw=0)) 
# vp <- viewport(x=0.65, y=0.2, width=.2, height=.2)
# pushViewport(vp)
# print(bichoro_legend, newpage=FALSE)
# popViewport()
