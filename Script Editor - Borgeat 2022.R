#

# Hit the right target! 
  #The Role of Issue Saliency among a Party's Potential Voters as a Determinant of the Party's Issue Emphasis in Switzerland 

# Quentin Borgeat
# Department of political science and international relations
# University of Geneva

# January 2022

############################################################################################################################

# Data: 

# Selects Panel RCS 2015 v1.1
  # https://www.swissubase.ch/en/catalogue/studies/12447/13617/overview
  # (last consultation: January 3rd, 2022)

# Selects Panel RCS 2019 v1.0.0
  # https://www.swissubase.ch/en/catalogue/studies/13846/17897/overview
  # (last consultation: January 3rd, 2022)

# APS-Inseratedaten zu den eidg. Wahlen 2015
  # available on request from the corresponding author
  # B�hlmann, Marc, Marl�ne Gerber, Laura Salathe and David Zumbach
  # at https://anneepolitique.swiss/fr/pages/contact
  # (last consultation: January 3rd, 2022)

# APS-Inseratedaten zu den eidg. Wahlen 2019
  # available on request from the corresponding author
  # B�hlmann, Marc, Marl�ne Gerber, Laura Salathe and David Zumbach
  # at https://anneepolitique.swiss/fr/pages/contact
  # (last consultation: January 3rd, 2022)

# Software: R version 4.1.0

###########################################################################################################################
rm(list = ls())
#Chargement des extensions necessaires----
library(foreign)
library(questionr)
library(haven)
library(sjmisc) 
library(scales)
library(caret)
library(naniar)
library(car)
library(tidyverse)
library(lme4)
library(pscl)
library(boot)
library(effects)
library(dplyr)


###########################################################################################################################
############                                       2015                                                        ############
###########################################################################################################################

getwd()
setwd("C:/Users/borgeatq/Documents/R/first paper")

D <- read_dta("APS-Wahlinserate_2015_20160830.dta")
S <- read_dta("828_Selects2015_PanelRCS_Data_v1.1.dta")

### Advertisements data

### considerer 1 aout comme debut de campagne ----
D <- D %>% dplyr::select(-starts_with("Kand_name")) %>% filter(date >= as.Date("2015-08-1") & date <= as.Date("2015-10-18"))

#####Recodage######
#Canton
D <- D %>% mutate(Canton = case_when(Kanton==0 ~ "CH",
                                     Kanton==1 ~ "ZH",
                                     Kanton==2 ~ "BE", 
                                     Kanton==3 ~ "LU",
                                     Kanton==4 ~ "UR",
                                     Kanton==5 ~ "SZ",
                                     Kanton==6 ~ "OW",
                                     Kanton==7 ~ "NW",
                                     Kanton==8 ~ "GL",
                                     Kanton==9 ~ "ZG",
                                     Kanton==10 ~ "FR",
                                     Kanton==11 ~ "SO",
                                     Kanton==12 ~ "BS",
                                     Kanton==13 ~ "BL",
                                     Kanton==14 ~ "SH",
                                     Kanton==15 ~ "AR",
                                     Kanton==16 ~ "AI",
                                     Kanton==17 ~ "SG",
                                     Kanton==18 ~ "GR",
                                     Kanton==19 ~ "AG",
                                     Kanton==20 ~ "TG",
                                     Kanton==21 ~ "TI",
                                     Kanton==22 ~ "VD",
                                     Kanton==23 ~ "VS",
                                     Kanton==24 ~ "NE",
                                     Kanton==25 ~ "GE",
                                     Kanton==26 ~ "JU"))
#party
D <- D %>% mutate(party = case_when(partei_rc==3 ~ 8,
                                    partei_rc==31 ~ 4,
                                    partei_rc==311 ~ 8,
                                    partei_rc==32 ~ 2,
                                    partei_rc==33 ~ 3,
                                    partei_rc==34 ~ 1,
                                    partei_rc==341 ~ 7,
                                    partei_rc==352 ~ 8,
                                    partei_rc==355 ~ 8,
                                    partei_rc==358 ~ 8,
                                    partei_rc==354 ~ 8,
                                    partei_rc==359 ~ 5,
                                    partei_rc==3591 ~ 6,
                                    partei_rc==36 ~ 8))
D <- D %>% mutate(party = case_when(partei_rc==3 ~ "Other",
                                    partei_rc==31 ~ "PDC",
                                    partei_rc==311 ~ "Other",
                                    partei_rc==32 ~ "FDP",
                                    partei_rc==33 ~ "PS",
                                    partei_rc==34 ~ "UDC",
                                    partei_rc==341 ~ "PDB",
                                    partei_rc==352 ~ "Other",
                                    partei_rc==355 ~ "Other",
                                    partei_rc==358 ~ "Other",
                                    partei_rc==354 ~ "Other",
                                    partei_rc==359 ~ "Verts",
                                    partei_rc==3591 ~ "Verts_lib",
                                    partei_rc==36 ~ "Other"))
#issue
D <- D %>% mutate(economy = case_when(Thema_Wirtschaft_total==1 ~ 1,
                                      Thema_Wirtschaft_total==0 ~ 0))
D <- D %>% mutate(environment = case_when(Thema_EnergieUmwelt==1 ~ 1,
                                          Thema_EnergieUmwelt==0 ~ 0))
D <- D %>% mutate(immigration = case_when(Thema_AsylMigration==1 ~ 1,
                                          Thema_AsylMigration==0 ~ 0))
D <- D %>% mutate(social = case_when(Thema_Sozial==1 ~ 1,
                                     Thema_Sozial==0 ~ 0))
D <- D %>% mutate(travail = case_when(Thema_Arbeit==1 ~ 1,
                                      Thema_Arbeit==0 ~ 0))
D <- D %>% mutate(EU = case_when(Thema_EU==1 ~ 1,
                                 Thema_EU==0 ~ 0))
D <- D %>% mutate(Other = case_when(Thema_Verkehr==1 ~ 1,
                                    Thema_Verkehr==0 ~ 0,
                                    Thema_Familie==1 ~ 1,
                                    Thema_Familie==0 ~ 0))

#### ne garder que les pubs avec des themes ####
D <- D %>% filter(No_Thema==0)

###select les variables d'interet####
D <- select(D, c("Canton", "immigration", "economy", "environment", "social", "travail", "EU", "Other", "party"))

####pivot long####
A <- pivot_longer(D, -c("Canton", "party"), names_to = "issue", values_to = "number")

####grouper les observations par combinaison party-issue-canton####
X <- A %>% group_by(Canton, party, issue) %>% summarise(number = sum(number))

####drop observations cantons et party other####
X1 <- X[!(X$Canton=="CH"),]
X1 <- X1[!(X1$party=="Other"),]
X1 <- X1[!(X1$issue=="Other" | X1$issue=="travail"),]

#### creer une variable "issue emphasis des autres partis"#####
W <- read_dta("APS-Wahlinserate_2015_20160830.dta")

W <- W %>% select(-starts_with("Kand_name")) %>% filter(date <= as.Date("2015-08-01"))

#Canton
W <- W %>% mutate(Canton = case_when(Kanton==0 ~ "CH",
                                     Kanton==1 ~ "ZH",
                                     Kanton==2 ~ "BE", 
                                     Kanton==3 ~ "LU",
                                     Kanton==4 ~ "UR",
                                     Kanton==5 ~ "SZ",
                                     Kanton==6 ~ "OW",
                                     Kanton==7 ~ "NW",
                                     Kanton==8 ~ "GL",
                                     Kanton==9 ~ "ZG",
                                     Kanton==10 ~ "FR",
                                     Kanton==11 ~ "SO",
                                     Kanton==12 ~ "BS",
                                     Kanton==13 ~ "BL",
                                     Kanton==14 ~ "SH",
                                     Kanton==15 ~ "AR",
                                     Kanton==16 ~ "AI",
                                     Kanton==17 ~ "SG",
                                     Kanton==18 ~ "GR",
                                     Kanton==19 ~ "AG",
                                     Kanton==20 ~ "TG",
                                     Kanton==21 ~ "TI",
                                     Kanton==22 ~ "VD",
                                     Kanton==23 ~ "VS",
                                     Kanton==24 ~ "NE",
                                     Kanton==25 ~ "GE",
                                     Kanton==26 ~ "JU"))
#party
W <- W %>% mutate(party = case_when(partei_rc==3 ~ 8,
                                    partei_rc==31 ~ 4,
                                    partei_rc==311 ~ 8,
                                    partei_rc==32 ~ 2,
                                    partei_rc==33 ~ 3,
                                    partei_rc==34 ~ 1,
                                    partei_rc==341 ~ 7,
                                    partei_rc==352 ~ 8,
                                    partei_rc==355 ~ 8,
                                    partei_rc==358 ~ 8,
                                    partei_rc==354 ~ 8,
                                    partei_rc==359 ~ 5,
                                    partei_rc==3591 ~ 6,
                                    partei_rc==36 ~ 8))

W <- W %>% mutate(party = case_when(partei_rc==3 ~ "Other",
                                    partei_rc==31 ~ "PDC",
                                    partei_rc==311 ~ "Other",
                                    partei_rc==32 ~ "FDP",
                                    partei_rc==33 ~ "PS",
                                    partei_rc==34 ~ "UDC",
                                    partei_rc==341 ~ "PDB",
                                    partei_rc==352 ~ "Other",
                                    partei_rc==355 ~ "Other",
                                    partei_rc==358 ~ "Other",
                                    partei_rc==354 ~ "Other",
                                    partei_rc==359 ~ "Verts",
                                    partei_rc==3591 ~ "Verts_lib",
                                    partei_rc==36 ~ "Other"))
#issue
W <- W %>% mutate(economy = case_when(Thema_Wirtschaft_total==1 ~ 1,
                                      Thema_Wirtschaft_total==0 ~ 0))
W <- W %>% mutate(environment = case_when(Thema_EnergieUmwelt==1 ~ 1,
                                          Thema_EnergieUmwelt==0 ~ 0))
W <- W %>% mutate(immigration = case_when(Thema_AsylMigration==1 ~ 1,
                                          Thema_AsylMigration==0 ~ 0))
W <- W %>% mutate(social = case_when(Thema_Sozial==1 ~ 1,
                                     Thema_Sozial==0 ~ 0))
W <- W %>% mutate(travail = case_when(Thema_Arbeit==1 ~ 1,
                                      Thema_Arbeit==0 ~ 0))
W <- W %>% mutate(EU = case_when(Thema_EU==1 ~ 1,
                                 Thema_EU==0 ~ 0))
W <- W %>% mutate(Other = case_when(Thema_Verkehr==1 ~ 1,
                                    Thema_Verkehr==0 ~ 0,
                                    Thema_Familie==1 ~ 1,
                                    Thema_Familie==0 ~ 0))

W <- select(W, c("Canton", "immigration", "economy", "environment", "social", "travail", "EU", "Other", "party"))

V <- pivot_longer(W, -c("Canton", "party"), names_to = "issue", values_to = "number")

V <- V %>% group_by(Canton, party, issue) %>% summarise(number = sum(number))

V <- V[!(V$Canton=="CH"),]
V <- V[!(V$party=="Other"),]
V <- V[!(V$issue=="Other" | V$issue=="travail"),]
V <- V[!(V$Canton=="CH"),]


###udc####
y <- V %>% group_by(issue, Canton) %>% filter(party!="UDC") %>%  summarise(number = sum(number))
y$party <- "UDC"

###plr####
y1 <- V %>% group_by(issue, Canton) %>% filter(party!="FDP") %>%  summarise(number = sum(number))
y1$party <- "FDP"

###ps####
y2 <- V %>% group_by(issue, Canton) %>% filter(party!="PS") %>%  summarise(number = sum(number))
y2$party <- "PS"

###pdr####
y3 <- V %>% group_by(issue, Canton) %>% filter(party!="PDC") %>%  summarise(number = sum(number))
y3$party <- "PDC"

###verts####
y4 <- V %>% group_by(issue, Canton) %>% filter(party!="Verts") %>%  summarise(number = sum(number))
y4$party <- "Verts"

###pbd####
y5 <- V %>% group_by(issue, Canton) %>% filter(party!="PDB") %>%  summarise(number = sum(number))
y5$party <- "PDB"

###verts_lib####
y6 <- V %>% group_by(issue, Canton) %>% filter(party!="Verts_lib") %>%  summarise(number = sum(number))
y6$party <- "Verts_lib"

###regrouper les differents dataset####
Y <- rbind(y, y1, y2, y3, y4, y5, y6)

Y <- rename(Y, number_other=number)

X1 <-left_join(X1, Y, by=c("Canton", "issue", "party"))

#### drop observations CH ----
X1 <- X1[!(X1$Canton=="CH"),]

### ajouter les observations manquantes ----
issue <- c("economy", "immigration", "environment", "social", "EU")
canton <- c("ZH", "BE", "LU", "UR", "SZ", "OW", "NW", 
            "GL", "ZG", "FR", "SO", "BS", "BL", "SH", "AR", "AI", "SG", "GR", "AG", "TG", "TI", "VD", "VS", "NE", "GE","JU")
party <- c("FDP", "PS", "PDC", "UDC", "Verts", "Verts_lib", "PDB")

data <- as.data.frame(list(Canton = rep(canton, 7*5),
                           party = rep(rep(party, each = 26), 5),
                           issue = rep(issue, each = 26*7)))


X1 <- merge(X1, data, by.x  = c("Canton", "party", "issue"), all.y = TRUE)

X1[is.na(X1)] <- 0

### drop les cantons ou les partis ne se sont pas presentes----
# 30 cantons 

# PLR - 5 cantons
X1 <- X1[!((X1$Canton=="AI" |
              X1$Canton=="GL" |
              X1$Canton=="UR" |
              X1$Canton=="OW" |
              X1$Canton=="NW") &
             X1$party=="FDP"),]

# PS - 3 cantons
X1 <- X1[!((X1$Canton=="UR" |
              X1$Canton=="OW" |
              X1$Canton=="NW") &
             X1$party=="PS"),]

# UDC - 2 cantons
X1 <- X1[!((X1$Canton=="AI" |
              X1$Canton=="GL") &
             X1$party=="UDC"),]

# PDC - 5 cantons
X1 <- X1[!((X1$Canton=="AR" |
              X1$Canton=="GL" |
              X1$Canton=="SH" |
              X1$Canton=="OW" |
              X1$Canton=="NW") &
             X1$party=="PDC"),]

# VERTS - 6 cantons
X1 <- X1[!((X1$Canton=="AI" |
              X1$Canton=="AR" |
              X1$Canton=="GL" |
              X1$Canton=="GR" |
              X1$Canton=="OW" |
              X1$Canton=="NW") &
             X1$party=="Verts"),]






#######################################################################################################################

### Voters data

### creer les variables de potential voters ----
S <- S %>% replace_with_na(replace = list(f14400a = 99,
                                          f14400b = 99,
                                          f14400c = 99,
                                          f14400d = 99,
                                          f14400e = 99))

S$mean_prob <- ((S$f14400a+S$f14400b+S$f14400c+S$f14400d+S$f14400e)/5)

S <- S %>% drop_na(mean_prob)

# FDP
S$prob_fdp <- (S$f14400a-S$mean_prob)

S <- S %>% mutate(potential_fdp = case_when(S$f10300main7==2 ~ 2,
                                            S$prob_fdp>0 ~ 1,
                                            S$prob_fdp<=0 ~ 0))

# CVP
S$prob_cvp <- (S$f14400b-S$mean_prob)

S <- S %>% mutate(potential_cvp = case_when(S$f10300main7==4 ~ 2,
                                            S$prob_cvp>0 ~ 1,
                                            S$prob_cvp<=0 ~ 0))

# SP
S$prob_sp <- (S$f14400c-S$mean_prob)

S <- S %>% mutate(potential_sp = case_when(S$f10300main7==6 ~ 2,
                                           S$prob_sp>0 ~ 1,
                                           S$prob_sp<=0 ~ 0))

# SVP
S$prob_svp <- (S$f14400d-S$mean_prob)

S <- S %>% mutate(potential_svp = case_when(S$f10300main7==1 ~ 2,
                                            S$prob_svp>0 ~ 1,
                                            S$prob_svp<=0 ~ 0))

# GPS
S$prob_gps <- (S$f14400e-S$mean_prob)

S <- S %>% mutate(potential_gps = case_when(S$f10300main7==7 ~ 2,
                                            S$prob_gps>0 ~ 1,
                                            S$prob_gps<=0 ~ 0))

# changer le nom des variables pour simplifier la suite
S$UDC_potential <- S$potential_svp
S$PLR_potential <- S$potential_fdp
S$PS_potential <- S$potential_sp
S$PDC_potential <- S$potential_cvp
S$VERTS_potential <- S$potential_gps



#### recodage pour utiliser vote_party= last vote et pas vote intention ----

S <- S %>% mutate(canton = case_when(canton ==0 ~ "CH",
                                     canton ==1 ~ "ZH",
                                     canton ==2 ~ "BE", 
                                     canton ==3 ~ "LU",
                                     canton ==4 ~ "UR",
                                     canton ==5 ~ "SZ",
                                     canton ==6 ~ "OW",
                                     canton ==7 ~ "NW",
                                     canton ==8 ~ "GL",
                                     canton ==9 ~ "ZG",
                                     canton ==10 ~ "FR",
                                     canton ==11 ~ "SO",
                                     canton ==12 ~ "BS",
                                     canton ==13 ~ "BL",
                                     canton ==14 ~ "SH",
                                     canton ==15 ~ "AR",
                                     canton ==16 ~ "AI",
                                     canton ==17 ~ "SG",
                                     canton ==18 ~ "GR",
                                     canton ==19 ~ "AG",
                                     canton ==20 ~ "TG",
                                     canton ==21 ~ "TI",
                                     canton ==22 ~ "VD",
                                     canton ==23 ~ "VS",
                                     canton ==24 ~ "NE",
                                     canton ==25 ~ "GE",
                                     canton ==26 ~ "JU"))


### POTENTIAL - prendre les moyennes pour la saillance des enjeux ####

### recodage ----
S <- S %>% mutate(issue_EU = case_when(f15310a==1 ~ "Very important",
                                       f15310a==2 ~ "Important",
                                       f15310a==3 ~ "Rather important",
                                       f15310a==4 ~ "Rather Not important"))
#immigration
S <- S %>% mutate(issue_immigration = case_when(f15310b==1 ~ "Very important",
                                                f15310b==2 ~ "Important",
                                                f15310b==3 ~ "Rather important",
                                                f15310b==4 ~ "Rather Not important"))
#social
S <- S %>% mutate(issue_social = case_when(f15310c==1 ~ "Very important",
                                           f15310c==2 ~ "Important",
                                           f15310c==3 ~ "Rather important",
                                           f15310c==4 ~ "Rather Not important"))
#Environment
S <- S %>% mutate(issue_environment = case_when(f15310d==1 ~ "Very important",
                                                f15310d==2 ~ "Important",
                                                f15310d==3 ~ "Rather important",
                                                f15310d==4 ~ "Rather Not important"))
#economy
S <- S %>% mutate(issue_economy = case_when(f15310e==1 ~ "Very important",
                                            f15310e==2 ~ "Important",
                                            f15310e==3 ~ "Rather important",
                                            f15310e==4 ~ "Rather Not important"))


S1 <- S
### POTENTIAL ----
###udc-immigration####
#pourcentage
u1 <- S1 %>%  group_by(canton, UDC_potential) %>%
  filter(!is.na(issue_immigration)) %>%
  count(issue_immigration, UDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
u1$party <- "UDC"
u1$issue <- "immigration"
### garder uniquement potential et very important par canton
u1 <- filter(u1, UDC_potential==1 & issue_immigration=="Very important")

####udc-EU#####
u2 <- S1 %>%  group_by(canton, UDC_potential) %>%
  filter(!is.na(issue_EU)) %>%
  count(issue_EU, UDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
u2$party <- "UDC"
u2$issue <- "EU"

### garder uniquement potential et very important par canton
u2 <- filter(u2, UDC_potential==1 & issue_EU=="Very important")
####udc-environment#####
u3 <- S1 %>%  group_by(canton, UDC_potential) %>%
  filter(!is.na(issue_environment)) %>%
  count(issue_environment, UDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
u3$party <- "UDC"

u3$issue <- "environment"

### garder uniquement potential et very important par canton
u3 <- filter(u3, UDC_potential==1 & issue_environment=="Very important")
####udc-economy#####
u4 <- S1 %>%  group_by(canton, UDC_potential) %>%
  filter(!is.na(issue_economy)) %>%
  count(issue_economy, UDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
u4$party <- "UDC"

u4$issue <- "economy"

### garder uniquement potential et very important par canton
u4 <- filter(u4, UDC_potential==1 & issue_economy=="Very important")
####udc-social#####
u5 <- S1 %>%  group_by(canton, UDC_potential) %>%
  filter(!is.na(issue_social)) %>%
  count(issue_social, UDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
u5$party <- "UDC"

u5$issue <- "social"

### garder uniquement potential et very important par canton
u5 <- filter(u5, UDC_potential==1 & issue_social=="Very important")

###plr-immigration####
#pourcentage
p1 <- S1 %>%  group_by(canton, PLR_potential) %>%
  filter(!is.na(issue_immigration)) %>%
  count(issue_immigration, PLR_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
p1$party <- "PLR"
p1$issue <- "immigration"

### garder uniquement potential et very important par canton
p1 <- filter(p1, PLR_potential==1 & issue_immigration=="Very important")
###plr-EU####
#pourcentage
p2 <- S1 %>%  group_by(canton, PLR_potential) %>%
  filter(!is.na(issue_EU)) %>%
  count(issue_EU, PLR_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
p2$party <- "PLR"

p2$issue <- "EU"

### garder uniquement potential et very important par canton
p2 <- filter(p2, PLR_potential==1 & issue_EU=="Very important")
###plr-environment####
#pourcentage
p3 <- S1 %>%  group_by(canton, PLR_potential) %>%
  filter(!is.na(issue_environment)) %>%
  count(issue_environment, PLR_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
p3$party <- "PLR"

p3$issue <- "environment"

### garder uniquement potential et very important par canton
p3 <- filter(p3, PLR_potential==1 & issue_environment=="Very important")
###plr-economy####
#pourcentage
p4 <- S1 %>%  group_by(canton, PLR_potential) %>%
  filter(!is.na(issue_economy)) %>%
  count(issue_economy, PLR_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
p4$party <- "PLR"

p4$issue <- "economy"

### garder uniquement potential et very important par canton
p4 <- filter(p4, PLR_potential==1 & issue_economy=="Very important")
###plr-social####
#pourcentage
p5 <- S1 %>%  group_by(canton, PLR_potential) %>%
  filter(!is.na(issue_social)) %>%
  count(issue_social, PLR_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
p5$party <- "PLR"

p5$issue <- "social"

### garder uniquement potential et very important par canton
p5 <- filter(p5, PLR_potential==1 & issue_social=="Very important")

###ps-immigration####
#pourcentage
s1 <- S1 %>%  group_by(canton, PS_potential) %>%
  filter(!is.na(issue_immigration)) %>%
  count(issue_immigration, PS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
s1$party <- "PS"

s1$issue <- "immigration"

### garder uniquement potential et very important par canton
s1 <- filter(s1, PS_potential==1 & issue_immigration=="Very important")
###ps-EU####
#pourcentage
s2 <- S1 %>%  group_by(canton, PS_potential) %>%
  filter(!is.na(issue_EU)) %>%
  count(issue_EU, PS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
s2$party <- "PS"

s2$issue <- "EU"

### garder uniquement potential et very important par canton
s2 <- filter(s2, PS_potential==1 & issue_EU=="Very important")
###ps-environment####
#pourcentage
s3 <- S1 %>%  group_by(canton, PS_potential) %>%
  filter(!is.na(issue_environment)) %>%
  count(issue_environment, PS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
s3$party <- "PS"

s3$issue <- "environment"

### garder uniquement potential et very important par canton
s3 <- filter(s3, PS_potential==1 & issue_environment=="Very important")
###ps-economy####
#pourcentage
s4 <- S1 %>%  group_by(canton, PS_potential) %>%
  filter(!is.na(issue_economy)) %>%
  count(issue_economy, PS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
s4$party <- "PS"

s4$issue <- "economy"

### garder uniquement potential et very important par canton
s4 <- filter(s4, PS_potential==1 & issue_economy=="Very important")
###ps-social####
#pourcentage
s5 <- S1 %>%  group_by(canton, PS_potential) %>%
  filter(!is.na(issue_social)) %>%
  count(issue_social, PS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
s5$party <- "PS"

s5$issue <- "social"

### garder uniquement potential et very important par canton
s5 <- filter(s5, PS_potential==1 & issue_social=="Very important")
###pdc-immigration####
#pourcentage
d1 <- S1 %>%  group_by(canton, PDC_potential) %>%
  filter(!is.na(issue_immigration)) %>%
  count(issue_immigration, PDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
d1$party <- "PDC"

d1$issue <- "immigration"

### garder uniquement potential et very important par canton
d1 <- filter(d1, PDC_potential==1 & issue_immigration=="Very important")
###pdc-EU####
#pourcentage
d2 <- S1 %>%  group_by(canton, PDC_potential) %>%
  filter(!is.na(issue_EU)) %>%
  count(issue_EU, PDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
d2$party <- "PDC"

d2$issue <- "EU"

### garder uniquement potential et very important par canton
d2 <- filter(d2, PDC_potential==1 & issue_EU=="Very important")
###pdc-environment####
#pourcentage
d3 <- S1 %>%  group_by(canton, PDC_potential) %>%
  filter(!is.na(issue_environment)) %>%
  count(issue_environment, PDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
d3$party <- "PDC"

d3$issue <- "environment"

### garder uniquement potential et very important par canton
d3 <- filter(d3, PDC_potential==1 & issue_environment=="Very important")
###pdc-economy####
#pourcentage
d4 <- S1 %>%  group_by(canton, PDC_potential) %>%
  filter(!is.na(issue_economy)) %>%
  count(issue_economy, PDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
d4$party <- "PDC"
d4$issue <- "economy"
### garder uniquement potential et very important par canton
d4 <- filter(d4, PDC_potential==1 & issue_economy=="Very important")
###pdc-social####
#pourcentage
d5 <- S1 %>%  group_by(canton, PDC_potential) %>%
  filter(!is.na(issue_social)) %>%
  count(issue_social, PDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
d5$party <- "PDC"
d5$issue <- "social"
### garder uniquement potential et very important par canton
d5 <- filter(d5, PDC_potential==1 & issue_social=="Very important")
###verts-immigration####
#pourcentage
v1 <- S1 %>%  group_by(canton, VERTS_potential) %>%
  filter(!is.na(issue_immigration)) %>%
  count(issue_immigration, VERTS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
v1$party <- "Verts"
v1$issue <- "immigration"
### garder uniquement potential et very important par canton
v1 <- filter(v1, VERTS_potential==1 & issue_immigration=="Very important")
###verts-EU####
#pourcentage
v2 <- S1 %>%  group_by(canton, VERTS_potential) %>%
  filter(!is.na(issue_EU)) %>%
  count(issue_EU, VERTS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
v2$party <- "Verts"
v2$issue <- "EU"
### garder uniquement potential et very important par canton
v2 <- filter(v2, VERTS_potential==1 & issue_EU=="Very important")
###verts-enviroment####
#pourcentage
v3 <- S1 %>%  group_by(canton, VERTS_potential) %>%
  filter(!is.na(issue_environment)) %>%
  count(issue_environment, VERTS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
v3$party <- "Verts"
v3$issue <- "environment"
### garder uniquement potential et very important par canton
v3 <- filter(v3, VERTS_potential==1 & issue_environment=="Very important")
###verts-economy####
#pourcentage
v4 <- S1 %>%  group_by(canton, VERTS_potential) %>%
  filter(!is.na(issue_economy)) %>%
  count(issue_economy, VERTS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
v4$party <- "Verts"
v4$issue <- "economy"
### garder uniquement potential et very important par canton
v4 <- filter(v4, VERTS_potential==1 & issue_economy=="Very important")
###verts-social####
#pourcentage
v5 <- S1 %>%  group_by(canton, VERTS_potential) %>%
  filter(!is.na(issue_social)) %>%
  count(issue_social, VERTS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
v5$party <- "Verts"
v5$issue <- "social"
### garder uniquement potential et very important par canton
v5 <- filter(v5, VERTS_potential==1 & issue_social=="Very important")



###regrouper les datasets des parti-enjeux#####
potential <- rbind(u1, u2, u3, u4, u5, p1, p2, p3, p4, p5, s1, s2, s3, s4, s5, 
                   d1, d2, d3, d4, d5, v1, v2, v3, v4, v5)

potential <- rename(potential, Canton=canton)
potential <- potential[!is.na(potential$Canton),]
potential <- rename(potential, N.p=n)
potential <- select(potential, Canton, prop, party, issue, N.p)
potential$party <- recode(potential$party, PLR="FDP")


final <-left_join(X1, potential, by=c("Canton", "issue", "party"))
final <- select(final, Canton, party, issue, number, number_other, prop, N.p)

### VOTERS ####

S <- S %>% replace_with_na(replace = list(f10300main7 = 99))

S <- S %>% mutate(vote_udc = case_when(S$f10300main7==1 ~ 1,
                                       S$f10300main7==2 ~ 0,
                                       S$f10300main7==3 ~ 0,
                                       S$f10300main7==4 ~ 0,
                                       S$f10300main7==5 ~ 0,
                                       S$f10300main7==6 ~ 0,
                                       S$f10300main7==7 ~ 0,
                                       S$f10300main7==8 ~ 0))

S <- S %>% mutate(vote_plr = case_when(S$f10300main7==1 ~ 0,
                                       S$f10300main7==2 ~ 1,
                                       S$f10300main7==3 ~ 0,
                                       S$f10300main7==4 ~ 0,
                                       S$f10300main7==5 ~ 0,
                                       S$f10300main7==6 ~ 0,
                                       S$f10300main7==7 ~ 0,
                                       S$f10300main7==8 ~ 0))

S <- S %>% mutate(vote_pdc = case_when(S$f10300main7==1 ~ 0,
                                       S$f10300main7==2 ~ 0,
                                       S$f10300main7==3 ~ 0,
                                       S$f10300main7==4 ~ 1,
                                       S$f10300main7==5 ~ 0,
                                       S$f10300main7==6 ~ 0,
                                       S$f10300main7==7 ~ 0,
                                       S$f10300main7==8 ~ 0))

S <- S %>% mutate(vote_ps = case_when(S$f10300main7==1 ~ 0,
                                       S$f10300main7==2 ~ 0,
                                       S$f10300main7==3 ~ 0,
                                       S$f10300main7==4 ~ 0,
                                       S$f10300main7==5 ~ 0,
                                       S$f10300main7==6 ~ 1,
                                       S$f10300main7==7 ~ 0,
                                       S$f10300main7==8 ~ 0))

S <- S %>% mutate(vote_verts = case_when(S$f10300main7==1 ~ 0,
                                       S$f10300main7==2 ~ 0,
                                       S$f10300main7==3 ~ 0,
                                       S$f10300main7==4 ~ 0,
                                       S$f10300main7==5 ~ 0,
                                       S$f10300main7==6 ~ 0,
                                       S$f10300main7==7 ~ 1,
                                       S$f10300main7==8 ~ 0))





###udc-immigration####
#pourcentage
u1 <- S1 %>%  group_by(canton, UDC_potential) %>% 
  filter(!is.na(issue_immigration)) %>% 
  count(issue_immigration, UDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
u1$party <- "UDC"
u1$issue <- "immigration"
### garder uniquement potential et very important par canton
u1 <- filter(u1, UDC_potential==2 & issue_immigration=="Very important")

####udc-EU#####
u2 <- S1 %>%  group_by(canton, UDC_potential) %>%
  filter(!is.na(issue_EU)) %>%
  count(issue_EU, UDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
u2$party <- "UDC"
u2$issue <- "EU"

### garder uniquement potential et very important par canton
u2 <- filter(u2, UDC_potential==2 & issue_EU=="Very important")
####udc-environment#####
u3 <- S1 %>%  group_by(canton, UDC_potential) %>%
  filter(!is.na(issue_environment)) %>%
  count(issue_environment, UDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
u3$party <- "UDC"

u3$issue <- "environment"

### garder uniquement potential et very important par canton
u3 <- filter(u3, UDC_potential==2 & issue_environment=="Very important")
####udc-economy#####
u4 <- S1 %>%  group_by(canton, UDC_potential) %>%
  filter(!is.na(issue_economy)) %>%
  count(issue_economy, UDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
u4$party <- "UDC"

u4$issue <- "economy"

### garder uniquement potential et very important par canton
u4 <- filter(u4, UDC_potential==2 & issue_economy=="Very important")
####udc-social#####
u5 <- S1 %>%  group_by(canton, UDC_potential) %>%
  filter(!is.na(issue_social)) %>%
  count(issue_social, UDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
u5$party <- "UDC"

u5$issue <- "social"

### garder uniquement potential et very important par canton
u5 <- filter(u5, UDC_potential==2 & issue_social=="Very important")

###plr-immigration####
#pourcentage
p1 <- S1 %>%  group_by(canton, PLR_potential) %>%
  filter(!is.na(issue_immigration)) %>%
  count(issue_immigration, PLR_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
p1$party <- "PLR"

p1$issue <- "immigration"

### garder uniquement potential et very important par canton
p1 <- filter(p1, PLR_potential==2 & issue_immigration=="Very important")
###plr-EU####
#pourcentage
p2 <- S1 %>%  group_by(canton, PLR_potential) %>%
  filter(!is.na(issue_EU)) %>%
  count(issue_EU, PLR_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
p2$party <- "PLR"

p2$issue <- "EU"

### garder uniquement potential et very important par canton
p2 <- filter(p2, PLR_potential==2 & issue_EU=="Very important")
###plr-environment####
#pourcentage
p3 <- S1 %>%  group_by(canton, PLR_potential) %>%
  filter(!is.na(issue_environment)) %>%
  count(issue_environment, PLR_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
p3$party <- "PLR"

p3$issue <- "environment"

### garder uniquement potential et very important par canton
p3 <- filter(p3, PLR_potential==2 & issue_environment=="Very important")
###plr-economy####
#pourcentage
p4 <- S1 %>%  group_by(canton, PLR_potential) %>%
  filter(!is.na(issue_economy)) %>%
  count(issue_economy, PLR_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
p4$party <- "PLR"

p4$issue <- "economy"

### garder uniquement potential et very important par canton
p4 <- filter(p4, PLR_potential==2 & issue_economy=="Very important")
###plr-social####
#pourcentage
p5 <- S1 %>%  group_by(canton, PLR_potential) %>%
  filter(!is.na(issue_social)) %>%
  count(issue_social, PLR_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
p5$party <- "PLR"

p5$issue <- "social"

### garder uniquement potential et very important par canton
p5 <- filter(p5, PLR_potential==2 & issue_social=="Very important")

###ps-immigration####
#pourcentage
s1 <- S1 %>%  group_by(canton, PS_potential) %>%
  filter(!is.na(issue_immigration)) %>%
  count(issue_immigration, PS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
s1$party <- "PS"

s1$issue <- "immigration"

### garder uniquement potential et very important par canton
s1 <- filter(s1, PS_potential==2 & issue_immigration=="Very important")
###ps-EU####
#pourcentage
s2 <- S1 %>%  group_by(canton, PS_potential) %>%
  filter(!is.na(issue_EU)) %>%
  count(issue_EU, PS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
s2$party <- "PS"

s2$issue <- "EU"

### garder uniquement potential et very important par canton
s2 <- filter(s2, PS_potential==2 & issue_EU=="Very important")
###ps-environment####
#pourcentage
s3 <- S1 %>%  group_by(canton, PS_potential) %>%
  filter(!is.na(issue_environment)) %>%
  count(issue_environment, PS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
s3$party <- "PS"

s3$issue <- "environment"

### garder uniquement potential et very important par canton
s3 <- filter(s3, PS_potential==2 & issue_environment=="Very important")
###ps-economy####
#pourcentage
s4 <- S1 %>%  group_by(canton, PS_potential) %>%
  filter(!is.na(issue_economy)) %>%
  count(issue_economy, PS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
s4$party <- "PS"

s4$issue <- "economy"

### garder uniquement potential et very important par canton
s4 <- filter(s4, PS_potential==2 & issue_economy=="Very important")
###ps-social####
#pourcentage
s5 <- S1 %>%  group_by(canton, PS_potential) %>%
  filter(!is.na(issue_social)) %>%
  count(issue_social, PS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
s5$party <- "PS"

s5$issue <- "social"

### garder uniquement potential et very important par canton
s5 <- filter(s5, PS_potential==2 & issue_social=="Very important")
###pdc-immigration####
#pourcentage
d1 <- S1 %>%  group_by(canton, PDC_potential) %>%
  filter(!is.na(issue_immigration)) %>%
  count(issue_immigration, PDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
d1$party <- "PDC"

d1$issue <- "immigration"

### garder uniquement potential et very important par canton
d1 <- filter(d1, PDC_potential==2 & issue_immigration=="Very important")
###pdc-EU####
#pourcentage
d2 <- S1 %>%  group_by(canton, PDC_potential) %>%
  filter(!is.na(issue_EU)) %>%
  count(issue_EU, PDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
d2$party <- "PDC"

d2$issue <- "EU"

### garder uniquement potential et very important par canton
d2 <- filter(d2, PDC_potential==2 & issue_EU=="Very important")
###pdc-environment####
#pourcentage
d3 <- S1 %>%  group_by(canton, PDC_potential) %>%
  filter(!is.na(issue_environment)) %>%
  count(issue_environment, PDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
d3$party <- "PDC"

d3$issue <- "environment"

### garder uniquement potential et very important par canton
d3 <- filter(d3, PDC_potential==2 & issue_environment=="Very important")
###pdc-economy####
#pourcentage
d4 <- S1 %>%  group_by(canton, PDC_potential) %>%
  filter(!is.na(issue_economy)) %>%
  count(issue_economy, PDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
d4$party <- "PDC"
d4$issue <- "economy"
### garder uniquement potential et very important par canton
d4 <- filter(d4, PDC_potential==2 & issue_economy=="Very important")
###pdc-social####
#pourcentage
d5 <- S1 %>%  group_by(canton, PDC_potential) %>%
  filter(!is.na(issue_social)) %>%
  count(issue_social, PDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
d5$party <- "PDC"
d5$issue <- "social"
### garder uniquement potential et very important par canton
d5 <- filter(d5, PDC_potential==2 & issue_social=="Very important")
###verts-immigration####
#pourcentage
v1 <- S1 %>%  group_by(canton, VERTS_potential) %>%
  filter(!is.na(issue_immigration)) %>%
  count(issue_immigration, VERTS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
v1$party <- "Verts"
v1$issue <- "immigration"
### garder uniquement potential et very important par canton
v1 <- filter(v1, VERTS_potential==2 & issue_immigration=="Very important")
###verts-EU####
#pourcentage
v2 <- S1 %>%  group_by(canton, VERTS_potential) %>%
  filter(!is.na(issue_EU)) %>%
  count(issue_EU, VERTS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
v2$party <- "Verts"
v2$issue <- "EU"
### garder uniquement potential et very important par canton
v2 <- filter(v2, VERTS_potential==2 & issue_EU=="Very important")
###verts-enviroment####
#pourcentage
v3 <- S1 %>%  group_by(canton, VERTS_potential) %>%
  filter(!is.na(issue_environment)) %>%
  count(issue_environment, VERTS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
v3$party <- "Verts"
v3$issue <- "environment"
### garder uniquement potential et very important par canton
v3 <- filter(v3, VERTS_potential==2 & issue_environment=="Very important")
###verts-economy####
#pourcentage
v4 <- S1 %>%  group_by(canton, VERTS_potential) %>%
  filter(!is.na(issue_economy)) %>%
  count(issue_economy, VERTS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
v4$party <- "Verts"
v4$issue <- "economy"
### garder uniquement potential et very important par canton
v4 <- filter(v4, VERTS_potential==2 & issue_economy=="Very important")
###verts-social####
#pourcentage
v5 <- S1 %>%  group_by(canton, VERTS_potential) %>%
  filter(!is.na(issue_social)) %>%
  count(issue_social, VERTS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
v5$party <- "Verts"
v5$issue <- "social"
### garder uniquement potential et very important par canton
v5 <- filter(v5, VERTS_potential==2 & issue_social=="Very important")



###regrouper les datasets des parti-enjeux#####
voters <- rbind(u1, u2, u3, u4, u5, p1, p2, p3, p4, p5, s1, s2, s3, s4, s5, 
                d1, d2, d3, d4, d5, v1, v2, v3, v4, v5)

voters <- rename(voters, Canton=canton)
voters <- voters[!is.na(voters$Canton),]
voters <- rename(voters, N.v=n)
voters <- select(voters, Canton, prop, party, issue, N.v)
voters$party <- recode(voters$party, PLR="FDP")

final <-left_join(final, voters, by=c("Canton", "issue", "party"))
### PUBLIC_2 ####
###immigration####
#pourcentage
I <- S %>%  group_by(canton) %>%
  count(issue_immigration) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
I$issue <- "immigration"
### garder uniquement potential et very important par canton
I <- filter(I, issue_immigration=="Very important")

###EU####
#pourcentage
EU <- S %>%  group_by(canton) %>%
  count(issue_EU) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
EU$issue <- "EU"
### garder uniquement potential et very important par canton
EU <- filter(EU, issue_EU=="Very important")

###social####
SO <- S %>%  group_by(canton) %>%
  count(issue_social) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
SO$issue <- "social"
### garder uniquement potential et very important par canton
SO <- filter(SO, issue_social=="Very important")

###economy####
eco <- S %>%  group_by(canton) %>%
  count(issue_economy) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
eco$issue <- "economy"
### garder uniquement potential et very important par canton
eco <- filter(eco, issue_economy=="Very important")

###environment####
ev <- S %>%  group_by(canton) %>%
  count(issue_environment) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
ev$issue <- "environment"
### garder uniquement potential et very important par canton
ev <- filter(ev, issue_environment=="Very important")


###combiner saillances generales####
i_i <- rbind(I, EU, SO, eco, ev)

#Canton
i_i <- rename(i_i, Canton=canton)
i_i <- rename(i_i, public_2=prop)
final <-left_join(final, i_i, by=c("Canton", "issue"))


### cleaning ----

final <- select(final, Canton, party, issue, number, number_other, prop.x, N.p, prop.y, N.v, public, public_2)

final <- rename(final, potential=prop.x)
final <- rename(final, voters=prop.y)


final <- final[!(final$party=="PDB"),]

### variables de controle ----
### party size ----
# https://www.bfs.admin.ch/bfs/fr/home/statistiques/politique/elections/conseil-national/force-partis.html#-1923937103
final <- final %>% mutate(party_size=case_when(party=="FDP" & Canton=="ZH" ~ 15.3,
                                               party=="FDP" & Canton=="BE" ~ 9.3,
                                               party=="FDP" & Canton=="LU" ~ 18.5,
                                               party=="FDP" & Canton=="UR" ~ 0,
                                               party=="FDP" & Canton=="SZ" ~ 20.6,
                                               party=="FDP" & Canton=="OW" ~ 0,
                                               party=="FDP" & Canton=="NW" ~ 0,
                                               party=="FDP" & Canton=="GL" ~ 0,
                                               party=="FDP" & Canton=="ZG" ~ 17.6,
                                               party=="FDP" & Canton=="FR" ~ 14.2,
                                               party=="FDP" & Canton=="SO" ~ 21.2,
                                               party=="FDP" & Canton=="BS" ~ 9.8,
                                               party=="FDP" & Canton=="BL" ~ 15.8,
                                               party=="FDP" & Canton=="SH" ~ 12.9,
                                               party=="FDP" & Canton=="AR" ~ 33.6,
                                               party=="FDP" & Canton=="AI" ~ 76.3,
                                               party=="FDP" & Canton=="SG" ~ 14.3,
                                               party=="FDP" & Canton=="GR" ~ 13.3,
                                               party=="FDP" & Canton=="AG" ~ 15.1,
                                               party=="FDP" & Canton=="TG" ~ 13.0,
                                               party=="FDP" & Canton=="TI" ~ 23.7,
                                               party=="FDP" & Canton=="VD" ~ 26.8,
                                               party=="FDP" & Canton=="VS" ~ 18.1,
                                               party=="FDP" & Canton=="NE" ~ 24.4,
                                               party=="FDP" & Canton=="GE" ~ 20.5,
                                               party=="FDP" & Canton=="JU" ~ 16.8,
                                               party=="PDC" & Canton=="ZH" ~ 4.2,
                                               party=="PDC" & Canton=="BE" ~ 1.8,
                                               party=="PDC" & Canton=="LU" ~ 23.9,
                                               party=="PDC" & Canton=="UR" ~ 26.8,
                                               party=="PDC" & Canton=="SZ" ~ 19.5,
                                               party=="PDC" & Canton=="OW" ~ 0,
                                               party=="PDC" & Canton=="NW" ~ 0,
                                               party=="PDC" & Canton=="GL" ~ 0,
                                               party=="PDC" & Canton=="ZG" ~ 26.4,
                                               party=="PDC" & Canton=="FR" ~ 22.7,
                                               party=="PDC" & Canton=="SO" ~ 14.8,
                                               party=="PDC" & Canton=="BS" ~ 6.4,
                                               party=="PDC" & Canton=="BL" ~ 9.1,
                                               party=="PDC" & Canton=="SH" ~ 0,
                                               party=="PDC" & Canton=="AR" ~ 0,
                                               party=="PDC" & Canton=="AI" ~ 76.3,
                                               party=="PDC" & Canton=="SG" ~ 16.6,
                                               party=="PDC" & Canton=="GR" ~ 16.8,
                                               party=="PDC" & Canton=="AG" ~ 8.6,
                                               party=="PDC" & Canton=="TG" ~ 13.1,
                                               party=="PDC" & Canton=="TI" ~ 20.1,
                                               party=="PDC" & Canton=="VD" ~ 4.1,
                                               party=="PDC" & Canton=="VS" ~ 39.8,
                                               party=="PDC" & Canton=="NE" ~ 3.6,
                                               party=="PDC" & Canton=="GE" ~ 12.1,
                                               party=="PDC" & Canton=="JU" ~ 27.6,
                                               party=="PS" & Canton=="ZH" ~ 21.4,
                                               party=="PS" & Canton=="BE" ~ 19.7,
                                               party=="PS" & Canton=="LU" ~ 13.6,
                                               party=="PS" & Canton=="UR" ~ 0,
                                               party=="PS" & Canton=="SZ" ~ 13.1,
                                               party=="PS" & Canton=="OW" ~ 0,
                                               party=="PS" & Canton=="NW" ~ 0,
                                               party=="PS" & Canton=="GL" ~ 45.0,
                                               party=="PS" & Canton=="ZG" ~ 13.8,
                                               party=="PS" & Canton=="FR" ~ 24.2,
                                               party=="PS" & Canton=="SO" ~ 20.0,
                                               party=="PS" & Canton=="BS" ~ 33.3,
                                               party=="PS" & Canton=="BL" ~ 22.2,
                                               party=="PS" & Canton=="SH" ~ 28.8,
                                               party=="PS" & Canton=="AR" ~ 28.6,
                                               party=="PS" & Canton=="AI" ~ 18.1,
                                               party=="PS" & Canton=="SG" ~ 14.2,
                                               party=="PS" & Canton=="GR" ~ 17.6,
                                               party=="PS" & Canton=="AG" ~ 16.1,
                                               party=="PS" & Canton=="TG" ~ 12.7,
                                               party=="PS" & Canton=="TI" ~ 15.9,
                                               party=="PS" & Canton=="VD" ~ 22.2,
                                               party=="PS" & Canton=="VS" ~ 13.3,
                                               party=="PS" & Canton=="NE" ~ 23.7,
                                               party=="PS" & Canton=="GE" ~ 19.9,
                                               party=="PS" & Canton=="JU" ~ 23.7,
                                               party=="UDC" & Canton=="ZH" ~ 30.7,
                                               party=="UDC" & Canton=="BE" ~ 33.1,
                                               party=="UDC" & Canton=="LU" ~ 28.5,
                                               party=="UDC" & Canton=="UR" ~ 44.1,
                                               party=="UDC" & Canton=="SZ" ~ 42.6,
                                               party=="UDC" & Canton=="OW" ~ 34.5,
                                               party=="UDC" & Canton=="NW" ~ 82.8,
                                               party=="UDC" & Canton=="GL" ~ 0,
                                               party=="UDC" & Canton=="ZG" ~ 30.5,
                                               party=="UDC" & Canton=="FR" ~ 25.9,
                                               party=="UDC" & Canton=="SO" ~ 28.8,
                                               party=="UDC" & Canton=="BS" ~ 17.6,
                                               party=="UDC" & Canton=="BL" ~ 29.8,
                                               party=="UDC" & Canton=="SH" ~ 45.3,
                                               party=="UDC" & Canton=="AR" ~ 36.1,
                                               party=="UDC" & Canton=="AI" ~ 0,
                                               party=="UDC" & Canton=="SG" ~ 35.8,
                                               party=="UDC" & Canton=="GR" ~ 29.7,
                                               party=="UDC" & Canton=="AG" ~ 38.0,
                                               party=="UDC" & Canton=="TG" ~ 39.9,
                                               party=="UDC" & Canton=="TI" ~ 11.3,
                                               party=="UDC" & Canton=="VD" ~ 22.6,
                                               party=="UDC" & Canton=="VS" ~ 22.1,
                                               party=="UDC" & Canton=="NE" ~ 20.4,
                                               party=="UDC" & Canton=="GE" ~ 17.6,
                                               party=="UDC" & Canton=="JU" ~ 12.8,
                                               party=="Verts" & Canton=="ZH" ~ 6.9,
                                               party=="Verts" & Canton=="BE" ~ 8.5,
                                               party=="Verts" & Canton=="LU" ~ 7.1,
                                               party=="Verts" & Canton=="UR" ~ 26.3,
                                               party=="Verts" & Canton=="SZ" ~ 1.4,
                                               party=="Verts" & Canton=="OW" ~ 0,
                                               party=="Verts" & Canton=="NW" ~ 0,
                                               party=="Verts" & Canton=="GL" ~ 0,
                                               party=="Verts" & Canton=="ZG" ~ 7.2,
                                               party=="Verts" & Canton=="FR" ~ 5.3,
                                               party=="Verts" & Canton=="SO" ~ 5.6,
                                               party=="Verts" & Canton=="BS" ~ 11.6,
                                               party=="Verts" & Canton=="BL" ~ 14.2,
                                               party=="Verts" & Canton=="SH" ~ 3.4,
                                               party=="Verts" & Canton=="AR" ~ 0,
                                               party=="Verts" & Canton=="AI" ~ 0,
                                               party=="Verts" & Canton=="SG" ~ 5.7,
                                               party=="Verts" & Canton=="GR" ~ 0,
                                               party=="Verts" & Canton=="AG" ~ 5.5,
                                               party=="Verts" & Canton=="TG" ~ 5.4,
                                               party=="Verts" & Canton=="TI" ~ 3.5,
                                               party=="Verts" & Canton=="VD" ~ 11.3,
                                               party=="Verts" & Canton=="VS" ~ 4.9,
                                               party=="Verts" & Canton=="NE" ~ 9.3,
                                               party=="Verts" & Canton=="GE" ~ 11.5,
                                               party=="Verts" & Canton=="JU" ~ 7.3))

### issue ownership ----
final <- final %>% mutate(ownership = case_when(party=="FDP" & issue=="economy" ~ 1,
                                                party=="UDC" & issue=="immigration" ~ 1, 
                                                party=="PS" & issue=="social" ~ 1,
                                                party=="Verts" & issue=="environment" ~ 1,
                                                party=="FDP" & issue=="EU" ~ 1,
                                                TRUE ~ 0))

### position ----

# udc
#potential
udc <- S %>% group_by(canton, UDC_potential) %>% filter(!is.na(f15201)) %>% filter(!(f15201==98)) %>% summarise_at(vars(f15201), mean)
udc <- filter(udc, UDC_potential==1)
udc <- rename(udc, position_potential=f15201)
udc$party <- "UDC"
#voters
udc_voter <- S %>% group_by(canton, vote_udc) %>% filter(!is.na(f15201)) %>% filter(!(f15201==98)) %>% summarise_at(vars(f15201), mean)
udc_voter <- filter(udc_voter, vote_udc==1)
udc_voter <- rename(udc_voter, position_voter=f15201)
udc_voter$party <- "UDC"
# fdp
#potential
fdp <- S %>% group_by(canton, PLR_potential) %>% filter(!is.na(f15201)) %>% filter(!(f15201==98)) %>% summarise_at(vars(f15201), mean)
fdp <- filter(fdp, PLR_potential==1)
fdp <- rename(fdp, position_potential=f15201)
fdp$party <- "FDP"
#voters
fdp_voter <- S %>% group_by(canton, vote_plr) %>% filter(!is.na(f15201)) %>% filter(!(f15201==98)) %>% summarise_at(vars(f15201), mean)
fdp_voter <- filter(fdp_voter, vote_plr==1)
fdp_voter <- rename(fdp_voter, position_voter=f15201)
fdp_voter$party <- "FDP"
# PS
#potential
ps <- S %>% group_by(canton, PS_potential) %>% filter(!is.na(f15201)) %>% filter(!(f15201==98)) %>% summarise_at(vars(f15201), mean)
ps <- filter(ps, PS_potential==1)
ps <- rename(ps, position_potential=f15201)
ps$party <- "PS"
#voters
ps_voter <- S %>% group_by(canton, vote_ps) %>% filter(!is.na(f15201)) %>% filter(!(f15201==98)) %>% summarise_at(vars(f15201), mean)
ps_voter <- filter(ps_voter, vote_ps==1)
ps_voter <- rename(ps_voter, position_voter=f15201)
ps_voter$party <- "PS"
# PDC
#potential
pdc <- S %>% group_by(canton, PDC_potential) %>% filter(!is.na(f15201)) %>% filter(!(f15201==98)) %>% summarise_at(vars(f15201), mean)
pdc <- filter(pdc, PDC_potential==1)
pdc <- rename(pdc, position_potential=f15201)
pdc$party <- "PDC"
#voters
pdc_voter <- S %>% group_by(canton, vote_pdc) %>% filter(!is.na(f15201)) %>% filter(!(f15201==98)) %>% summarise_at(vars(f15201), mean)
pdc_voter <- filter(pdc_voter, vote_pdc==1)
pdc_voter <- rename(pdc_voter, position_voter=f15201)
pdc_voter$party <- "PDC"
# VERTS
#potential
v <- S %>% group_by(canton, VERTS_potential) %>% filter(!is.na(f15201)) %>% filter(!(f15201==98)) %>% summarise_at(vars(f15201), mean)
v <- filter(v, VERTS_potential==1)
v <- rename(v, position_potential=f15201)
v$party <- "Verts"
#voters
v_voter <- S %>% group_by(canton, vote_verts) %>% filter(!is.na(f15201)) %>% filter(!(f15201==98)) %>% summarise_at(vars(f15201), mean)
v_voter <- filter(v_voter, vote_verts==1)
v_voter <- rename(v_voter, position_voter=f15201)
v_voter$party <- "Verts"


# bind potential
position_potential <- rbind(udc, fdp, ps, pdc, v)

#Canton
position_potential <- rename(position_potential, Canton=canton)

final <-left_join(final, position_potential, by=c("Canton", "party"))

# bind voter
position_voter <- rbind(udc_voter, fdp_voter, ps_voter, pdc_voter, v_voter)

#Canton

position_voter <- rename(position_voter, Canton=canton)

final <-left_join(final, position_voter, by=c("Canton", "party"))

### district magnitude ----
final <- final %>% mutate(district=case_when(Canton=="ZH" ~ 35,
                                             Canton=="BE" ~ 24,
                                             Canton=="LU" ~ 9,
                                             Canton=="UR" ~ 1,
                                             Canton=="SZ" ~ 4,
                                             Canton=="OW" ~ 1,
                                             Canton=="NW" ~ 1,
                                             Canton=="GL" ~ 1,
                                             Canton=="ZG" ~ 3,
                                             Canton=="FR" ~ 7,
                                             Canton=="SO" ~ 6,
                                             Canton=="BS" ~ 5,
                                             Canton=="BL" ~ 7,
                                             Canton=="SH" ~ 2,
                                             Canton=="AR" ~ 1,
                                             Canton=="AI" ~ 1,
                                             Canton=="SG" ~ 12,
                                             Canton=="GR" ~ 5,
                                             Canton=="AG" ~ 16,
                                             Canton=="TG" ~ 6,
                                             Canton=="TI" ~ 8,
                                             Canton=="VD" ~ 19,
                                             Canton=="VS" ~ 8,
                                             Canton=="NE" ~ 4,
                                             Canton=="GE" ~ 12,
                                             Canton=="JU" ~ 2))

### CANDIDATS DATA ----
C <- read_dta("candidats.dta")

###recodage----
C <- C %>% mutate(issue = case_when(issue==1 ~ "economy",
                                    issue==2 ~ "social",
                                    issue==3 ~ "environment",
                                    issue==4 ~ "EU",
                                    issue==5 ~ "immigration"))


C <- C %>% mutate(party = case_when(T9a==1 ~ "FDP",
                                    T9a==2 ~ "CVP",
                                    T9a==3 ~ "PS",
                                    T9a==4 ~ "SVP",
                                    T9a==5 ~ "Verts",
                                    T9a==6 ~ "Verts_lib",
                                    T9b==7 ~ "PBD"))

C <- C %>% mutate(canton = case_when(T6==0 ~ "CH",
                                     T6==1 ~ "ZH",
                                     T6==2 ~ "BE", 
                                     T6==3 ~ "LU",
                                     T6==4 ~ "UR",
                                     T6==5 ~ "SZ",
                                     T6==6 ~ "OW",
                                     T6==7 ~ "NW",
                                     T6==8 ~ "GL",
                                     T6==9 ~ "ZG",
                                     T6==10 ~ "FR",
                                     T6==11 ~ "SO",
                                     T6==12 ~ "BS",
                                     T6==13 ~ "BL",
                                     T6==14 ~ "SH",
                                     T6==15 ~ "AR",
                                     T6==16 ~ "AI",
                                     T6==17 ~ "SG",
                                     T6==18 ~ "GR",
                                     T6==19 ~ "AG",
                                     T6==20 ~ "TG",
                                     T6==21 ~ "TI",
                                     T6==22 ~ "VD",
                                     T6==23 ~ "VS",
                                     T6==24 ~ "NE",
                                     T6==25 ~ "GE",
                                     T6==26 ~ "JU"))

### position des partis a partir des candidats ----
C$f15201 <- C$C5a

C <- C %>% mutate(vote_udc=case_when(T9a==4 ~ 1,
                                     T9a==1 ~ 0,
                                     T9a==2 ~ 0,
                                     T9a==3 ~ 0,
                                     T9a==5 ~ 0,
                                     T9a==6 ~ 0,
                                     T9b==7 ~ 0,))
C <- C %>% mutate(vote_plr=case_when(T9a==1 ~ 1,
                                     T9a==4 ~ 0,
                                     T9a==2 ~ 0,
                                     T9a==3 ~ 0,
                                     T9a==5 ~ 0,
                                     T9a==6 ~ 0,
                                     T9b==7 ~ 0,))
C <- C %>% mutate(vote_ps=case_when(T9a==3 ~ 1,
                                    T9a==1 ~ 0,
                                    T9a==2 ~ 0,
                                    T9a==4 ~ 0,
                                    T9a==5 ~ 0,
                                    T9a==6 ~ 0,
                                    T9b==7 ~ 0,))
C <- C %>% mutate(vote_pdc=case_when(T9a==2 ~ 1,
                                     T9a==1 ~ 0,
                                     T9a==4 ~ 0,
                                     T9a==3 ~ 0,
                                     T9a==5 ~ 0,
                                     T9a==6 ~ 0,
                                     T9b==7 ~ 0,))
C <- C %>% mutate(vote_verts=case_when(T9a==5 ~ 1,
                                       T9a==1 ~ 0,
                                       T9a==2 ~ 0,
                                       T9a==3 ~ 0,
                                       T9a==4 ~ 0,
                                       T9a==6 ~ 0,
                                       T9b==7 ~ 0,))
C <- C %>% mutate(vote_verts_lib=case_when(T9a==6 ~ 1,
                                           T9a==1 ~ 0,
                                           T9a==2 ~ 0,
                                           T9a==3 ~ 0,
                                           T9a==5 ~ 0,
                                           T9a==4 ~ 0,
                                           T9b==7 ~ 0,))
C <- C %>% mutate(vote_pbd=case_when(T9b==7 ~ 1,
                                     T9a==1 ~ 0,
                                     T9a==2 ~ 0,
                                     T9a==3 ~ 0,
                                     T9a==5 ~ 0,
                                     T9a==6 ~ 0,
                                     T9a==4 ~ 0,))


# udc
#voters
udc_voter <- C %>% group_by(canton, vote_udc) %>% filter(!is.na(f15201)) %>% summarise_at(vars(f15201), mean)
udc_voter <- filter(udc_voter, vote_udc==1)
udc_voter <- rename(udc_voter, position_voter=f15201)
udc_voter$party <- "UDC"
# fdp
#voters
fdp_voter <- C %>% group_by(canton, vote_plr) %>% filter(!is.na(f15201)) %>% summarise_at(vars(f15201), mean)
fdp_voter <- filter(fdp_voter, vote_plr==1)
fdp_voter <- rename(fdp_voter, position_voter=f15201)
fdp_voter$party <- "FDP"
# PS
#voters
ps_voter <- C %>% group_by(canton, vote_ps) %>% filter(!is.na(f15201)) %>% summarise_at(vars(f15201), mean)
ps_voter <- filter(ps_voter, vote_ps==1)
ps_voter <- rename(ps_voter, position_voter=f15201)
ps_voter$party <- "PS"
# PDC
#voters
pdc_voter <- C %>% group_by(canton, vote_pdc) %>% filter(!is.na(f15201)) %>% summarise_at(vars(f15201), mean)
pdc_voter <- filter(pdc_voter, vote_pdc==1)
pdc_voter <- rename(pdc_voter, position_voter=f15201)
pdc_voter$party <- "PDC"
# VERTS
#voters
v_voter <- C %>% group_by(canton, vote_verts) %>% filter(!is.na(f15201)) %>% summarise_at(vars(f15201), mean)
v_voter <- filter(v_voter, vote_verts==1)
v_voter <- rename(v_voter, position_voter=f15201)
v_voter$party <- "Verts"


position_voter <- rbind(udc_voter, fdp_voter, ps_voter, pdc_voter, v_voter)

position_voter <- select(position_voter, canton, position_voter, party)

position_voter <- rename(position_voter, Canton=canton)
position_voter <- rename(position_voter, position_party=position_voter)

final <-left_join(final, position_voter, by=c("Canton", "party"))

#### regrouper par canton et parti + sortir mip ----
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

C <- C %>%  group_by(canton, party) %>% summarise_at(vars(issue), list(mip=getmode))

C <- rename(C, Canton=canton)
C <- rename(C, mip_party=mip)
C <- C[!is.na(C$party),]

final <-left_join(final, C, by=c("Canton", "party"))
### cleaning ----
rm(list=setdiff(ls(), "final"))
final <- select(final, Canton, party, issue, number, number_other, potential, N.p, voters, N.v, public_2, 
                position_voter, position_potential, ownership,
                party_size, district)
final$Canton <- as.factor(final$Canton)
final$party <- as.factor(final$party)
final$issue <- as.factor(final$issue)
final$party <- recode(final$party, PLR="FDP", PDC="CVP", PDB="BDP", PS="SP", UDC="SVP", Verts="Green", Verts_lib="Green_liberal")

# SP
final <- final[!(final$Canton=="AI" & final$party=="SP"),]
# VERTS
final <- final[!(final$Canton=="UR" & final$party=="Green"),]

final <- final[!(final$party=="Green_liberal"),]

final$potential <- final$potential %>%  replace_na(0)
final$voters <- final$voters %>%  replace_na(0)

save(final, file = "final_2015_prop_aout.Rdata")














###########################################################################################################################
############                                       2019                                                        ############
###########################################################################################################################
D <- read_dta("Wahlinserate_2019_allcantons.dta")
S <- read_dta("1184_Selects2019_Panel_Data_v1.0.0.dta")

### Advertisements data 

###drop les ads avant et apres la campagne ####
D$YYYYMMDD <- as.Date(as.character(D$YYYYMMDD), format='%Y%m%d')

# considerer 1 aout comme debut de campagne
D <- D %>% dplyr::filter(YYYYMMDD >= as.Date("2019-08-1") & YYYYMMDD <= as.Date("2019-10-20"))

#### creer variable pour descriptive ####
D$theme_n <- paste(D$Thema_EU, D$Thema_EnergieUmwelt, D$Thema_EnergieUmwelt_2, D$Thema_WirtschaftFinanzen,
                   D$Thema_Sozial, D$Thema_AsylMigration, D$Thema_Verkehr, D$Thema_Arbeit,
                   D$Thema_Rechtsstaat, D$Thema_Gesundheit, D$Thema_Bildung, D$Thema_Gender
                   , sep ="")


#####Recodage######
#Canton
D <- rename(D, Canton=Kanton)
#party
D <- D %>% mutate(party = case_when(Partei==3 ~ "Other",
                                    Partei==31 ~ "PDC",
                                    Partei==311 ~ "Other",
                                    Partei==32 ~ "FDP",
                                    Partei==33 ~ "PS",
                                    Partei==34 ~ "UDC",
                                    Partei==341 ~ "PDB",
                                    Partei==352 ~ "Other",
                                    Partei==355 ~ "Other",
                                    Partei==358 ~ "Other",
                                    Partei==354 ~ "Other",
                                    Partei==359 ~ "Verts",
                                    Partei==3591 ~ "Verts_lib",
                                    Partei==36 ~ "Other"))
#issue
D <- D %>% mutate(economy = case_when(Thema_WirtschaftFinanzen==1 ~ 1,
                                      Thema_WirtschaftFinanzen==0 ~ 0,
                                      Thema_Arbeit==1 ~ 1,
                                      Thema_Arbeit==0 ~ 0))
D <- D %>% mutate(environment = case_when(Thema_EnergieUmwelt==1 ~ 1,
                                          Thema_EnergieUmwelt==0 ~ 0,
                                          Thema_EnergieUmwelt_2==1 ~ 1,
                                          Thema_EnergieUmwelt_2==0 ~ 0))
D <- D %>% mutate(immigration = case_when(Thema_AsylMigration==1 ~ 1,
                                          Thema_AsylMigration==0 ~ 0))
D <- D %>% mutate(social = case_when(Thema_Sozial==1 ~ 1,
                                     Thema_Sozial==0 ~ 0))
D <- D %>% mutate(EU = case_when(Thema_EU==1 ~ 1,
                                 Thema_EU==0 ~ 0))
D <- D %>% mutate(Other = case_when(Thema_Verkehr==1 ~ 1,
                                    Thema_Verkehr==0 ~ 0))
#### enregistrer le dataset pour STATA - descriptive statistics----
#install.packages("xlsx")
#library(xlsx)
#write.xlsx(D,"C:/Users/borgeatq/Documents/R/first paper/ads_2019_descriptive2.xlsx", sheetName = "ads_2019_descriptive", 
           #col.names = TRUE, row.names = TRUE, append = FALSE)

#### ne garder que les pubs avec des themes ####
D <- D %>% filter(No_Thema==0)
###select les variables d'interet####
D <- select(D, c("Canton", "immigration", "economy", "environment", "social", "EU", "Other", "party"))

####pivot long####
X <- pivot_longer(D, -c("Canton", "party"), names_to = "issue", values_to = "number")

####grouper les observations par combinaison party-issue-canton####
X <- X %>% group_by(Canton, party, issue) %>% summarise(number = sum(number))

####drop observations cantons et party other####
X1 <- X[!(X$Canton=="CH"),]
X1 <- X1[!(X1$party=="Other"),]


#### creer une variable "issue emphasis des autres partis" - repris (10.11.20)#####
W <- read_dta("Wahlinserate_2019_allcantons.dta")

W$YYYYMMDD <- as.Date(as.character(W$YYYYMMDD), format='%Y%m%d')

W <- W %>% dplyr::filter(YYYYMMDD <= as.Date("2019-08-01"))

#Canton
W <- rename(W, Canton=Kanton)
#party
W <- W %>% mutate(party = case_when(Partei==3 ~ "Other",
                                    Partei==31 ~ "PDC",
                                    Partei==311 ~ "Other",
                                    Partei==32 ~ "FDP",
                                    Partei==33 ~ "PS",
                                    Partei==34 ~ "UDC",
                                    Partei==341 ~ "PDB",
                                    Partei==352 ~ "Other",
                                    Partei==355 ~ "Other",
                                    Partei==358 ~ "Other",
                                    Partei==354 ~ "Other",
                                    Partei==359 ~ "Verts",
                                    Partei==3591 ~ "Verts_lib",
                                    Partei==36 ~ "Other"))
#issue
W <- W %>% mutate(economy = case_when(Thema_WirtschaftFinanzen==1 ~ 1,
                                      Thema_WirtschaftFinanzen==0 ~ 0,
                                      Thema_Arbeit==1 ~ 1,
                                      Thema_Arbeit==0 ~ 0))
W <- W %>% mutate(environment = case_when(Thema_EnergieUmwelt==1 ~ 1,
                                          Thema_EnergieUmwelt==0 ~ 0,
                                          Thema_EnergieUmwelt_2==1 ~ 1,
                                          Thema_EnergieUmwelt_2==0 ~ 0))
W <- W %>% mutate(immigration = case_when(Thema_AsylMigration==1 ~ 1,
                                          Thema_AsylMigration==0 ~ 0))
W <- W %>% mutate(social = case_when(Thema_Sozial==1 ~ 1,
                                     Thema_Sozial==0 ~ 0))
W <- W %>% mutate(EU = case_when(Thema_EU==1 ~ 1,
                                 Thema_EU==0 ~ 0))
W <- W %>% mutate(Other = case_when(Thema_Verkehr==1 ~ 1,
                                    Thema_Verkehr==0 ~ 0))


W <- select(W, c("Canton", "immigration", "economy", "environment", "social", "EU", "Other", "party"))

V <- pivot_longer(W, -c("Canton", "party"), names_to = "issue", values_to = "number")

V <- V %>% group_by(Canton, party, issue) %>% summarise(number = sum(number))

V <- V[!(V$Canton=="CH"),]
V <- V[!(V$party=="Other"),]


###udc####
y <- V %>% group_by(issue, Canton) %>% filter(party!="UDC") %>%  summarise(number = sum(number))
y$party <- "UDC"

###plr####
y1 <- V %>% group_by(issue, Canton) %>% filter(party!="FDP") %>%  summarise(number = sum(number))
y1$party <- "FDP"

###ps####
y2 <- V %>% group_by(issue, Canton) %>% filter(party!="PS") %>%  summarise(number = sum(number))
y2$party <- "PS"

###pdr####
y3 <- V %>% group_by(issue, Canton) %>% filter(party!="PDC") %>%  summarise(number = sum(number))
y3$party <- "PDC"

###verts####
y4 <- V %>% group_by(issue, Canton) %>% filter(party!="Verts") %>%  summarise(number = sum(number))
y4$party <- "Verts"

###verts_lib####
y6 <- V %>% group_by(issue, Canton) %>% filter(party!="Verts_lib") %>%  summarise(number = sum(number))
y6$party <- "Verts_lib"

###regrouper les differents dataset####
Y <- rbind(y, y1, y2, y3, y4, y6)

Y <- rename(Y, number_other=number)

X1 <-left_join(X1, Y, by=c("Canton", "issue", "party"))

#### drop observations CH ----
X1 <- X1[!(X1$Canton=="CH"),]

### ajouter les observations manquantes ----
issue <- c("economy", "immigration", "environment", "social", "EU")
canton <- c("ZH", "BE", "LU", "UR", "SZ", "OW", "NW", 
            "GL", "ZG", "FR", "SO", "BS", "BL", "SH", "AR", "AI", "SG", "GR", "AG", "TG", "TI", "VD", "VS", "NE", "GE","JU")
party <- c("FDP", "PS", "PDC", "UDC", "Verts", "Verts_lib", "PDB")

data <- as.data.frame(list(Canton = rep(canton, 7*5),
                           party = rep(rep(party, each = 26), 5),
                           issue = rep(issue, each = 26*7)))


X1 <- merge(X1, data, by.x  = c("Canton", "party", "issue"), all.y = TRUE)

X1[is.na(X1)] <- 0

### drop les cantons ou les partis ne se sont pas presentes----
# 21 cantons 

# PLR - 4 cantons
X1 <- X1[!((X1$Canton=="AI" |
              X1$Canton=="GL" |
              X1$Canton=="UR" |
              X1$Canton=="NW") &
             X1$party=="FDP"),]

# PS - 2 cantons
X1 <- X1[!((X1$Canton=="AR" |
              X1$Canton=="NW") &
             X1$party=="PS"),]

# UDC - 1 cantons
X1 <- X1[!((X1$Canton=="GL") &
             X1$party=="UDC"),]

# PDC - 2 cantons
X1 <- X1[!((X1$Canton=="AR" |
              X1$Canton=="GL") &
             X1$party=="PDC"),]

# VERTS - 5 cantons
X1 <- X1[!((X1$Canton=="AI" |
              X1$Canton=="AR" |
              X1$Canton== "UR" |
              X1$Canton=="OW" |
              X1$Canton=="NW") &
             X1$party=="Verts"),]

# VERTS_lib - 7 cantons
X1 <- X1[!((X1$Canton=="AI" |
              X1$Canton=="AR" |
              X1$Canton=="GL" |
              X1$Canton=="JU" |
              X1$Canton=="UR" |
              X1$Canton=="OW" |
              X1$Canton=="NW") &
             X1$party=="Verts_lib"),]




#######################################################################################################################

### Voters data

### creer les variables de potential voters ----
S <- S %>% replace_with_na(replace = list(W1_f14400a = 99,
                                          W1_f14400b = 99,
                                          W1_f14400c = 99,
                                          W1_f14400d = 99,
                                          W1_f14400e = 99))

S$mean_prob <- ((S$W1_f14400a+S$W1_f14400b+S$W1_f14400c+S$W1_f14400d+S$W1_f14400e)/5)

S <- S %>% drop_na(mean_prob)

# FDP
S$prob_fdp <- (S$W1_f14400a-S$mean_prob)

S <- S %>% mutate(potential_fdp = case_when(S$W1_f10300main6==2 ~ 2,
                                            S$prob_fdp>0 ~ 1,
                                            S$prob_fdp<=0 ~ 0))

# CVP
S$prob_cvp <- (S$W1_f14400b-S$mean_prob)

S <- S %>% mutate(potential_cvp = case_when(S$W1_f10300main6==3 ~ 2,
                                            S$prob_cvp>0 ~ 1,
                                            S$prob_cvp<=0 ~ 0))

# SP
S$prob_sp <- (S$W1_f14400c-S$mean_prob)

S <- S %>% mutate(potential_sp = case_when(S$W1_f10300main6==5 ~ 2,
                                           S$prob_sp>0 ~ 1,
                                           S$prob_sp<=0 ~ 0))

# SVP
S$prob_svp <- (S$W1_f14400d-S$mean_prob)

S <- S %>% mutate(potential_svp = case_when(S$W1_f10300main6==1 ~ 2,
                                            S$prob_svp>0 ~ 1,
                                            S$prob_svp<=0 ~ 0))

# GPS
S$prob_gps <- (S$W1_f14400e-S$mean_prob)

S <- S %>% mutate(potential_gps = case_when(S$W1_f10300main6==6 ~ 2,
                                            S$prob_gps>0 ~ 1,
                                            S$prob_gps<=0 ~ 0))

# changer le nom des variables pour simplifier la suite
S$UDC_potential <- S$potential_svp
S$PLR_potential <- S$potential_fdp
S$PS_potential <- S$potential_sp
S$PDC_potential <- S$potential_cvp
S$VERTS_potential <- S$potential_gps

### recodage  ----
S <- S %>% mutate(canton = case_when(W1_f10000 ==0 ~ "CH",
                                     W1_f10000 ==1 ~ "ZH",
                                     W1_f10000 ==2 ~ "BE", 
                                     W1_f10000 ==3 ~ "LU",
                                     W1_f10000 ==4 ~ "UR",
                                     W1_f10000 ==5 ~ "SZ",
                                     W1_f10000 ==6 ~ "OW",
                                     W1_f10000 ==7 ~ "NW",
                                     W1_f10000 ==8 ~ "GL",
                                     W1_f10000 ==9 ~ "ZG",
                                     W1_f10000 ==10 ~ "FR",
                                     W1_f10000 ==11 ~ "SO",
                                     W1_f10000 ==12 ~ "BS",
                                     W1_f10000 ==13 ~ "BL",
                                     W1_f10000 ==14 ~ "SH",
                                     W1_f10000 ==15 ~ "AR",
                                     W1_f10000 ==16 ~ "AI",
                                     W1_f10000 ==17 ~ "SG",
                                     W1_f10000 ==18 ~ "GR",
                                     W1_f10000 ==19 ~ "AG",
                                     W1_f10000 ==20 ~ "TG",
                                     W1_f10000 ==21 ~ "TI",
                                     W1_f10000 ==22 ~ "VD",
                                     W1_f10000 ==23 ~ "VS",
                                     W1_f10000 ==24 ~ "NE",
                                     W1_f10000 ==25 ~ "GE",
                                     W1_f10000 ==26 ~ "JU"))


# essayer avec les proportions pour avoir plus de diffÃ©rence entre potential et voters 
### recodage ----
S <- S %>% mutate(issue_EU = case_when(W1_f15310a==1 ~ "Very important",
                                       W1_f15310a==2 ~ "Important",
                                       W1_f15310a==3 ~ "Rather important",
                                       W1_f15310a==4 ~ "Rather Not important"))
#immigration
S <- S %>% mutate(issue_immigration = case_when(W1_f15310b==1 ~ "Very important",
                                                W1_f15310b==2 ~ "Important",
                                                W1_f15310b==3 ~ "Rather important",
                                                W1_f15310b==4 ~ "Rather Not important"))
#social
S <- S %>% mutate(issue_social = case_when(W1_f15310c==1 ~ "Very important",
                                           W1_f15310c==2 ~ "Important",
                                           W1_f15310c==3 ~ "Rather important",
                                           W1_f15310c==4 ~ "Rather Not important"))
#Environment
S <- S %>% mutate(issue_environment = case_when(W1_f15310d==1 ~ "Very important",
                                                W1_f15310d==2 ~ "Important",
                                                W1_f15310d==3 ~ "Rather important",
                                                W1_f15310d==4 ~ "Rather Not important"))
#economy
S <- S %>% mutate(issue_economy = case_when(W1_f15310e==1 ~ "Very important",
                                            W1_f15310e==2 ~ "Important",
                                            W1_f15310e==3 ~ "Rather important",
                                            W1_f15310e==4 ~ "Rather Not important"))


S1 <- S
### POTENTIAL ----
###udc-immigration####
#pourcentage
u1 <- S1 %>%  group_by(canton, UDC_potential) %>%
  filter(!is.na(issue_immigration)) %>%
  count(issue_immigration, UDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
u1$party <- "UDC"
u1$issue <- "immigration"
### garder uniquement potential et very important par canton
u1 <- filter(u1, UDC_potential==1 & issue_immigration=="Very important")

####udc-EU#####
u2 <- S1 %>%  group_by(canton, UDC_potential) %>%
  filter(!is.na(issue_EU)) %>%
  count(issue_EU, UDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
u2$party <- "UDC"
u2$issue <- "EU"

### garder uniquement potential et very important par canton
u2 <- filter(u2, UDC_potential==1 & issue_EU=="Very important")
####udc-environment#####
u3 <- S1 %>%  group_by(canton, UDC_potential) %>%
  filter(!is.na(issue_environment)) %>%
  count(issue_environment, UDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
u3$party <- "UDC"

u3$issue <- "environment"

### garder uniquement potential et very important par canton
u3 <- filter(u3, UDC_potential==1 & issue_environment=="Very important")
####udc-economy#####
u4 <- S1 %>%  group_by(canton, UDC_potential) %>%
  filter(!is.na(issue_economy)) %>%
  count(issue_economy, UDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
u4$party <- "UDC"

u4$issue <- "economy"

### garder uniquement potential et very important par canton
u4 <- filter(u4, UDC_potential==1 & issue_economy=="Very important")
####udc-social#####
u5 <- S1 %>%  group_by(canton, UDC_potential) %>%
  filter(!is.na(issue_social)) %>%
  count(issue_social, UDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
u5$party <- "UDC"

u5$issue <- "social"

### garder uniquement potential et very important par canton
u5 <- filter(u5, UDC_potential==1 & issue_social=="Very important")

###plr-immigration####
#pourcentage
p1 <- S1 %>%  group_by(canton, PLR_potential) %>%
  filter(!is.na(issue_immigration)) %>%
  count(issue_immigration, PLR_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
p1$party <- "PLR"
p1$issue <- "immigration"

### garder uniquement potential et very important par canton
p1 <- filter(p1, PLR_potential==1 & issue_immigration=="Very important")
###plr-EU####
#pourcentage
p2 <- S1 %>%  group_by(canton, PLR_potential) %>%
  filter(!is.na(issue_EU)) %>%
  count(issue_EU, PLR_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
p2$party <- "PLR"

p2$issue <- "EU"

### garder uniquement potential et very important par canton
p2 <- filter(p2, PLR_potential==1 & issue_EU=="Very important")
###plr-environment####
#pourcentage
p3 <- S1 %>%  group_by(canton, PLR_potential) %>%
  filter(!is.na(issue_environment)) %>%
  count(issue_environment, PLR_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
p3$party <- "PLR"

p3$issue <- "environment"

### garder uniquement potential et very important par canton
p3 <- filter(p3, PLR_potential==1 & issue_environment=="Very important")
###plr-economy####
#pourcentage
p4 <- S1 %>%  group_by(canton, PLR_potential) %>%
  filter(!is.na(issue_economy)) %>%
  count(issue_economy, PLR_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
p4$party <- "PLR"

p4$issue <- "economy"

### garder uniquement potential et very important par canton
p4 <- filter(p4, PLR_potential==1 & issue_economy=="Very important")
###plr-social####
#pourcentage
p5 <- S1 %>%  group_by(canton, PLR_potential) %>%
  filter(!is.na(issue_social)) %>%
  count(issue_social, PLR_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
p5$party <- "PLR"

p5$issue <- "social"

### garder uniquement potential et very important par canton
p5 <- filter(p5, PLR_potential==1 & issue_social=="Very important")

###ps-immigration####
#pourcentage
s1 <- S1 %>%  group_by(canton, PS_potential) %>%
  filter(!is.na(issue_immigration)) %>%
  count(issue_immigration, PS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
s1$party <- "PS"

s1$issue <- "immigration"

### garder uniquement potential et very important par canton
s1 <- filter(s1, PS_potential==1 & issue_immigration=="Very important")
###ps-EU####
#pourcentage
s2 <- S1 %>%  group_by(canton, PS_potential) %>%
  filter(!is.na(issue_EU)) %>%
  count(issue_EU, PS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
s2$party <- "PS"

s2$issue <- "EU"

### garder uniquement potential et very important par canton
s2 <- filter(s2, PS_potential==1 & issue_EU=="Very important")
###ps-environment####
#pourcentage
s3 <- S1 %>%  group_by(canton, PS_potential) %>%
  filter(!is.na(issue_environment)) %>%
  count(issue_environment, PS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
s3$party <- "PS"

s3$issue <- "environment"

### garder uniquement potential et very important par canton
s3 <- filter(s3, PS_potential==1 & issue_environment=="Very important")
###ps-economy####
#pourcentage
s4 <- S1 %>%  group_by(canton, PS_potential) %>%
  filter(!is.na(issue_economy)) %>%
  count(issue_economy, PS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
s4$party <- "PS"

s4$issue <- "economy"

### garder uniquement potential et very important par canton
s4 <- filter(s4, PS_potential==1 & issue_economy=="Very important")
###ps-social####
#pourcentage
s5 <- S1 %>%  group_by(canton, PS_potential) %>%
  filter(!is.na(issue_social)) %>%
  count(issue_social, PS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
s5$party <- "PS"

s5$issue <- "social"

### garder uniquement potential et very important par canton
s5 <- filter(s5, PS_potential==1 & issue_social=="Very important")
###pdc-immigration####
#pourcentage
d1 <- S1 %>%  group_by(canton, PDC_potential) %>%
  filter(!is.na(issue_immigration)) %>%
  count(issue_immigration, PDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
d1$party <- "PDC"

d1$issue <- "immigration"

### garder uniquement potential et very important par canton
d1 <- filter(d1, PDC_potential==1 & issue_immigration=="Very important")
###pdc-EU####
#pourcentage
d2 <- S1 %>%  group_by(canton, PDC_potential) %>%
  filter(!is.na(issue_EU)) %>%
  count(issue_EU, PDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
d2$party <- "PDC"

d2$issue <- "EU"

### garder uniquement potential et very important par canton
d2 <- filter(d2, PDC_potential==1 & issue_EU=="Very important")
###pdc-environment####
#pourcentage
d3 <- S1 %>%  group_by(canton, PDC_potential) %>%
  filter(!is.na(issue_environment)) %>%
  count(issue_environment, PDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
d3$party <- "PDC"

d3$issue <- "environment"

### garder uniquement potential et very important par canton
d3 <- filter(d3, PDC_potential==1 & issue_environment=="Very important")
###pdc-economy####
#pourcentage
d4 <- S1 %>%  group_by(canton, PDC_potential) %>%
  filter(!is.na(issue_economy)) %>%
  count(issue_economy, PDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
d4$party <- "PDC"
d4$issue <- "economy"
### garder uniquement potential et very important par canton
d4 <- filter(d4, PDC_potential==1 & issue_economy=="Very important")
###pdc-social####
#pourcentage
d5 <- S1 %>%  group_by(canton, PDC_potential) %>%
  filter(!is.na(issue_social)) %>%
  count(issue_social, PDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
d5$party <- "PDC"
d5$issue <- "social"
### garder uniquement potential et very important par canton
d5 <- filter(d5, PDC_potential==1 & issue_social=="Very important")
###verts-immigration####
#pourcentage
v1 <- S1 %>%  group_by(canton, VERTS_potential) %>%
  filter(!is.na(issue_immigration)) %>%
  count(issue_immigration, VERTS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
v1$party <- "Verts"
v1$issue <- "immigration"
### garder uniquement potential et very important par canton
v1 <- filter(v1, VERTS_potential==1 & issue_immigration=="Very important")
###verts-EU####
#pourcentage
v2 <- S1 %>%  group_by(canton, VERTS_potential) %>%
  filter(!is.na(issue_EU)) %>%
  count(issue_EU, VERTS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
v2$party <- "Verts"
v2$issue <- "EU"
### garder uniquement potential et very important par canton
v2 <- filter(v2, VERTS_potential==1 & issue_EU=="Very important")
###verts-enviroment####
#pourcentage
v3 <- S1 %>%  group_by(canton, VERTS_potential) %>%
  filter(!is.na(issue_environment)) %>%
  count(issue_environment, VERTS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
v3$party <- "Verts"
v3$issue <- "environment"
### garder uniquement potential et very important par canton
v3 <- filter(v3, VERTS_potential==1 & issue_environment=="Very important")
###verts-economy####
#pourcentage
v4 <- S1 %>%  group_by(canton, VERTS_potential) %>%
  filter(!is.na(issue_economy)) %>%
  count(issue_economy, VERTS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
v4$party <- "Verts"
v4$issue <- "economy"
### garder uniquement potential et very important par canton
v4 <- filter(v4, VERTS_potential==1 & issue_economy=="Very important")
###verts-social####
#pourcentage
v5 <- S1 %>%  group_by(canton, VERTS_potential) %>%
  filter(!is.na(issue_social)) %>%
  count(issue_social, VERTS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
v5$party <- "Verts"
v5$issue <- "social"
### garder uniquement potential et very important par canton
v5 <- filter(v5, VERTS_potential==1 & issue_social=="Very important")



###regrouper les datasets des parti-enjeux#####
potential <- rbind(u1, u2, u3, u4, u5, p1, p2, p3, p4, p5, s1, s2, s3, s4, s5, 
                   d1, d2, d3, d4, d5, v1, v2, v3, v4, v5)

potential <- rename(potential, Canton=canton)
potential <- potential[!is.na(potential$Canton),]
potential <- rename(potential, N.p=n)
potential <- select(potential, Canton, prop, party, issue, N.p)
potential$party <- recode(potential$party, PLR="FDP")


final <-left_join(X1, potential, by=c("Canton", "issue", "party"))
final <- select(final, Canton, party, issue, number, number_other, prop, N.p)

### VOTERS ----

S <- S %>% replace_with_na(replace = list(W1_f10300main6 = 9))

S <- S %>% mutate(vote_udc = case_when(S$W1_f10300main6==1 ~ 1,
                                       S$W1_f10300main6==2 ~ 0,
                                       S$W1_f10300main6==3 ~ 0,
                                       S$W1_f10300main6==4 ~ 0,
                                       S$W1_f10300main6==5 ~ 0,
                                       S$W1_f10300main6==6 ~ 0,
                                       S$W1_f10300main6==7 ~ 0))

S <- S %>% mutate(vote_plr = case_when(S$W1_f10300main6==1 ~ 0,
                                       S$W1_f10300main6==2 ~ 1,
                                       S$W1_f10300main6==3 ~ 0,
                                       S$W1_f10300main6==4 ~ 0,
                                       S$W1_f10300main6==5 ~ 0,
                                       S$W1_f10300main6==6 ~ 0,
                                       S$W1_f10300main6==7 ~ 0))

S <- S %>% mutate(vote_pdc = case_when(S$W1_f10300main6==1 ~ 0,
                                       S$W1_f10300main6==2 ~ 0,
                                       S$W1_f10300main6==3 ~ 1,
                                       S$W1_f10300main6==4 ~ 0,
                                       S$W1_f10300main6==5 ~ 0,
                                       S$W1_f10300main6==6 ~ 0,
                                       S$W1_f10300main6==7 ~ 0))

S <- S %>% mutate(vote_ps = case_when(S$W1_f10300main6==1 ~ 0,
                                       S$W1_f10300main6==2 ~ 0,
                                       S$W1_f10300main6==3 ~ 0,
                                       S$W1_f10300main6==4 ~ 0,
                                       S$W1_f10300main6==5 ~ 1,
                                       S$W1_f10300main6==6 ~ 0,
                                       S$W1_f10300main6==7 ~ 0))

S <- S %>% mutate(vote_verts = case_when(S$W1_f10300main6==1 ~ 0,
                                       S$W1_f10300main6==2 ~ 0,
                                       S$W1_f10300main6==3 ~ 0,
                                       S$W1_f10300main6==4 ~ 0,
                                       S$W1_f10300main6==5 ~ 0,
                                       S$W1_f10300main6==6 ~ 1,
                                       S$W1_f10300main6==7 ~ 0))

###udc-immigration####
#pourcentage
u1 <- S1 %>%  group_by(canton, UDC_potential) %>%
  filter(!is.na(issue_immigration)) %>%
  count(issue_immigration, UDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
u1$party <- "UDC"
u1$issue <- "immigration"
### garder uniquement potential et very important par canton
u1 <- filter(u1, UDC_potential==2 & issue_immigration=="Very important")

####udc-EU#####
u2 <- S1 %>%  group_by(canton, UDC_potential) %>%
  filter(!is.na(issue_EU)) %>%
  count(issue_EU, UDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
u2$party <- "UDC"
u2$issue <- "EU"

### garder uniquement potential et very important par canton
u2 <- filter(u2, UDC_potential==2 & issue_EU=="Very important")
####udc-environment#####
u3 <- S1 %>%  group_by(canton, UDC_potential) %>%
  filter(!is.na(issue_environment)) %>%
  count(issue_environment, UDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
u3$party <- "UDC"

u3$issue <- "environment"

### garder uniquement potential et very important par canton
u3 <- filter(u3, UDC_potential==2 & issue_environment=="Very important")
####udc-economy#####
u4 <- S1 %>%  group_by(canton, UDC_potential) %>%
  filter(!is.na(issue_economy)) %>%
  count(issue_economy, UDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
u4$party <- "UDC"

u4$issue <- "economy"

### garder uniquement potential et very important par canton
u4 <- filter(u4, UDC_potential==2 & issue_economy=="Very important")
####udc-social#####
u5 <- S1 %>%  group_by(canton, UDC_potential) %>%
  filter(!is.na(issue_social)) %>%
  count(issue_social, UDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
u5$party <- "UDC"

u5$issue <- "social"

### garder uniquement potential et very important par canton
u5 <- filter(u5, UDC_potential==2 & issue_social=="Very important")

###plr-immigration####
#pourcentage
p1 <- S1 %>%  group_by(canton, PLR_potential) %>%
  filter(!is.na(issue_immigration)) %>%
  count(issue_immigration, PLR_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
p1$party <- "PLR"

p1$issue <- "immigration"

### garder uniquement potential et very important par canton
p1 <- filter(p1, PLR_potential==2 & issue_immigration=="Very important")
###plr-EU####
#pourcentage
p2 <- S1 %>%  group_by(canton, PLR_potential) %>%
  filter(!is.na(issue_EU)) %>%
  count(issue_EU, PLR_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
p2$party <- "PLR"

p2$issue <- "EU"

### garder uniquement potential et very important par canton
p2 <- filter(p2, PLR_potential==2 & issue_EU=="Very important")
###plr-environment####
#pourcentage
p3 <- S1 %>%  group_by(canton, PLR_potential) %>%
  filter(!is.na(issue_environment)) %>%
  count(issue_environment, PLR_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
p3$party <- "PLR"

p3$issue <- "environment"

### garder uniquement potential et very important par canton
p3 <- filter(p3, PLR_potential==2 & issue_environment=="Very important")
###plr-economy####
#pourcentage
p4 <- S1 %>%  group_by(canton, PLR_potential) %>%
  filter(!is.na(issue_economy)) %>%
  count(issue_economy, PLR_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
p4$party <- "PLR"

p4$issue <- "economy"

### garder uniquement potential et very important par canton
p4 <- filter(p4, PLR_potential==2 & issue_economy=="Very important")
###plr-social####
#pourcentage
p5 <- S1 %>%  group_by(canton, PLR_potential) %>%
  filter(!is.na(issue_social)) %>%
  count(issue_social, PLR_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
p5$party <- "PLR"

p5$issue <- "social"

### garder uniquement potential et very important par canton
p5 <- filter(p5, PLR_potential==2 & issue_social=="Very important")

###ps-immigration####
#pourcentage
s1 <- S1 %>%  group_by(canton, PS_potential) %>%
  filter(!is.na(issue_immigration)) %>%
  count(issue_immigration, PS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
s1$party <- "PS"

s1$issue <- "immigration"

### garder uniquement potential et very important par canton
s1 <- filter(s1, PS_potential==2 & issue_immigration=="Very important")
###ps-EU####
#pourcentage
s2 <- S1 %>%  group_by(canton, PS_potential) %>%
  filter(!is.na(issue_EU)) %>%
  count(issue_EU, PS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
s2$party <- "PS"

s2$issue <- "EU"

### garder uniquement potential et very important par canton
s2 <- filter(s2, PS_potential==2 & issue_EU=="Very important")
###ps-environment####
#pourcentage
s3 <- S1 %>%  group_by(canton, PS_potential) %>%
  filter(!is.na(issue_environment)) %>%
  count(issue_environment, PS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
s3$party <- "PS"

s3$issue <- "environment"

### garder uniquement potential et very important par canton
s3 <- filter(s3, PS_potential==2 & issue_environment=="Very important")
###ps-economy####
#pourcentage
s4 <- S1 %>%  group_by(canton, PS_potential) %>%
  filter(!is.na(issue_economy)) %>%
  count(issue_economy, PS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
s4$party <- "PS"

s4$issue <- "economy"

### garder uniquement potential et very important par canton
s4 <- filter(s4, PS_potential==2 & issue_economy=="Very important")
###ps-social####
#pourcentage
s5 <- S1 %>%  group_by(canton, PS_potential) %>%
  filter(!is.na(issue_social)) %>%
  count(issue_social, PS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
s5$party <- "PS"

s5$issue <- "social"

### garder uniquement potential et very important par canton
s5 <- filter(s5, PS_potential==2 & issue_social=="Very important")
###pdc-immigration####
#pourcentage
d1 <- S1 %>%  group_by(canton, PDC_potential) %>%
  filter(!is.na(issue_immigration)) %>%
  count(issue_immigration, PDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
d1$party <- "PDC"

d1$issue <- "immigration"

### garder uniquement potential et very important par canton
d1 <- filter(d1, PDC_potential==2 & issue_immigration=="Very important")
###pdc-EU####
#pourcentage
d2 <- S1 %>%  group_by(canton, PDC_potential) %>%
  filter(!is.na(issue_EU)) %>%
  count(issue_EU, PDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
d2$party <- "PDC"

d2$issue <- "EU"

### garder uniquement potential et very important par canton
d2 <- filter(d2, PDC_potential==2 & issue_EU=="Very important")
###pdc-environment####
#pourcentage
d3 <- S1 %>%  group_by(canton, PDC_potential) %>%
  filter(!is.na(issue_environment)) %>%
  count(issue_environment, PDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
d3$party <- "PDC"

d3$issue <- "environment"

### garder uniquement potential et very important par canton
d3 <- filter(d3, PDC_potential==2 & issue_environment=="Very important")
###pdc-economy####
#pourcentage
d4 <- S1 %>%  group_by(canton, PDC_potential) %>%
  filter(!is.na(issue_economy)) %>%
  count(issue_economy, PDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
d4$party <- "PDC"
d4$issue <- "economy"
### garder uniquement potential et very important par canton
d4 <- filter(d4, PDC_potential==2 & issue_economy=="Very important")
###pdc-social####
#pourcentage
d5 <- S1 %>%  group_by(canton, PDC_potential) %>%
  filter(!is.na(issue_social)) %>%
  count(issue_social, PDC_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
d5$party <- "PDC"
d5$issue <- "social"
### garder uniquement potential et very important par canton
d5 <- filter(d5, PDC_potential==2 & issue_social=="Very important")
###verts-immigration####
#pourcentage
v1 <- S1 %>%  group_by(canton, VERTS_potential) %>%
  filter(!is.na(issue_immigration)) %>%
  count(issue_immigration, VERTS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
v1$party <- "Verts"
v1$issue <- "immigration"
### garder uniquement potential et very important par canton
v1 <- filter(v1, VERTS_potential==2 & issue_immigration=="Very important")
###verts-EU####
#pourcentage
v2 <- S1 %>%  group_by(canton, VERTS_potential) %>%
  filter(!is.na(issue_EU)) %>%
  count(issue_EU, VERTS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
v2$party <- "Verts"
v2$issue <- "EU"
### garder uniquement potential et very important par canton
v2 <- filter(v2, VERTS_potential==2 & issue_EU=="Very important")
###verts-enviroment####
#pourcentage
v3 <- S1 %>%  group_by(canton, VERTS_potential) %>%
  filter(!is.na(issue_environment)) %>%
  count(issue_environment, VERTS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
v3$party <- "Verts"
v3$issue <- "environment"
### garder uniquement potential et very important par canton
v3 <- filter(v3, VERTS_potential==2 & issue_environment=="Very important")
###verts-economy####
#pourcentage
v4 <- S1 %>%  group_by(canton, VERTS_potential) %>%
  filter(!is.na(issue_economy)) %>%
  count(issue_economy, VERTS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
v4$party <- "Verts"
v4$issue <- "economy"
### garder uniquement potential et very important par canton
v4 <- filter(v4, VERTS_potential==2 & issue_economy=="Very important")
###verts-social####
#pourcentage
v5 <- S1 %>%  group_by(canton, VERTS_potential) %>%
  filter(!is.na(issue_social)) %>%
  count(issue_social, VERTS_potential) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
v5$party <- "Verts"
v5$issue <- "social"
### garder uniquement potential et very important par canton
v5 <- filter(v5, VERTS_potential==2 & issue_social=="Very important")


###regrouper les datasets des parti-enjeux#####
voters <- rbind(u1, u2, u3, u4, u5, p1, p2, p3, p4, p5, s1, s2, s3, s4, s5, 
                d1, d2, d3, d4, d5, v1, v2, v3, v4, v5)

voters <- rename(voters, Canton=canton)
voters <- voters[!is.na(voters$Canton),]
voters <- rename(voters, N.v=n)
voters <- select(voters, Canton, prop, party, issue, N.v)
voters$party <- recode(voters$party, PLR="FDP")

final <-left_join(final, voters, by=c("Canton", "issue", "party"))

### PUBLIC_2 ####
###immigration####
#pourcentage
I <- S %>%  group_by(canton) %>%
  filter(!is.na(issue_immigration)) %>%
  count(issue_immigration) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
I$issue <- "immigration"
### garder uniquement potential et very important par canton
I <- filter(I, issue_immigration=="Very important")

###EU####
#pourcentage
EU <- S %>%  group_by(canton) %>%
  filter(!is.na(issue_EU)) %>%
  count(issue_EU) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
EU$issue <- "EU"
### garder uniquement potential et very important par canton
EU <- filter(EU, issue_EU=="Very important")

###social####
SO <- S %>%  group_by(canton) %>%
  filter(!is.na(issue_social)) %>%
  count(issue_social) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
SO$issue <- "social"
### garder uniquement potential et very important par canton
SO <- filter(SO, issue_social=="Very important")

###economy####
eco <- S %>%  group_by(canton) %>%
  filter(!is.na(issue_economy)) %>%
  count(issue_economy) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
eco$issue <- "economy"
### garder uniquement potential et very important par canton
eco <- filter(eco, issue_economy=="Very important")

###environment####
ev <- S %>%  group_by(canton) %>%
  filter(!is.na(issue_environment)) %>%
  count(issue_environment) %>%
  mutate(prop = prop.table(n))
### crÃÂ©er variable party et enjeu
ev$issue <- "environment"
### garder uniquement potential et very important par canton
ev <- filter(ev, issue_environment=="Very important")


###combiner saillances generales####
i_i <- rbind(I, EU, SO, eco, ev)

#Canton
i_i <- rename(i_i, Canton=canton)
i_i <- rename(i_i, public_2=prop)
final <-left_join(final, i_i, by=c("Canton", "issue"))


### cleaning ----

final <- select(final, Canton, party, issue, number, number_other, prop.x, N.p, prop.y, N.v, public_2)

final <- rename(final, potential=prop.x)
final <- rename(final, voters=prop.y)


final <- final[!(final$party=="PDB"),]


### variables de controle ----
### party size ----
# https://www.bfs.admin.ch/bfs/fr/home/statistiques/politique/elections/conseil-national/force-partis.html#-1923937103
final <- final %>% mutate(party_size=case_when(party=="FDP" & Canton=="ZH" ~ 15.3,
                                               party=="FDP" & Canton=="BE" ~ 9.3,
                                               party=="FDP" & Canton=="LU" ~ 18.5,
                                               party=="FDP" & Canton=="UR" ~ 0,
                                               party=="FDP" & Canton=="SZ" ~ 20.6,
                                               party=="FDP" & Canton=="OW" ~ 0,
                                               party=="FDP" & Canton=="NW" ~ 0,
                                               party=="FDP" & Canton=="GL" ~ 0,
                                               party=="FDP" & Canton=="ZG" ~ 17.6,
                                               party=="FDP" & Canton=="FR" ~ 14.2,
                                               party=="FDP" & Canton=="SO" ~ 21.2,
                                               party=="FDP" & Canton=="BS" ~ 9.8,
                                               party=="FDP" & Canton=="BL" ~ 15.8,
                                               party=="FDP" & Canton=="SH" ~ 12.9,
                                               party=="FDP" & Canton=="AR" ~ 33.6,
                                               party=="FDP" & Canton=="AI" ~ 76.3,
                                               party=="FDP" & Canton=="SG" ~ 14.3,
                                               party=="FDP" & Canton=="GR" ~ 13.3,
                                               party=="FDP" & Canton=="AG" ~ 15.1,
                                               party=="FDP" & Canton=="TG" ~ 13.0,
                                               party=="FDP" & Canton=="TI" ~ 23.7,
                                               party=="FDP" & Canton=="VD" ~ 26.8,
                                               party=="FDP" & Canton=="VS" ~ 18.1,
                                               party=="FDP" & Canton=="NE" ~ 24.4,
                                               party=="FDP" & Canton=="GE" ~ 20.5,
                                               party=="FDP" & Canton=="JU" ~ 16.8,
                                               party=="PDC" & Canton=="ZH" ~ 4.2,
                                               party=="PDC" & Canton=="BE" ~ 1.8,
                                               party=="PDC" & Canton=="LU" ~ 23.9,
                                               party=="PDC" & Canton=="UR" ~ 26.8,
                                               party=="PDC" & Canton=="SZ" ~ 19.5,
                                               party=="PDC" & Canton=="OW" ~ 0,
                                               party=="PDC" & Canton=="NW" ~ 0,
                                               party=="PDC" & Canton=="GL" ~ 0,
                                               party=="PDC" & Canton=="ZG" ~ 26.4,
                                               party=="PDC" & Canton=="FR" ~ 22.7,
                                               party=="PDC" & Canton=="SO" ~ 14.8,
                                               party=="PDC" & Canton=="BS" ~ 6.4,
                                               party=="PDC" & Canton=="BL" ~ 9.1,
                                               party=="PDC" & Canton=="SH" ~ 0,
                                               party=="PDC" & Canton=="AR" ~ 0,
                                               party=="PDC" & Canton=="AI" ~ 76.3,
                                               party=="PDC" & Canton=="SG" ~ 16.6,
                                               party=="PDC" & Canton=="GR" ~ 16.8,
                                               party=="PDC" & Canton=="AG" ~ 8.6,
                                               party=="PDC" & Canton=="TG" ~ 13.1,
                                               party=="PDC" & Canton=="TI" ~ 20.1,
                                               party=="PDC" & Canton=="VD" ~ 4.1,
                                               party=="PDC" & Canton=="VS" ~ 39.8,
                                               party=="PDC" & Canton=="NE" ~ 3.6,
                                               party=="PDC" & Canton=="GE" ~ 12.1,
                                               party=="PDC" & Canton=="JU" ~ 27.6,
                                               party=="PS" & Canton=="ZH" ~ 21.4,
                                               party=="PS" & Canton=="BE" ~ 19.7,
                                               party=="PS" & Canton=="LU" ~ 13.6,
                                               party=="PS" & Canton=="UR" ~ 0,
                                               party=="PS" & Canton=="SZ" ~ 13.1,
                                               party=="PS" & Canton=="OW" ~ 0,
                                               party=="PS" & Canton=="NW" ~ 0,
                                               party=="PS" & Canton=="GL" ~ 45.0,
                                               party=="PS" & Canton=="ZG" ~ 13.8,
                                               party=="PS" & Canton=="FR" ~ 24.2,
                                               party=="PS" & Canton=="SO" ~ 20.0,
                                               party=="PS" & Canton=="BS" ~ 33.3,
                                               party=="PS" & Canton=="BL" ~ 22.2,
                                               party=="PS" & Canton=="SH" ~ 28.8,
                                               party=="PS" & Canton=="AR" ~ 28.6,
                                               party=="PS" & Canton=="AI" ~ 18.1,
                                               party=="PS" & Canton=="SG" ~ 14.2,
                                               party=="PS" & Canton=="GR" ~ 17.6,
                                               party=="PS" & Canton=="AG" ~ 16.1,
                                               party=="PS" & Canton=="TG" ~ 12.7,
                                               party=="PS" & Canton=="TI" ~ 15.9,
                                               party=="PS" & Canton=="VD" ~ 22.2,
                                               party=="PS" & Canton=="VS" ~ 13.3,
                                               party=="PS" & Canton=="NE" ~ 23.7,
                                               party=="PS" & Canton=="GE" ~ 19.9,
                                               party=="PS" & Canton=="JU" ~ 23.7,
                                               party=="UDC" & Canton=="ZH" ~ 30.7,
                                               party=="UDC" & Canton=="BE" ~ 33.1,
                                               party=="UDC" & Canton=="LU" ~ 28.5,
                                               party=="UDC" & Canton=="UR" ~ 44.1,
                                               party=="UDC" & Canton=="SZ" ~ 42.6,
                                               party=="UDC" & Canton=="OW" ~ 34.5,
                                               party=="UDC" & Canton=="NW" ~ 82.8,
                                               party=="UDC" & Canton=="GL" ~ 0,
                                               party=="UDC" & Canton=="ZG" ~ 30.5,
                                               party=="UDC" & Canton=="FR" ~ 25.9,
                                               party=="UDC" & Canton=="SO" ~ 28.8,
                                               party=="UDC" & Canton=="BS" ~ 17.6,
                                               party=="UDC" & Canton=="BL" ~ 29.8,
                                               party=="UDC" & Canton=="SH" ~ 45.3,
                                               party=="UDC" & Canton=="AR" ~ 36.1,
                                               party=="UDC" & Canton=="AI" ~ 0,
                                               party=="UDC" & Canton=="SG" ~ 35.8,
                                               party=="UDC" & Canton=="GR" ~ 29.7,
                                               party=="UDC" & Canton=="AG" ~ 38.0,
                                               party=="UDC" & Canton=="TG" ~ 39.9,
                                               party=="UDC" & Canton=="TI" ~ 11.3,
                                               party=="UDC" & Canton=="VD" ~ 22.6,
                                               party=="UDC" & Canton=="VS" ~ 22.1,
                                               party=="UDC" & Canton=="NE" ~ 20.4,
                                               party=="UDC" & Canton=="GE" ~ 17.6,
                                               party=="UDC" & Canton=="JU" ~ 12.8,
                                               party=="Verts" & Canton=="ZH" ~ 6.9,
                                               party=="Verts" & Canton=="BE" ~ 8.5,
                                               party=="Verts" & Canton=="LU" ~ 7.1,
                                               party=="Verts" & Canton=="UR" ~ 26.3,
                                               party=="Verts" & Canton=="SZ" ~ 1.4,
                                               party=="Verts" & Canton=="OW" ~ 0,
                                               party=="Verts" & Canton=="NW" ~ 0,
                                               party=="Verts" & Canton=="GL" ~ 0,
                                               party=="Verts" & Canton=="ZG" ~ 7.2,
                                               party=="Verts" & Canton=="FR" ~ 5.3,
                                               party=="Verts" & Canton=="SO" ~ 5.6,
                                               party=="Verts" & Canton=="BS" ~ 11.6,
                                               party=="Verts" & Canton=="BL" ~ 14.2,
                                               party=="Verts" & Canton=="SH" ~ 3.4,
                                               party=="Verts" & Canton=="AR" ~ 0,
                                               party=="Verts" & Canton=="AI" ~ 0,
                                               party=="Verts" & Canton=="SG" ~ 5.7,
                                               party=="Verts" & Canton=="GR" ~ 0,
                                               party=="Verts" & Canton=="AG" ~ 5.5,
                                               party=="Verts" & Canton=="TG" ~ 5.4,
                                               party=="Verts" & Canton=="TI" ~ 3.5,
                                               party=="Verts" & Canton=="VD" ~ 11.3,
                                               party=="Verts" & Canton=="VS" ~ 4.9,
                                               party=="Verts" & Canton=="NE" ~ 9.3,
                                               party=="Verts" & Canton=="GE" ~ 11.5,
                                               party=="Verts" & Canton=="JU" ~ 7.3,
                                               party=="Verts_lib" & Canton=="ZH" ~ 8.2,
                                               party=="Verts_lib" & Canton=="BE" ~ 6.0,
                                               party=="Verts_lib" & Canton=="LU" ~ 5.8,
                                               party=="Verts_lib" & Canton=="UR" ~ 0,
                                               party=="Verts_lib" & Canton=="SZ" ~ 2.8,
                                               party=="Verts_lib" & Canton=="OW" ~ 0,
                                               party=="Verts_lib" & Canton=="NW" ~ 0,
                                               party=="Verts_lib" & Canton=="GL" ~ 0,
                                               party=="Verts_lib" & Canton=="ZG" ~ 3.6,
                                               party=="Verts_lib" & Canton=="FR" ~ 3.2,
                                               party=="Verts_lib" & Canton=="SO" ~ 3.5,
                                               party=="Verts_lib" & Canton=="BS" ~ 4.8,
                                               party=="Verts_lib" & Canton=="BL" ~ 2.7,
                                               party=="Verts_lib" & Canton=="SH" ~ 0,
                                               party=="Verts_lib" & Canton=="AR" ~ 0,
                                               party=="Verts_lib" & Canton=="AI" ~ 0,
                                               party=="Verts_lib" & Canton=="SG" ~ 4.9,
                                               party=="Verts_lib" & Canton=="GR" ~ 7.9,
                                               party=="Verts_lib" & Canton=="AG" ~ 5.2,
                                               party=="Verts_lib" & Canton=="TG" ~ 6.2,
                                               party=="Verts_lib" & Canton=="TI" ~ 0.8,
                                               party=="Verts_lib" & Canton=="VD" ~ 3.9,
                                               party=="Verts_lib" & Canton=="VS" ~ 0,
                                               party=="Verts_lib" & Canton=="NE" ~ 3.4,
                                               party=="Verts_lib" & Canton=="GE" ~ 2.3,
                                               party=="Verts_lib" & Canton=="JU" ~ 0,
                                               party=="PDB" & Canton=="ZH" ~ 3.6,
                                               party=="PDB" & Canton=="BE" ~ 11.8,
                                               party=="PDB" & Canton=="LU" ~ 1.4,
                                               party=="PDB" & Canton=="UR" ~ 0,
                                               party=="PDB" & Canton=="SZ" ~ 0,
                                               party=="PDB" & Canton=="OW" ~ 0,
                                               party=="PDB" & Canton=="NW" ~ 0,
                                               party=="PDB" & Canton=="GL" ~ 51.5,
                                               party=="PDB" & Canton=="ZG" ~ 0,
                                               party=="PDB" & Canton=="FR" ~ 1.3,
                                               party=="PDB" & Canton=="SO" ~ 3.4,
                                               party=="PDB" & Canton=="BS" ~ 1.1,
                                               party=="PDB" & Canton=="BL" ~ 2.8,
                                               party=="PDB" & Canton=="SH" ~ 0,
                                               party=="PDB" & Canton=="AR" ~ 0,
                                               party=="PDB" & Canton=="AI" ~ 0,
                                               party=="PDB" & Canton=="SG" ~ 3.6,
                                               party=="PDB" & Canton=="GR" ~ 14.5,
                                               party=="PDB" & Canton=="AG" ~ 5.1,
                                               party=="PDB" & Canton=="TG" ~ 3.8,
                                               party=="PDB" & Canton=="TI" ~ 0,
                                               party=="PDB" & Canton=="VD" ~ 1.8,
                                               party=="PDB" & Canton=="VS" ~ 0,
                                               party=="PDB" & Canton=="NE" ~ 1.0,
                                               party=="PDB" & Canton=="GE" ~ 1.0,
                                               party=="PDB" & Canton=="JU" ~ 0))

### government status ----
final <- final %>% mutate(gov_status=case_when(party=="FDP" ~ 1,
                                               party=="PS" ~ 1,
                                               party=="UDC" ~ 1,
                                               party=="PDC" ~ 1,
                                               party=="Verts" ~ 0,
                                               party=="Verts_lib" ~ 0,
                                               party=="PDB" ~ 0))

### issue ownership ----
final <- final %>% mutate(ownership = case_when(party=="FDP" & issue=="economy" ~ 1,
                                                party=="UDC" & issue=="immigration" ~ 1, 
                                                party=="PS" & issue=="social" ~ 1,
                                                party=="Verts" & issue=="environment" ~ 1,
                                                party=="FDP" & issue=="EU" ~ 1,
                                                TRUE ~ 0))

### position ----
S$f15201 <- S$W1_f15200

# udc
#potential
udc <- S %>% group_by(canton, UDC_potential) %>% filter(!is.na(f15201)) %>% filter(!(f15201==98)) %>% summarise_at(vars(f15201), mean)
udc <- filter(udc, UDC_potential==1)
udc <- rename(udc, position_potential=f15201)
udc$party <- "UDC"
#voters
udc_voter <- S %>% group_by(canton, vote_udc) %>% filter(!is.na(f15201)) %>% filter(!(f15201==98)) %>% summarise_at(vars(f15201), mean)
udc_voter <- filter(udc_voter, vote_udc==1)
udc_voter <- rename(udc_voter, position_voter=f15201)
udc_voter$party <- "UDC"
# fdp
#potential
fdp <- S %>% group_by(canton, PLR_potential) %>% filter(!is.na(f15201)) %>% filter(!(f15201==98)) %>% summarise_at(vars(f15201), mean)
fdp <- filter(fdp, PLR_potential==1)
fdp <- rename(fdp, position_potential=f15201)
fdp$party <- "FDP"
#voters
fdp_voter <- S %>% group_by(canton, vote_plr) %>% filter(!is.na(f15201)) %>% filter(!(f15201==98)) %>% summarise_at(vars(f15201), mean)
fdp_voter <- filter(fdp_voter, vote_plr==1)
fdp_voter <- rename(fdp_voter, position_voter=f15201)
fdp_voter$party <- "FDP"
# PS
#potential
ps <- S %>% group_by(canton, PS_potential) %>% filter(!is.na(f15201)) %>% filter(!(f15201==98)) %>% summarise_at(vars(f15201), mean)
ps <- filter(ps, PS_potential==1)
ps <- rename(ps, position_potential=f15201)
ps$party <- "PS"
#voters
ps_voter <- S %>% group_by(canton, vote_ps) %>% filter(!is.na(f15201)) %>% filter(!(f15201==98)) %>% summarise_at(vars(f15201), mean)
ps_voter <- filter(ps_voter, vote_ps==1)
ps_voter <- rename(ps_voter, position_voter=f15201)
ps_voter$party <- "PS"
# PDC
#potential
pdc <- S %>% group_by(canton, PDC_potential) %>% filter(!is.na(f15201)) %>% filter(!(f15201==98)) %>% summarise_at(vars(f15201), mean)
pdc <- filter(pdc, PDC_potential==1)
pdc <- rename(pdc, position_potential=f15201)
pdc$party <- "PDC"
#voters
pdc_voter <- S %>% group_by(canton, vote_pdc) %>% filter(!is.na(f15201)) %>% filter(!(f15201==98)) %>% summarise_at(vars(f15201), mean)
pdc_voter <- filter(pdc_voter, vote_pdc==1)
pdc_voter <- rename(pdc_voter, position_voter=f15201)
pdc_voter$party <- "PDC"
# VERTS
#potential
v <- S %>% group_by(canton, VERTS_potential) %>% filter(!is.na(f15201)) %>% filter(!(f15201==98)) %>% summarise_at(vars(f15201), mean)
v <- filter(v, VERTS_potential==1)
v <- rename(v, position_potential=f15201)
v$party <- "Verts"
#voters
v_voter <- S %>% group_by(canton, vote_verts) %>% filter(!is.na(f15201)) %>% filter(!(f15201==98)) %>% summarise_at(vars(f15201), mean)
v_voter <- filter(v_voter, vote_verts==1)
v_voter <- rename(v_voter, position_voter=f15201)
v_voter$party <- "Verts"


# bind potential
position_potential <- rbind(udc, fdp, ps, pdc, v)

#Canton
position_potential <- rename(position_potential, Canton=canton)

final <-left_join(final, position_potential, by=c("Canton", "party"))

# bind voter
position_voter <- rbind(udc_voter, fdp_voter, ps_voter, pdc_voter, v_voter)

#Canton

position_voter <- rename(position_voter, Canton=canton)

final <-left_join(final, position_voter, by=c("Canton", "party"))

### district magnitude ----
final <- final %>% mutate(district=case_when(Canton=="ZH" ~ 35,
                                             Canton=="BE" ~ 24,
                                             Canton=="LU" ~ 9,
                                             Canton=="UR" ~ 1,
                                             Canton=="SZ" ~ 4,
                                             Canton=="OW" ~ 1,
                                             Canton=="NW" ~ 1,
                                             Canton=="GL" ~ 1,
                                             Canton=="ZG" ~ 3,
                                             Canton=="FR" ~ 7,
                                             Canton=="SO" ~ 6,
                                             Canton=="BS" ~ 5,
                                             Canton=="BL" ~ 7,
                                             Canton=="SH" ~ 2,
                                             Canton=="AR" ~ 1,
                                             Canton=="AI" ~ 1,
                                             Canton=="SG" ~ 12,
                                             Canton=="GR" ~ 5,
                                             Canton=="AG" ~ 16,
                                             Canton=="TG" ~ 6,
                                             Canton=="TI" ~ 8,
                                             Canton=="VD" ~ 19,
                                             Canton=="VS" ~ 8,
                                             Canton=="NE" ~ 4,
                                             Canton=="GE" ~ 12,
                                             Canton=="JU" ~ 2))


### cleaning ----
rm(list=setdiff(ls(), "final"))
final <- select(final, Canton, party, issue, number, number_other, potential, N.p, voters, N.v, public_2, 
                position_voter, position_potential, ownership,
                party_size, gov_status, district)
final$Canton <- as.factor(final$Canton)
final$party <- as.factor(final$party)
final$issue <- as.factor(final$issue)
final$issue_canton <- as.factor(final$issue_canton)
final$party <- recode(final$party, PLR="FDP", PDC="CVP", PDB="BDP", PS="SP", UDC="SVP", Verts="Green", Verts_lib="Green_liberal")


### enlever les observations qui n'ont rien pour voters car aucune observations dans les Selects ----
# PDC
final <- final[!(final$Canton=="SH" & final$party=="CVP"),]
# VERTS_lib
final <- final[!(final$party=="Green_liberal"),]

# SP
final <- final[!(final$Canton=="OW" & final$party=="SP"),]
# VERTS
final <- final[!(final$Canton=="GR" & final$party=="Green"),]

# remplace les NA (= personnes n'a dit "4-extremely important")
final$potential <- final$potential %>%  replace_na(0)
final$voters <- final$voters %>%  replace_na(0)


save(final, file = "final_2019_prop_aout.Rdata")

D <- read_dta("Wahlinserate_2019_allcantons.dta")

### cleaning ----
rm(list=setdiff(ls(), "final"))
final <- select(final, Canton, party, issue, number, number_other, potential, N.p, voters, N.v, public_2, 
                position_voter, position_potential, ownership,
                party_size, district,)
final$Canton <- as.factor(final$Canton)
final$party <- as.factor(final$party)
final$issue <- as.factor(final$issue)
final$party <- recode(final$party, PLR="FDP", PDC="CVP", PDB="BDP", PS="SP", UDC="SVP", Verts="Green", Verts_lib="Green_liberal")

### enlever les observations qui n'ont rien pour voters car aucune observations dans les Selects ----
# PDC
final <- final[!(final$Canton=="SH" & final$party=="CVP"),]
# VERTS_lib
final <- final[!(final$party=="Green_liberal"),]

# SP
final <- final[!(final$Canton=="OW" & final$party=="SP"),]
# VERTS
final <- final[!(final$Canton=="GR" & final$party=="Green"),]

# remplace les NA (= personnes n'a dit "4-extremely important")
final$potential <- final$potential %>%  replace_na(0)
final$voters <- final$voters %>%  replace_na(0)


save(final, file = "final_2019_prop_aout.Rdata")













###########################################################################################################################
############                                       Analysis                                                    ############
###########################################################################################################################



###########################################################################################################################
load("~/R/first paper/final_2015_prop_aout.Rdata")
final_2015 <- final
load("~/R/first paper/final_2019_prop_aout.Rdata")
final_2019 <- final

final_2019$year <- "2019"
final_2015$year <- "2015"

final_both <- rbind(final_2019, final_2015)
rm(list=setdiff(ls(), "final_both"))

############################################################################################################################
### load les deux dataset ----
setwd("C:/Users/borgeatq/Documents/R/first paper")
load("~/R/first paper/final_both_prop_aout.Rdata")

final_both$year <- as.factor(final_both$year)
final_both$gov_status <- as.factor(final_both$gov_status)
final_both$ownership <- as.factor(final_both$ownership)

final_both <- final_both[!is.na(final_both$public),]
final_both <- final_both[!is.na(final_both$position_voter),]
final_both <- final_both[!(final_both$party=="Green_liberal"),]

### relative measure of DV ----
x <- aggregate(final_both$number, by=list(Canton=final_both$Canton, year=final_both$year, party=final_both$party), FUN=sum)

final_both <-left_join(final_both, x, by=c("Canton", "year", "party"))

final_both$number_rel <- ((final_both$number*100)/final_both$x)

final_both$number_rel <- as.integer(final_both$number_rel)

### relative measure position ----

final_both$distance_v <- abs(final_both$position_voter-final_both$position_party)
final_both$distance_p <- abs(final_both$position_potential-final_both$position_party)
final_both$distance_p_v <- abs(final_both$position_potential-final_both$position_voter)

### relative measure IV ----
final_both$difference_voters <- (final_both$potential-final_both$voters)
final_both$difference_voters2 <- abs(final_both$difference_voters)
final_both$difference_public_2 <- (final_both$potential-final_both$public_2)


### party_size reprendre valeur extreme ----
final_both$party_size[final_both$party_size==82.8] <- 41
final_both$party_size[final_both$party_size==76.3] <- 38
### Issue ownership ----
final_both <- final_both %>% mutate(IO=case_when((Canton=="ZH" & issue=="immigration" & party=="SP" & year=="2019") ~ 1,
                                                     (Canton=="BE" & issue=="immigration" & party=="SP" & year=="2019") ~ 1,
                                                     (Canton=="LU" & issue=="immigration" & party=="SP" & year=="2019") ~ 1,
                                                     (Canton=="UR" & issue=="immigration" & party=="SVP" & year=="2019") ~ 1,
                                                     (Canton=="SW" & issue=="immigration" & party=="SVP" & year=="2019") ~ 1,
                                                     (Canton=="OW" & issue=="immigration" & party=="SVP" & year=="2019") ~ 1,
                                                     (Canton=="NW" & issue=="immigration" & party=="SVP" & year=="2019") ~ 1,
                                                     (Canton=="GL" & issue=="immigration" & party=="SVP" & year=="2019") ~ 1,
                                                     (Canton=="ZG" & issue=="immigration" & party=="SP" & year=="2019") ~ 1,
                                                     (Canton=="FR" & issue=="immigration" & party=="SP" & year=="2019") ~ 1,
                                                     (Canton=="SO" & issue=="immigration" & party=="SP" & year=="2019") ~ 1,
                                                     (Canton=="BS" & issue=="immigration" & party=="SP" & year=="2019") ~ 1,
                                                     (Canton=="BL" & issue=="immigration" & party=="SVP" & year=="2019") ~ 1,
                                                     (Canton=="SH" & issue=="immigration" & party=="SP" & year=="2019") ~ 1,
                                                     (Canton=="ZU" & issue=="immigration" & party=="SVP" & year=="2019") ~ 1,
                                                     (Canton=="AR" & issue=="immigration" & party=="SVP" & year=="2019") ~ 1,
                                                     (Canton=="AI" & issue=="immigration" & party=="SP" & year=="2019") ~ 1,
                                                     (Canton=="SG" & issue=="immigration" & party=="SVP" & year=="2019") ~ 1,
                                                     (Canton=="GR" & issue=="immigration" & party=="SP" & year=="2019") ~ 1,
                                                     (Canton=="AG" & issue=="immigration" & party=="SVP" & year=="2019") ~ 1,
                                                     (Canton=="TG" & issue=="immigration" & party=="SP" & year=="2019") ~ 1,
                                                     (Canton=="TI" & issue=="immigration" & party=="SP" & year=="2019") ~ 1,
                                                     (Canton=="VD" & issue=="immigration" & party=="SP" & year=="2019") ~ 1,
                                                     (Canton=="VS" & issue=="immigration" & party=="SVP" & year=="2019") ~ 1,
                                                     (Canton=="NE" & issue=="immigration" & party=="SP" & year=="2019") ~ 1,
                                                     (Canton=="GE" & issue=="immigration" & party=="SP" & year=="2019") ~ 1,
                                                     (Canton=="JU" & issue=="immigration" & party=="SVP" & year=="2019") ~ 1,
                                                     (Canton=="JU" & issue=="immigration" & party=="SP" & year=="2019") ~ 1,
                                                     (issue=="environment" & party=="Green" & year=="2019") ~ 1,
                                                     (issue=="social" & party=="SP" & year=="2019") ~ 1,
                                                     (issue=="economy" & party=="FDP" & year=="2019") ~ 1,
                                                     (Canton=="JU" & issue=="economy" & party=="FDP" & year=="2019") ~0,
                                                     (issue=="EU" & party=="FDP" & year=="2019") ~ 1,
                                                     (Canton=="UR" & issue=="EU" & party=="FDP" & year=="2019") ~ 0,
                                                     (Canton=="GL" & issue=="EU" & party=="FDP" & year=="2019") ~ 0,
                                                     (Canton=="BS" & issue=="EU" & party=="FDP" & year=="2019") ~ 0,
                                                     (Canton=="SH" & issue=="EU" & party=="FDP" & year=="2019") ~ 0,
                                                     (Canton=="UR" & issue=="EU" & party=="SVP" & year=="2019") ~ 1,
                                                     (Canton=="GL" & issue=="EU" & party=="SVP" & year=="2019") ~ 1,
                                                     (Canton=="BS" & issue=="EU" & party=="SP" & year=="2019") ~ 1,
                                                     (Canton=="SH" & issue=="EU" & party=="SVP" & year=="2019") ~ 1,
                                                     (issue=="EU" & party=="FDP" & year=="2015") ~ 1,
                                                     (Canton=="UR" & issue=="EU" & party=="FDP" & year=="2015") ~ 0,
                                                     (Canton=="OW" & issue=="EU" & party=="FDP" & year=="2015") ~ 0,
                                                     (Canton=="BS" & issue=="EU" & party=="FDP" & year=="2015") ~ 0,
                                                     (Canton=="UR" & issue=="EU" & party=="SVP" & year=="2015") ~ 1,
                                                     (Canton=="OW" & issue=="EU" & party=="SVP" & year=="2015") ~ 1,
                                                     (Canton=="BS" & issue=="EU" & party=="SP" & year=="2015") ~ 1,
                                                     (issue=="environment" & party=="Green" & year=="2015") ~ 1,
                                                     (issue=="social" & party=="SP" & year=="2015") ~ 1,
                                                     (issue=="economy" & party=="FDP" & year=="2015") ~ 1,
                                                     (issue=="immigration" & party=="SVP" & year=="2015") ~ 1,
                                                     (Canton=="ZG" & issue=="immigration" & party=="SVP" & year=="2015") ~ 0,
                                                     (Canton=="BS" & issue=="immigration" & party=="SVP" & year=="2015") ~ 0,
                                                     (Canton=="TI" & issue=="immigration" & party=="SVP" & year=="2015") ~ 0,
                                                     (Canton=="NE" & issue=="immigration" & party=="SVP" & year=="2015") ~ 0,
                                                     (Canton=="GE" & issue=="immigration" & party=="SVP" & year=="2015") ~ 0,
                                                     (Canton=="ZG" & issue=="immigration" & party=="CVP" & year=="2015") ~ 1,
                                                     (Canton=="BS" & issue=="immigration" & party=="SP" & year=="2015") ~ 1,
                                                     (Canton=="TI" & issue=="immigration" & party=="SP" & year=="2015") ~ 1,
                                                     (Canton=="NE" & issue=="immigration" & party=="SP" & year=="2015") ~ 1,
                                                     (Canton=="GE" & issue=="immigration" & party=="SP" & year=="2015") ~ 1))


final_both$IO[is.na(final_both$IO)] <- 0

final_both$IO <- as.factor(final_both$IO)
### party_year ----
party <- final_both$party
year <- final_both$year
issue <- final_both$issue
party_issue <- final_both$party_issue


final_both$party_year <- ifelse((party=="FDP") & (year=="2019"), "FDP_2019",
                                  ifelse((party=="FDP") & (year=="2015"), "FDP_2015",
                                         ifelse((party=="SVP") & (year=="2019"), "SVP_2019",
                                                ifelse((party=="SVP") & (year=="2015"), "SVP_2015",
                                                       ifelse((party=="CVP") & (year=="2019"), "CVP_2019",
                                                              ifelse((party=="CVP") & (year=="2015"), "CVP_2015",
                                                                     ifelse((party=="SP") & (year=="2019"), "SP_2019",
                                                                            ifelse((party=="SP") & (year=="2015"), "SP_2015",
                                                                                   ifelse((party=="Green") & (year=="2019"), "Green_2019",
                                                                                          ifelse((party=="Green") & (year=="2015"), "Green_2015",
                                                                                                 ifelse((party=="Green_liberal") & (year=="2019"), "Green_liberal_2019",
                                                                                                        ifelse((party=="Green_liberal") & (year=="2015"), "Green_liberal_2015", 99))))))))))))


## analysis both ----
library(NBZIMM)
library(GLMMadaptive)
library(dataPreparation)
library(performance)
library(DHARMa)
library(insight)
library(MuMIn)
library(stargazer)
library(glmmTMB)

### DESCRIPTIVE STATISTICS - suite ---- 

stargazer(final_both, type = "text", out = "table_2.html")
### finaliser ----
scales <-build_scales(final_both, cols = c("potential","voters","public", "public_2", "party_size", "gov_status", 
                                           "distance_p", "distance_v", "distance_p_v", "position_voter", "position_potential", 
                                           "number_other", "district", "party_size_weighted", "difference_voters", "difference_public_2", "IO", "party_year"), 
                      verbose = TRUE)
final_both_s <-fast_scale(final_both, scales = scales, way = "scale", verbose = TRUE)


rm(list=setdiff(ls(), "final_both_s"))

final_both_s$Canton <- relevel(final_both_s$Canton, ref = "NW")
final_both_s$issue <- relevel(final_both_s$issue, ref = "immigration")
final_both_s$party <- relevel(final_both_s$party, ref = "Green")
final_both_s$year <- relevel(final_both_s$year, ref = "2019")

write.dta(final_both_s, "C:/Users/borgeatq/Documents/R/first paper/final_both_prop_s_fin aout.dta", 
          convert.factors = c("labels", "string", "numeric", "codes"))




### Model 1 ----
summary(glmm.tmb_potential <- glmmTMB(number ~ potential+party_size+IO
                            +distance_p_v+number_other+district+year+
                              (1|Canton)+(1|issue)+(1|party),
                            zi = ~ party_size+district+issue+party,
                            data = final_both_s,
                            family = nbinom2(link = "log"),
                            control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))))

sjPlot::tab_model(glmm.tmb_potential, transform=NULL, show.se = TRUE, 
                  show.aic = TRUE, string.est = "Estimate", p.style = "stars")

potential_predict_19 <- ggpredict(glmm.tmb_potential, c("potential[-2.537, -0.641, 0.307, 1.255, 2.203, 3.092]"), 
                               type = "fe", 
                               condition = c(voters=mean(final_both_s$voters),
                                             public_2=mean(final_both_s$public_2),
                                             party_size=mean(final_both_s$party_size),
                                             gov_status=1,
                                             IO=0,
                                             distance_p_v=mean(final_both_s$distance_p_v),
                                             number_other=mean(final_both_s$number_other),
                                             district=mean(final_both_s$district),
                                             year="2019"))
potential_predict_19
plot(potential_predict_19)

potential_predict_15 <- ggpredict(glmm.tmb_potential, c("potential[-2.537, -0.641, 0.307, 1.255, 2.203, 3.092]"), 
                                  type = "fe", 
                                  condition = c(voters=mean(final_both_s$voters),
                                                public_2=mean(final_both_s$public_2),
                                                party_size=mean(final_both_s$party_size),
                                                gov_status=1,
                                                IO=0,
                                                distance_p_v=mean(final_both_s$distance_p_v),
                                                number_other=mean(final_both_s$number_other),
                                                district=mean(final_both_s$district),
                                                year="2015"))
potential_predict_15
plot(potential_predict_15)

## Model 2 ----
summary(glmm.tmb_voters <- glmmTMB(number ~ voters+party_size+IO
                            +distance_p_v+number_other+district+year+
                              (1|Canton)+(1|issue)+(1|party),
                            zi = ~ party_size+district+issue+party,
                            data = final_both_s,
                            family = nbinom2(link = "log"),
                            control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))))


sjPlot::tab_model(glmm.tmb_voters, transform=NULL, show.se = TRUE, 
                  show.aic = TRUE, string.est = "Estimate", p.style = "stars")

### Model 3 ----
summary(glmm.tmb_public <- glmmTMB(number ~ public_2+party_size+IO
                                   +distance_p_v+number_other+district+year+
                                     (1|Canton)+(1|issue)+(1|party),
                                   zi = ~ party_size+district+issue+party,
                                   data = final_both_s,
                                   family = nbinom2(link = "log"),
                                   control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))))


sjPlot::tab_model(glmm.tmb_public, transform=NULL, show.se = TRUE, 
                  show.aic = TRUE, string.est = "Estimate", p.style = "stars")

### Model 4 ----
summary(glmm.tmb <- glmmTMB(number ~ potential+public_2+voters+party_size+IO
                            +distance_p_v+number_other+district+year+
                              (1|Canton)+(1|issue)+(1|party),
                            zi = ~ party_size+district+issue+party,
                            data = final_both_s,
                            family = nbinom2(link = "log"),
                            control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))))
summary(glmm.tmb)

sjPlot::tab_model(glmm.tmb, transform=NULL, show.se = TRUE, 
                  show.aic = TRUE, string.est = "Estimate", p.style = "stars")


### predict probabilities - Model 4 ----
library(car)
library(ggeffects)

potential_predict <- ggpredict(glmm.tmb, c("potential[-2.282, -0.602, 0.238, 1.078, 1.918, 2.758]"), 
                               type = "fe", 
                               condition = c(voters=mean(final_both_s$voters),
                                             public_2=mean(final_both_s$public_2),
                                             party_size=mean(final_both_s$party_size),
                                             gov_status=1,
                                             IO=0,
                                             distance_p_v=mean(final_both_s$distance_p_v),
                                             number_other=mean(final_both_s$number_other),
                                             district=mean(final_both_s$district),
                                             year="2019"))

plot_prob_potential_fe <- ggplot(potential_predict, aes(x=x))+
  geom_line(aes(y=predicted), color="black", linetype=1)+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), color="gray40", linetype=2, alpha=0.1)+
  xlab("Issue saliency among potential voters") + ylab("Predicted number of ads")+
  scale_y_continuous(limits = c(0, 15))+scale_x_continuous(breaks=c(-2.282, -0.602, 1.078, 2.758))+theme_minimal()

plot_prob_potential_fe


voters_predict <- ggpredict(glmm.tmb, c("voters[-1.518, -0.496, 0.014, 0.525, 1.035, 1.546]"), 
                            type = "fe", 
                            condition = c(potential=mean(final_both_s$potential),
                                          public_2=mean(final_both_s$public_2),
                                          party_size=mean(final_both_s$party_size),
                                          gov_status=1,
                                          IO=0,
                                          distance_p_v=mean(final_both_s$distance_p_v),
                                          number_other=mean(final_both_s$number_other),
                                          district=mean(final_both_s$district),
                                          year="2019"))



plot_prob_voters_fe <- ggplot(voters_predict, aes(x=x))+
  geom_line(aes(y=predicted), color="black", linetype=1)+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), color="gray40", linetype=2, alpha=0.1)+
  xlab("Issue saliency among voters") + ylab("Predicted number of ads")+
  scale_y_continuous(limits = c(0, 15))+scale_x_continuous(breaks=c(-1.518, -0.496, 0.525, 1.546))+theme_minimal()


plot_prob_voters_fe


public_2_predict <- ggpredict(glmm.tmb, c("public_2[-2.714, -0.772, 0.343, 1.456, 4.120]"), 
                              type = "fe", 
                              condition = c(voters=mean(final_both_s$voters),
                                            potential=mean(final_both_s$potential),
                                            party_size=mean(final_both_s$party_size),
                                            gov_status=1,
                                            IO=0,
                                            distance_p_v=mean(final_both_s$distance_p_v),
                                            number_other=mean(final_both_s$number_other),
                                            district=mean(final_both_s$district),
                                            year="2019"))


plot_prob_public_fe <- ggplot(public_2_predict, aes(x=x))+
  geom_line(aes(y=predicted), color="black", linetype=1)+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), color="gray40", linetype=2, alpha=0.1)+
  xlab("Issue saliency among the public") + ylab("Predicted number of ads")+
  scale_y_continuous(limits = c(0, 15))+scale_x_continuous(breaks=c(-2.714, -0.772, 1.456, 4.120))+theme_minimal()

plot_prob_public_fe


library(ggpubr)
predict_prob <- ggarrange(plot_prob_potential_fe, plot_prob_voters_fe, plot_prob_public_fe, common.legend = TRUE)

predict_prob

### predict probabilities with interaction ----

summary(glmm.tmb_interaction <- glmmTMB(number ~ potential*voters+public_2+party_size+IO
                            +distance_p_v+number_other+district+year+
                              (1|Canton)+(1|issue)+(1|party),
                            zi = ~ party_size+district+issue+party,
                            data = final_both_s,
                            family = nbinom2(link = "log"),
                            control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))))
summary(glmm.tmb_interaction)

sjPlot::tab_model(glmm.tmb_interaction, transform=NULL, show.se = TRUE, 
                  show.aic = TRUE, string.est = "Estimate", p.style = "stars")

library(ggeffects)
potential_predict_interaction <- ggpredict(glmm.tmb_interaction, c("potential[-2.282, -0.602, 0.238, 1.078, 1.918, 2.758]", 
                                           "voters[-1.518, 1.546]"), 
                               type = "fe", 
                               condition = c(public_2=mean(final_both_s$public_2),
                                             party_size=mean(final_both_s$party_size),
                                             IO=0,
                                             distance_p_v=mean(final_both_s$distance_p_v),
                                             number_other=mean(final_both_s$number_other),
                                             district=mean(final_both_s$district),
                                             year="2019"))

potential_predict_interaction
plot(potential_predict_interaction)

plot_prob_potential_fe_interaction <- ggplot(potential_predict_interaction, aes(x=x, group = group))+
  geom_line(aes(y=predicted, group = group), color="black", linetype=1)+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill = group), linetype=2, alpha=0.1)+
  scale_fill_manual(name = 'Issue saliency among voters', values=c("grey1", "grey50"))+
  xlab("Issue saliency among potential voters") + ylab("Predicted number of ads")+
  scale_y_continuous(limits = c(0, 15))+scale_x_continuous(breaks=c(-2.282, -0.602, 1.078, 2.758))+
  theme_minimal()+
  theme(legend.position="top")
plot_prob_potential_fe_interaction

### MODEL WITH RELATIVE VD -----

summary(glmm.tmb_rel_1 <- glmmTMB(number_rel ~ potential+party_size+IO
                                +distance_p_v+number_other+district+year+
                                  (1|Canton)+(1|issue)+(1|party),
                                zi = ~ party_size+district+issue+party,
                                data = final_both_s,
                                family = nbinom2(link = "log"),
                                control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))))


sjPlot::tab_model(glmm.tmb_rel_1, transform=NULL, show.se = TRUE, 
                  show.aic = TRUE, string.est = "Estimate", p.style = "stars")


summary(glmm.tmb_rel_2 <- glmmTMB(number_rel ~ voters+party_size+IO
                                +distance_p_v+number_other+district+year+
                                  (1|Canton)+(1|issue)+(1|party),
                                zi = ~ party_size+district+issue+party,
                                data = final_both_s,
                                family = nbinom2(link = "log"),
                                control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))))


sjPlot::tab_model(glmm.tmb_rel_2, transform=NULL, show.se = TRUE, 
                  show.aic = TRUE, string.est = "Estimate", p.style = "stars")

summary(glmm.tmb_rel_3 <- glmmTMB(number_rel ~ public_2+party_size+IO
                                  +distance_p_v+number_other+district+year+
                                    (1|Canton)+(1|issue)+(1|party),
                                  zi = ~ party_size+district+issue+party,
                                  data = final_both_s,
                                  family = nbinom2(link = "log"),
                                  control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))))


sjPlot::tab_model(glmm.tmb_rel_3, transform=NULL, show.se = TRUE, 
                  show.aic = TRUE, string.est = "Estimate", p.style = "stars")


summary(glmm.tmb_rel <- glmmTMB(number_rel ~ potential+voters+public_2+party_size+IO
                            +distance_p_v+number_other+district+year+
                              (1|Canton)+(1|issue)+(1|party),
                            zi = ~ party_size+district+issue+party,
                            data = final_both_s,
                            family = nbinom2(link = "log"),
                            control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))))


sjPlot::tab_model(glmm.tmb_rel, transform=NULL, show.se = TRUE, 
                  show.aic = TRUE, string.est = "Estimate", p.style = "stars")


potential_predict_rel <- ggpredict(glmm.tmb_rel, c("potential[-2.282, -0.602, 0.238, 1.078, 1.918, 2.758]"), 
                                   type = "fe", 
                                   condition = c(voters=mean(final_both_s$voters),
                                                 public_2=mean(final_both_s$public_2),
                                                 party_size=mean(final_both_s$party_size),
                                                 gov_status=1,
                                                 IO=1,
                                                 distance_p_v=mean(final_both_s$distance_p_v),
                                                 number_other=mean(final_both_s$number_other),
                                                 district=mean(final_both_s$district),
                                                 year="2019"))

plot_prob_vd_rel <- ggplot(potential_predict_rel, aes(x=x))+
  geom_line(aes(y=predicted), color="black", linetype=1)+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), color="gray40", linetype=2, alpha=0.1)+
  xlab("Issue saliency among potential voters") + ylab("Predicted percentage of ads")+
  scale_y_continuous(limits = c(0, 180))+scale_x_continuous(breaks=c(-2.282, -0.602, 1.078, 2.758))+theme_minimal()

plot_prob_vd_rel


ggsave('plot_prob_vd_rel.png', plot_prob_vd_rel, width = 10, height = 8, units = "in")

voters_predict_rel <- ggpredict(glmm.tmb_rel, c("voters[-1.518, -0.496, 0.014, 0.525, 1.035, 1.546]"), 
                                type = "fe", 
                                condition = c(potential=mean(final_both_s$potential),
                                              public_2=mean(final_both_s$public_2),
                                              party_size=mean(final_both_s$party_size),
                                              gov_status=1,
                                              IO=1,
                                              distance_p_v=mean(final_both_s$distance_p_v),
                                              number_other=mean(final_both_s$number_other),
                                              district=mean(final_both_s$district),
                                              year="2019"))

plot_prob_vd_rel_voters <- ggplot(voters_predict_rel, aes(x=x))+
  geom_line(aes(y=predicted), color="black", linetype=1)+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), color="gray40", linetype=2, alpha=0.1)+
  xlab("Issue saliency among potential voters") + ylab("")+
  scale_y_continuous(limits = c(0, 180))+scale_x_continuous(breaks=c(-1.518, -0.496, 0.525, 1.546))+theme_minimal()

plot_prob_vd_rel_voters

predict_prob_rel <- ggarrange(plot_prob_vd_rel, plot_prob_vd_rel_voters, common.legend = TRUE)

predict_prob_rel

### MODEL WITH RELATIVE VI -----
summary(glmm.tmb_vd_vi_rel <- glmmTMB(number ~ difference_voters+difference_public_2+party_size+IO
                                      +distance_p_v+number_other+district+year+
                                        (1|Canton)+(1|issue)+(1|party),
                                      zi = ~ party_size+district+issue+party,
                                      data = final_both_s,
                                      family = nbinom2(link = "log"),
                                      control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))))

sjPlot::tab_model(glmm.tmb_vd_vi_rel, transform=NULL, show.se = TRUE, 
                  show.aic = TRUE, string.est = "Estimate", p.style = "stars")


potential_predict_vi_rel <- ggpredict(glmm.tmb_vd_vi_rel, c("difference_voters"), 
                                      type = "fe", 
                                      condition = c(difference_public_2=mean(final_both_s$difference_public_2),
                                                    party_size=mean(final_both_s$party_size),
                                                    gov_status=1,
                                                    IO=1,
                                                    distance_p_v=mean(final_both_s$distance_p_v),
                                                    number_other=mean(final_both_s$number_other),
                                                    district=mean(final_both_s$district),
                                                    year="2019"))


plot_prob_difference_voters_fe <- ggplot(potential_predict_vi_rel, aes(x=x))+
  geom_line(aes(y=predicted), color="black", linetype=1)+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), color="gray40", linetype=2, alpha=0.1)+
  xlab("Difference in Issue saliency between potential voters and voters") + ylab("Predicted number of ads")+
  scale_y_continuous(limits = c(0, 40))+theme_minimal()

plot_prob_difference_voters_fe


potential_predict_vi_rel_2 <- ggpredict(glmm.tmb_vd_vi_rel, c("difference_public_2"), 
                                        type = "fe", 
                                        condition = c(difference_voters=mean(final_both_s$difference_voters),
                                                      party_size=mean(final_both_s$party_size),
                                                      gov_status=1,
                                                      IO=1,
                                                      distance_p_v=mean(final_both_s$distance_p_v),
                                                      number_other=mean(final_both_s$number_other),
                                                      district=mean(final_both_s$district),
                                                      year="2019"))

plot_prob_difference_voters_fe2 <- ggplot(potential_predict_vi_rel_2, aes(x=x))+
  geom_line(aes(y=predicted), color="black", linetype=1)+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), color="gray40", linetype=2, alpha=0.1)+
  xlab("Difference in Issue saliency between potential voters and public")+ ylab("")+
  scale_y_continuous(limits = c(0, 40))+theme_minimal()

plot_prob_difference_voters_fe2

predict_prob_vi_rel <- ggarrange(plot_prob_difference_voters_fe, plot_prob_difference_voters_fe2, common.legend = TRUE)

predict_prob_vi_rel

