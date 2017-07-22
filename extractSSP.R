library(rvest)
library(dplyr)
library(stringr)
library(reshape2)
library(tidyr)
rm(list = ls())

# From 1995 to 2003 "_" Ex:http://www.ssp.sp.gov.br/Estatistica/plantrim/2001_01.htm
# From 2004 to 2017 "- Ex: http://www.ssp.sp.gov.br/Estatistica/plantrim/2017-01.htm

#1995_01 to 2017-01

# Script
#   Read each page
#   Extract tables
#   Clean Tables
#   Join Tables


years = c(1996:2016)
quarter = c("01","02","03","04")
#DEBUG
#years = c(2000:2002)
#quarter = c("02")
root <- "http://www.ssp.sp.gov.br/Estatistica/plantrim/"
extension <- ".htm"

for (i in years){
  for(j in quarter){
      period <- paste(i,"Q",j, sep = "_")
    if(i <= 2003){
      file <- paste(i,j,sep = "_")
    }else{
      file <- paste(i,j,sep = "-")
    }
    doclink <- read_html(paste0(root, file, extension))
    node <- html_nodes(doclink,"table")
    if(i == "2001"){
      tb <- html_table(node[[1]], fill = TRUE)
      if(file %in% c("2001_01","2001_03","2001_04")){
        set <- filter(tb, X2 == "IX")
        set2 <- filter(tb, X2 != "IX")
        set$X2 <- NULL
        set3 <- filter(tb, X12 == "IX")
        set3 <- select(set3, X12,X13,X14,X15,X16)
        colnames(set3) <- c("X1", "X2", "X3", "X4", "X5")
        set <- select(set, X1, X3, X4, X5, X6)
        colnames(set) <- c("X1", "X2", "X3", "X4", "X5")
        set3$X3 <- as.character(set3$X3)
        set3$X4 <- as.character(set3$X4)
        set3$X5 <- as.character(set3$X5)
        set2 <- select(set2, X1, X2, X3, X4, X5)
        tb <- bind_rows(set2,set)
        tb <- bind_rows(tb,set3)
      }
    }else{
      for(k in 1:length(node)){
        if(k == 1){
          tb <- html_table(node[[k]], fill = TRUE)
        }else{
          temp <- html_table(node[[k]], fill = TRUE)
          tb <- bind_rows(tb,temp)
        }
      }
    }
    tb$X1 <- period
    if(i == years[1] && j == quarter[1]){
      base <- select(tb, X1, X2, X3, X4, X5)
    }else{
      temp <- select(tb, X1, X2, X3, X4, X5)
    base <- bind_rows(base, temp)
    }
    print(paste(ncol(tb), " ",period))
  }
}

write.csv(base, "../raw_extract.csv")
base <- read.csv("../raw_extract.csv")

raw <- base
raw$X <- NULL

raw$X2 <- str_to_upper(raw$X2)
raw$X3 <- str_to_upper(raw$X3)
raw$X4 <- str_to_upper(raw$X4)
raw$X5 <- str_to_upper(raw$X5)

raw$X3 <- gsub("^\\..*|-", 0, raw$X3)
raw$X4 <- gsub("^\\..*|-", 0, raw$X4)
raw$X5 <- gsub("^\\..*|-", 0, raw$X5)

raw$X3 <- gsub("^\\(\\d\\) ", "", raw$X3)
raw$X4 <- gsub("^\\(\\d\\) ", "", raw$X4)
raw$X5 <- gsub("^\\(\\d\\) ", "", raw$X5)

#bk <- raw
#raw <- bk
raw <- raw %>%
  filter(str_length("X5") > 0) %>%
  filter(!grepl("^\\(|^\\d", X2)) %>%
  filter(grepl("^\\d", X3))


colnames(raw) <- c("Period", "Item", "Capital", "Metropolitan", "Interior")

raw$Item <- gsub("\n  | {1,50}"," ", raw$Item)
raw$Item <- gsub("[ÔÕ]","O", raw$Item)
raw$Item <- gsub("Í","I", raw$Item)
raw$Item <- gsub("Ã","A", raw$Item)
raw$Item <- str_trim(raw$Item, side = "both")
raw$Item <- gsub("DELITOS ","", raw$Item)
raw$Item <- gsub("CONTRA COSTUMES","CONTRA OS COSTUMES", raw$Item)
raw$Item <- gsub("CONTRA PESSOA","CONTRA A PESSOA", raw$Item)
raw$Item <- gsub("CONTRA PATRIMONIO","CONTRA O PATRIMONIO", raw$Item)
raw$Item <- gsub("^ESTUPRO \\(.*","ESTUPRO", raw$Item)
raw$Item <- gsub("^EXAMES CLINICOS LABORATORIAIS REALIZADOS - IML.*","EXAMES CLINICOS LABORATORIAIS REALIZADOS - IML", raw$Item)
raw$Item <- gsub("^EXAMES NECROSCÓPICOS REALIZADOS - IML.*","EXAMES NECROSCÓPICOS REALIZADOS - IML", raw$Item)
raw$Item <- gsub("^OUTROS.*|^CONTRAVEN.*","OUTROS", raw$Item)
raw$Item <- gsub("^LESAO CORPORAL.*","LESAO CORPORAL", raw$Item)
raw$Item <- gsub("^ROUBO - OUTROS.*","ROUBO - OUTROS", raw$Item)
raw$Item <- gsub("^LAUDOS OUTROS EXPEDIDOS DO ANO - IML.*","LAUDOS OUTROS EXPEDIDOS DO ANO - IML", raw$Item)
raw$Item <- gsub("^LAUDOS OUTROS EXPEDIDOS DO ANO ANTERIOR IML.*","LAUDOS OUTROS EXPEDIDOS DO ANO ANTERIOR IML", raw$Item)
raw$Item <- gsub("^EXAMES OUTROS REALIZADOS - IML.*","EXAMES OUTROS REALIZADOS - IML", raw$Item)
raw$Item <- gsub("^EXAMES PERICIAIS REALIZADOS - IC.*","EXAMES PERICIAIS REALIZADOS - IC", raw$Item)
raw$Item <- gsub("^EXTORSAO MEDIANTE SEQÜESTRO.*","EXTORSAO MEDIANTE SEQÜESTRO", raw$Item)
raw$Item <- gsub("VEICULOS?","VEICULOS", raw$Item)
raw$Item <- gsub("^HOMICIDIO CULPOSO.*","HOMICIDIO CULPOSO", raw$Item)
raw$Item <- gsub("^HOMICIDIO DOLOSO.*","HOMICIDIO DOLOSO", raw$Item)
raw$Item <- gsub("^LAUDOS CLINICOS LABORATORIAIS EXPEDIDOS (DO ANO )?- IML.*","LAUDOS CLINICOS LABORATORIAIS EXPEDIDOS DO ANO - IML", raw$Item)
raw$Item <- gsub("^LAUDOS CLINICOS LABORATORIAIS EXPEDIDOS DO ANO ANTERIOR - IML.*","LAUDOS CLINICOS LABORATORIAIS EXPEDIDOS DO ANO ANTERIOR - IML", raw$Item)
raw$Item <- gsub("^LAUDOS NECROSCÓPICOS EXPEDIDOS (DO ANO )?- IML.*","LAUDOS NECROSCÓPICOS EXPEDIDOS DO ANO - IML", raw$Item)
raw$Item <- gsub("^LAUDOS NECROSCÓPICOS EXPEDIDOS DO ANO ANTERIOR.*","LAUDOS NECROSCÓPICOS EXPEDIDOS DO ANO ANTERIOR - IML", raw$Item)
raw$Item <- gsub("^LAUDOS PERICIAIS EXPEDIDOS (DO ANO )?- IC.*","LAUDOS PERICIAIS EXPEDIDOS DO ANO - IC", raw$Item)
raw$Item <- gsub("^LAUDOS PERICIAIS EXPEDIDOS DO ANO ANTERIOR- IC.*","LAUDOS PERICIAIS EXPEDIDOS DO ANO ANTERIOR- IC", raw$Item)
raw$Item <- gsub("^Nº DE AUTOS DE APREENSAO \\(ART\\..*","Nº DE AUTOS DE APREENSAO (ART 173 ECA)", raw$Item)
raw$Item <- gsub("^Nº DE REVISTAS PESSOAIS.*","Nº DE REVISTAS PESSOAIS/ IDENTIFICAÇÃO", raw$Item)
raw$Item <- gsub("^Nº DE VITIMAS DELATROCINIO.*","Nº DE VITIMAS DE LATROCINIO", raw$Item)
raw$Item <- gsub("^Nº DE VITIMAS EM HOMICIDIO DOLOSO POR ACIDEN.*","Nº DE VITIMAS EM HOMICIDIO DOLOSO POR ACIDENTE DE TRANSITO", raw$Item)
raw$Item <- gsub("CONF\\. ?POL\\. {1,2}?CIV.","CONFRONTO COM A POLICIA CIVIL", raw$Item)
raw$Item <- gsub("CONF\\. ?POL\\. {1,2}?MIL.","CONFRONTO COM A POLICIA MILITAR", raw$Item)
raw$Item <- gsub("POL. CIV.","POLICIAL CIVIL", raw$Item)
raw$Item <- gsub("POL. MIL.","POLICIAL MILITAR", raw$Item)
raw$Item <- gsub("\\(\\d.?\\)","", raw$Item)
raw$Item <- gsub("TOT\\.","TOTAL", raw$Item)
raw$Item <- gsub("TOTAL INQ. POL. INSTAURADOS","TOTAL DE INQUÉRITOS INSTAURADOS", raw$Item)
raw$Item <- gsub("BOL\\.","BOLETINS", raw$Item)
raw$Item <- gsub("POLICIAL MILITAR","POLICIAIS MILITARES", raw$Item)
raw$Item <- gsub("POLICIAL CIVIL","POLICIAIS CIVIS", raw$Item)
raw$Item <- gsub("CIRC. LAV.","CIRCUNSTANCIADOS LAVRADO", raw$Item)
raw$Item <- gsub("^PESSOAS FERIDAS EM CONFRONTO COM A POLICIA CIVIL.*|^PESSOAS FERIDAS.*CIV.*","PESSOAS FERIDAS EM CONFRONTO COM A POLICIA CIVIL", raw$Item)
raw$Item <- gsub("^PESSOAS FERIDAS EM CONFRONTO COM A POLICIA MILITAR.*|^PESSOAS FERIDAS.*MIL.*","PESSOAS FERIDAS EM CONFRONTO COM A POLICIA MILITAR", raw$Item)
raw$Item <- gsub("^PESSOAS MORTAS EM CONFRONTO COM A POLICIA CIVIL.*|^PESSOAS MORTAS.*CIV.*","PESSOAS MORTAS EM CONFRONTO COM A POLICIA CIVIL", raw$Item)
raw$Item <- gsub("^PESSOAS MORTAS EM CONFRONTO COM A POLICIA MILITAR.*|^PESSOAS MORTAS.*MIL.*$","PESSOAS MORTAS EM CONFRONTO COM A POLICIA MILITAR", raw$Item)
raw$Item <- gsub("^PRISOES EFETUADAS.*","PRISOES EFETUADAS", raw$Item)
raw$Item <- gsub("^FURTO - OUTROS.*","FURTO", raw$Item)


#1 year_quarter
#2 item
#3 capital
#4 gde sp
#5 interior

raw$Period = as.character(raw$Period)

gr <- raw %>%
  group_by(Item) %>%
  summarise(minPeriod = min(Period),
            maxPeriod = max(Period),
            count = n())
dat <- merge(raw, gr, by = "Item")

dat <- dat %>%
  filter(minPeriod == "1996_Q_01" & maxPeriod == "2016_Q_04") %>%
  select(Period,Item, Capital, Metropolitan, Interior)
dat <- melt(dat, id=c("Period", "Item"))


dat$value <- gsub("\\.","",dat$value)
dat$value <- gsub("-","0",dat$value)
dat$value <- as.numeric(dat$value)

dat <- dat %>%
  group_by(Period, Item, variable) %>%
  summarise(Value = sum(value))

data <- spread(dat,Item, Value)
colnames(data)[2] <- "Region"

# TRANSLATION
colnames(data) <- c("Period",
                    "Region",
                    "Firearms Arrested",
                    "Against the Person",
                    "Against the Patrimony",
                    "Narcotics",
                    "Rape",
                    "Kidnapping",
                    "Theft",
                    "Vehicle Theft",
                    "Manslaughter",
                    "Murder",
                    "Robbery-Murder",
                    "Bodily Injury",
                    "Others",
                    "People Hurt in Confrontation with Civil Police",
                    "People Hurt in Confrontation with Military Police",
                    "Dead People in Confrontation with Civil Police",
                    "People Hurt in Confrontation with Military Police",
                    "Civil Police Officers Hurt in Service",
                    "Dead Military Police Officers in Service",
                    "Military Police Officers Hurt in Service",
                    "Dead Military Police Officers Dead in Service",
                    "Arrests Made",
                    "Robbery of Vehicles",
                    "Attempt of Homicide",
                    "Total Notifications ",
                    "Total Felonies",
                    "Total Investigations Opened",
                    "Narcotics Traffic")
write.csv(data,"crimeData.csv")
