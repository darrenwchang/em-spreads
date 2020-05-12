#paperdata.R
#Darren Chang
#AEM 4545 Final Paper, FA19
#Cornell University, Department of Economics

library(fredr)
library(plyr)
library(tidyverse)
library(ggthemes)
library(reshape2)
library(xts)
library(zoo)
library(timetk)
library(stargazer)
library(estimatr)
library(plm)
library(gdata)
library(imputeTS)
setwd("C:/Users/darre/Documents/_Cornell 19-20/AEM 4545/_paperdata")
fredr_set_key("390e3981b7db01b1b43d340104fdd153")

sovereignDebt  <- read.csv("EMBI.csv")  %>% 
  as_tibble(.) %>%
  rename(date = ï..Date) %>%
  rename(dominicanRepublic = dominicanRepublican) %>%
  rename(Venezuela = Venezuela.RB)

sovereignDebt$date <- as.Date(paste(sovereignDebt$date,"-01",sep = ""),
 format = "%Y-%m-%d")

sovereignDebt  <- 
  sovereignDebt  %>% 
  mutate(devCountries = devCountries/100) %>%
  mutate(latAm = latAm/100) %>%
  mutate(MENA = MENA/100) %>% 
  mutate(Argentina = Argentina/100) %>% 
  mutate(Belarus = Belarus/100) %>% 
  mutate(Belize = Belize/100) %>% 
  mutate(Brazil = Brazil/100) %>% 
  mutate(Chile = Chile/100) %>% 
  mutate(China = China/100) %>% 
  mutate(Colombia = Colombia/100) %>% 
  mutate(Cote = Cote/100) %>% 
  mutate(Croatia = Croatia/100) %>%
  mutate(dominicanRepublic = dominicanRepublic/100) %>% 
  mutate(Ecuador = Ecuador/100) %>% 
  mutate(Egypt = Egypt/100) %>% 
  mutate( elSalvador = elSalvador/100) %>% 
  mutate(Gabon = Gabon/100) %>% 
  mutate(Georgia = Georgia/100) %>% 
  mutate(Ghana = Ghana/100) %>% 
  mutate(Hungary = Hungary/100) %>% 
  mutate(India = India/100) %>% 
  mutate(Indonesia = (Indonesia/100) + 0.1) %>% 
  mutate(Iraq = Iraq/100) %>% 
  mutate(Jamaica = Jamaica/100) %>% 
  mutate(Jordan = Jordan/100) %>% 
  mutate(Kazakhstan = Kazakhstan/100) %>% 
  mutate(Lebanon = Lebanon/100) %>% 
  mutate(Lithuania = Lithuania/100) %>% 
  mutate(Malaysia = Malaysia/100) %>% 
  mutate(Mexico = Mexico/100) %>% 
  mutate(Morocco = Morocco/100) %>%
  mutate(Nigeria = Nigeria/100) %>%
  mutate(Pakistan = Pakistan/100) %>% 
  mutate(Panama = Panama/100) %>% 
  mutate(Peru = Peru/100) %>% 
  mutate(Philippines = Philippines/100) %>% 
  mutate(Poland = Poland/100) %>% 
  mutate(Russia = Russia/100) %>% 
  mutate(Senegal = Senegal/100) %>% 
  mutate(southAfrica = southAfrica/100) %>% 
  mutate(sriLanka = sriLanka/100) %>% 
  mutate(trinidadAndTobago = trinidadAndTobago/100) %>% 
  mutate(Tunisia = Tunisia/100) %>% 
  mutate(Turkey = Turkey/100) %>% 
  mutate(Ukraine = Ukraine/100) %>% 
  mutate(Uruguay = Uruguay/100) %>% 
  mutate(Venezuela = Venezuela/100) %>%
  mutate(Vietnam = Vietnam/100)
  
#!colnames(sovereignDebt) == "date"

#FFR
FFR <- fredr_series_observations(
  series_id = "DFF",
  frequency = "d"
  #frequency = "m"
  )

EFFR <- FFR %>% 
  rename(EFFR = value) %>% 
  select(-series_id)

#10yr
T10Y <- fredr_series_observations(
  series_id = "DGS10",
  frequency = "m"
)

T10Y <- T10Y %>% 
  rename(T10Y = value) %>% 
  select(-series_id)

#10yr-FFR
T10YFF <- fredr_series_observations(
  series_id = "T10YFF",
  frequency = "m"
)

T10YFF <- T10YFF %>% 
  rename(T10YFF = value) %>% 
  select(-series_id)

#VIX
VIX <- fredr_series_observations(
  series_id = "VIXCLS",
  frequency = "m"
  )

VIX <- VIX %>% 
  rename(VIX = value) %>% 
  mutate(VIX = VIX + 0.1) %>% 
  select(-series_id)

#GDP
GDP <- read.csv("GDP.csv") %>%
  as_tibble(.) %>%
  rename(date = ï..date) %>%
  select(-lowIncome)

GDP$date <- as.Date(GDP$date, format = "%m/%d/%Y")

GDP_xts <- GDP %>% 
  tk_xts(silent = FALSE, date_var = date)

GDP_xts <- lag.xts(GDP_xts, k = 4) %>%
  as_tibble(.) %>% 
  mutate(date = GDP$date) %>%
  select(date, everything())

qtrsDate <- GDP$date

GDP <- (GDP-GDP_xts)/GDP_xts
GDP <- GDP %>% 
  mutate(date = qtrsDate)

GDP <- complete(GDP, date = seq(as.Date("1990-01-01"),
                            as.Date("2019-06-30"), 
                            by = "month")) %>%
       fill(2:101)

#Inflation
#world bank global economic monitor: CPI Price %yoy nominal 
CPI <- read.csv("CPI.csv") %>% 
  as_tibble(.) %>% 
  rename(date = X)

CPI$date <- as.Date(paste(CPI$date,"-01",sep = ""),
 format = "%Y-%m-%d")

#argentina CPI from FRED: FPCPITOTLZGARG
#annual percent, Not Seasonally Adjusted
Argentina_CPI <- fredr_series_observations(
  series_id = "FPCPITOTLZGARG",
  frequency = "a"
  #frequency = "m"
  )

Argentina_CPI <- Argentina_CPI %>% 
  rename(Argentina = value) %>% 
  select(-series_id)

CPI <- merge(CPI, Argentina_CPI, by = "date", all.x = TRUE)

CPI <- CPI[,order(names(CPI))] %>% 
  select(date, everything())

CPI <- complete(CPI,
  date = seq(as.Date("1989-12-01"),
            as.Date("2019-11-01"), 
            by = "month")) %>%
       fill(6) 

#debt to GDP, % of GDP
#China from IMF
debt <- read.csv("debt.csv") %>% 
  as_tibble(.) %>% 
  rename(date = ï..)

debt$date <- as.Date(paste(debt$date,"-01-01",sep = ""),
 format = "%Y-%m-%d")

China_Debt <- fredr_series_observations(
  series_id = "GGGDTACNA188N",
  frequency = "a"
  )

China_Debt <- China_Debt %>% 
  rename(China = value) %>% 
  select(-series_id)

debt <- merge(debt, China_Debt, by = "date", all.x = TRUE)

Egypt_Debt <- fredr_series_observations(
  series_id = "EGYGGDGDPGDPPT",
  frequency = "a")

Egypt_Debt <- Egypt_Debt %>% 
  rename(Egypt = value) %>% 
  select(-series_id)

debt <- merge(debt, Egypt_Debt, by = "date", all.x = TRUE)

Philippines_Debt <- fredr_series_observations(
  series_id = "DEBTTLPHA188A",
  frequency = "a")

Philippines_Debt <- Philippines_Debt %>% 
  rename(Philippines = value) %>% 
  select(-series_id)

debt <- merge(debt, Philippines_Debt, by = "date", all.x = TRUE)

debt <- debt[,order(names(debt))] %>%
  select(date, everything())

debt <- complete(debt,
  date = seq(as.Date("1988-01-01"),
            as.Date("2017-11-01"), 
            by = "month")) %>%
       fill(2:178) 

debt <- debt %>% 
  select(-Egypt.x) %>% 
  rename(Egypt = Egypt.y)

#deficit to GDP, % of GDP
revenue <- read.csv("revenue.csv") %>% 
  as_tibble(.) %>% 
  rename(date = X)

revenue$date <- as.Date(paste(revenue$date,"-01-01",sep = ""),
 format = "%Y-%m-%d")

yrdate <- revenue$date

expenditure <- read.csv("expenditure.csv") %>% 
  as_tibble(.) %>% 
  rename(date = ï..)

expenditure$date <- as.Date(paste(expenditure$date,"-01-01",sep = ""),
 format = "%Y-%m-%d")

deficit <- revenue-expenditure
deficit <- deficit %>% 
  mutate(date = yrdate)

deficit <- complete(deficit,
  date = seq(as.Date("1989-01-01"),
            as.Date("2019-11-01"), 
            by = "month")) %>%
       fill(2:195) 

#change in REER from 10 year moving average
exchange <- read.csv("exchange.csv") %>%
  as_tibble(.) %>%
  rename(date =  ï..date)

exchange$date <- as.Date(paste(exchange$date,"-01",sep = ""),
 format = "%Y-%m-%d")

exchangeMean <- na_ma(exchange, k = 120, weighting = "simple") %>% 
  as_tibble(.)

exchangeMean <- rollapply(exchangeMean[,2:120], width = 120,
  FUN = mean,
  by.column = TRUE,
  partial = TRUE,
  fill = NA) %>% 
  as_tibble(.) %>% 
  mutate(date =exchange$date) %>% 
  select(date, everything())

dateExchange <-exchange$date

exchange <- ((exchange -exchangeMean)/exchangeMean)*100
exchange <- as_tibble(exchange) %>% 
  mutate(date = dateExchange) %>% 
  select(date, everything())

#explanation of fxn: https://www.rdocumentation.org/packages/imputeTS/versions/1.5/topics/na.ma

#exchangeMean <- rollapply(data =exchange_zoo, 
#   width = 120,  FUN = function(x) mean(x, na.rm = TRUE),
#   by.column = TRUE,  partial = TRUE, fill = NA)  %>% 
#   as_tibble(.) method that didn't work

#exchangeMean <- rollmean(exchange[2:120], 120) %>%
#   as_tibble(.) %>% 
#   mutate(date =exchange$date) %>% 
#   select(date, everything()) also didn't work


#for %age change purposes
#exchange_xts <-exchange %>% 
#  tk_xts(silent = FALSE, date_var = date)

#exchange <- lag.xts(exchange_xts, k = 12) %>%
#  as_tibble(.) %>% 
#  mutate(date =exchange$date) %>%
#  select(date, everything())

#monthsDate <-exchange$date

#exchange <- (exchange-exchange_xts)/exchange_xts
#exchange <-exchange %>% 
#  mutate(date = monthsDate)

#month of reserves, months import cover
reserves <- read.csv("Reserves.csv") %>% 
  as_tibble(.) %>% 
  rename(date = ï..date)

reserves$date <- as.Date(paste(reserves$date,"-01",sep = ""),
 format = "%Y-%m-%d")

#income status
income <- read.csv("income.csv") %>% 
  as_tibble(.) %>% 
  rename(date = ï..)

income$date <- as.Date(paste(income$date,"-01-01",sep = ""),
 format = "%Y-%m-%d")

income <- complete(income,
  date = seq(as.Date("1987-01-01"),
            as.Date("2018-01-01"), 
            by = "month")) %>%
       fill(2:218) 

#openness
#source: WDI, indicator: API_NE.TRD.GNFS.ZS_DS2
openness <- read.csv("openness.csv") %>% 
  as_tibble(.) %>% 
  rename(date = ï..)

openness$date <- as.Date(paste(openness$date,"-01-01",sep = ""),
 format = "%Y-%m-%d")

openness <- complete(openness,
  date = seq(as.Date("1960-01-01"),
    as.Date("2018-12-30"),
    by = "month")) %>% 
  fill(2:242)

#credit ratings
ratings <- read.csv("ratings.csv", stringsAsFactors=FALSE, na.strings = c("", " ")) %>% 
  as_tibble(.) %>% 
  rename(Country = ï..Country) %>% 
  rename(date = Time) %>% 
  mutate(date = as.Date(date, format = "%m/%e/%Y")) %>% 
  arrange(Country, date) %>%
  group_by(Country) %>%
  fill(., 3:14, .direction = "down") %>%
  complete(.,
  date = seq(as.Date("1986-11-18"),
    as.Date("2017-09-06"),
    by = "day")) %>% 
  fill(., 3:14, .direction = "down") %>% 
  select(Country, date, Moody_LTF, SP_LTF, Fitch_LTF)

# ratings$Moody_LTF <- fct_explicit_na(ratings$Moody_LTF)
# ratings$SP_LTF <- fct_explicit_na(ratings$SP_LTF)
# ratings$Fitch_LTF <- fct_explicit_na(ratings$Fitch_LTF)

# ratingsMoody <- c("A1", "A2", "A3", "Aa3", "B1", "B2", "B3",
#  "Ba1", "Ba2", "Ba3", "baa1", "Baa1", "Baa2", "Baa3", "Ca",
#   "Caa1", "Caa2", "Caa3")

ratings$Moody_LTF <- ifelse(ratings$Moody_LTF == "A1", 5,
 ifelse(ratings$Moody_LTF == "A2", 6,
  ifelse(ratings$Moody_LTF == "A3", 7,
    ifelse(ratings$Moody_LTF == "Aa3", 4,
      ifelse(ratings$Moody_LTF == "B1", 14,
        ifelse(ratings$Moody_LTF == "B2", 15,
          ifelse(ratings$Moody_LTF == "B3", 16,
            ifelse(ratings$Moody_LTF == "Ba1", 11,
              ifelse(ratings$Moody_LTF == "Ba2", 12,
                ifelse(ratings$Moody_LTF == "Ba3", 13,
                  ifelse(ratings$Moody_LTF == "baa1", 8,
                    ifelse(ratings$Moody_LTF == "Baa1", 8,
                      ifelse(ratings$Moody_LTF == "Baa2", 9,
                        ifelse(ratings$Moody_LTF == "Baa3", 10,
                          ifelse(ratings$Moody_LTF == "Ca", 22,
                            ifelse(ratings$Moody_LTF == "Caa1", 17,
                              ifelse(ratings$Moody_LTF == "Caa2", 18,
                                ifelse(ratings$Moody_LTF == "Caa3", 19,NA))))))))))))))))))

ratings$SP_LTF <- ifelse(ratings$SP_LTF == "A",6,
 ifelse(ratings$SP_LTF == "A-", 7,
  ifelse(ratings$SP_LTF == "A+", 5,
    ifelse(ratings$SP_LTF == "AA-", 4,
      ifelse(ratings$SP_LTF == "B", 16,
        ifelse(ratings$SP_LTF == "B-", 17,
          ifelse(ratings$SP_LTF == "B+", 15,
            ifelse(ratings$SP_LTF == "BB", 12,
              ifelse(ratings$SP_LTF == "BB-", 13,
                ifelse(ratings$SP_LTF == "BB+", 11,
                  ifelse(ratings$SP_LTF == "BBB", 9,
                    ifelse(ratings$SP_LTF == "BBB-", 10,
                      ifelse(ratings$SP_LTF == "BBB+", 8,
                        ifelse(ratings$SP_LTF == "CC", 20,
                          ifelse(ratings$SP_LTF == "CCC", 18,
                            ifelse(ratings$SP_LTF == "CCC-", 19,
                              ifelse(ratings$SP_LTF == "CCC+", 17,
                                ifelse(ratings$SP_LTF == "SD", 22,NA))))))))))))))))))

ratings$Fitch_LTF <- ifelse(ratings$Fitch_LTF == "A", 6,
 ifelse(ratings$Fitch_LTF == "A-", 7,
  ifelse(ratings$Fitch_LTF == "A+", 5,
    ifelse(ratings$Fitch_LTF == "B", 15,
      ifelse(ratings$Fitch_LTF == "B-", 16,
        ifelse(ratings$Fitch_LTF == "B+", 14,
          ifelse(ratings$Fitch_LTF == "BB", 12,
            ifelse(ratings$Fitch_LTF == "BB-", 13,
              ifelse(ratings$Fitch_LTF == "BB+", 11,
                ifelse(ratings$Fitch_LTF == "BBB", 9,
                  ifelse(ratings$Fitch_LTF == "BBB-", 10,
                    ifelse(ratings$Fitch_LTF == "BBB+", 8,
                      ifelse(ratings$Fitch_LTF == "C", 21,
                        ifelse(ratings$Fitch_LTF == "CC", 20,
                          ifelse(ratings$Fitch_LTF == "CCC", 18,
                            ifelse(ratings$Fitch_LTF == "CCC-", 19,
                              ifelse(ratings$Fitch_LTF == "D", 24,
                                ifelse(ratings$Fitch_LTF == "DD", 23,
                                  ifelse(ratings$Fitch_LTF == "DDD", 22, NA)))))))))))))))))))

# levelsMoody <- c(5,6,7,4,14,15,16,11,12,13,
#   8,8,9,10,22,17,18,19,NA
#   )

# levelsMoody <- c("5", "6", "7", "4", "14", "15", "16", "11", "12", "13",
#  "8", "8", "9", "10", "22", "17", "18", "19"
#   )

# levels(ratings$SP_LTF) <- c(6,7,5,4,15,16,14,12,13,11,9,10,
#   8,20,18,19,17,22,NA
#   )

# levels(ratings$Fitch_LTF) <- c(6,7,5,15,16,14,12,13,11,9,10,
#   8,21,20,18,19,24,23,22,NA
#   )

ratings$Moody_LTF <- as.numeric(ratings$Moody_LTF, na.rm = FALSE)
ratings$SP_LTF <- as.numeric(ratings$SP_LTF, na.rm = FALSE)
ratings$Fitch_LTF <- as.numeric(ratings$Fitch_LTF, na.rm = FALSE)

ratings <- ratings %>%
  ungroup() %>% 
  mutate(rating = rowMeans(.[,3:5], na.rm = TRUE)) %>% 
  rename(country = Country) %>%
  select(country, date, rating) %>% 
  melt(., id = c("country", "date")) 

ratings <- ratings %>% 
  dcast(., date ~ country, mean, 
    value.var = "value") %>% 
  as_tibble(.)

# #Moody_LTF
# [1] "A1"   "A2"   "A3"   "Aa3"  "B1"   "B2"   "B3"   "Ba1" 
# [9] "Ba2"  "Ba3"  "baa1" "Baa1" "Baa2" "Baa3" "Ca"   "Caa1"
# [17] "Caa2" "Caa3"

# #SP_LTF
#  [1] "A"    "A-"   "A+"   "AA-"  "B"    "B-"   "B+"   "BB"  
#  [9] "BB-"  "BB+"  "BBB"  "BBB-" "BBB+" "CC"   "CCC"  "CCC-"
# [17] "CCC+" "SD" 

# #Fitch_LTF
#  [1] "A"    "A-"   "A+"   "B"    "B-"   "B+"   "BB"   "BB-" 
#  [9] "BB+"  "BBB"  "BBB-" "BBB+" "C"    "CC"   "CCC"  "CCC-"
# [17] "D"    "DD"   "DDD" 

#indicators
recessionIndicator <- c(as.Date("2001-01-01"),
                        as.Date("2001-02-01"),
                        as.Date("2001-03-01"),
                        as.Date("2001-04-01"),
                        as.Date("2001-05-01"),
                        as.Date("2001-06-01"),
                        as.Date("2001-07-01"),
                        as.Date("2001-08-01"),
                        as.Date("2001-09-01"),
                        as.Date("2007-10-01"),
                        as.Date("2007-11-01"),
                        as.Date("2007-12-01"),
                        as.Date("2008-01-01"),
                        as.Date("2008-02-01"),
                        as.Date("2008-03-01"),
                        as.Date("2008-04-01"),
                        as.Date("2008-05-01"),
                        as.Date("2008-06-01"),
                        as.Date("2008-07-01"),
                        as.Date("2008-08-01"),
                        as.Date("2008-09-01"),
                        as.Date("2008-10-01"),
                        as.Date("2008-11-01"),
                        as.Date("2008-12-01"),
                        as.Date("2009-01-01"),
                        as.Date("2009-02-01"),
                        as.Date("2009-03-01"),
                        as.Date("2009-04-01"),
                        as.Date("2009-05-01"),
                        as.Date("2009-06-01"))

greatIndicator <- c(as.Date("2007-10-01"),
                    as.Date("2007-11-01"),
                    as.Date("2007-12-01"),
                    as.Date("2008-01-01"),
                    as.Date("2008-02-01"),
                    as.Date("2008-03-01"),
                    as.Date("2008-04-01"),
                    as.Date("2008-05-01"),
                    as.Date("2008-06-01"),
                    as.Date("2008-07-01"),
                    as.Date("2008-08-01"),
                    as.Date("2008-09-01"),
                    as.Date("2008-10-01"),
                    as.Date("2008-11-01"),
                    as.Date("2008-12-01"),
                    as.Date("2009-01-01"),
                    as.Date("2009-02-01"),
                    as.Date("2009-03-01"),
                    as.Date("2009-04-01"),
                    as.Date("2009-05-01"),
                    as.Date("2009-06-01"))

assignName <- function(country){

  assign(paste0(tolower(country),"Bond"), sovereignDebt %>% 
    select(date, country) %>% 
    rename(spread = country) %>% 
    as_tibble(.), envir = .GlobalEnv)

  assign(paste0(tolower(country), "GDP"), GDP %>% 
    select(date, country) %>% 
    rename(gdp = country) %>% 
    as_tibble(.), envir = .GlobalEnv)

  assign(paste0(tolower(country), "CPI"), CPI %>% 
    select(date, country) %>% 
    rename(cpi = country) %>% 
    as_tibble(.), envir = .GlobalEnv)

  assign(paste0(tolower(country), "Debt"), debt %>% 
    select(date, country) %>% 
    rename(debt = country) %>% 
    as_tibble(.), envir = .GlobalEnv)

  assign(paste0(tolower(country), "Deficit"), deficit %>% 
    select(date, country) %>% 
    rename(deficit = country) %>% 
    as_tibble(.), envir = .GlobalEnv)

  assign(paste0(tolower(country), "Exchange"),exchange %>% 
    select(date, country) %>% 
    rename(exchange = country) %>% 
    as_tibble(.), envir = .GlobalEnv)

  assign(paste0(tolower(country), "Reserve"), reserves %>% 
    select(date, country) %>% 
    rename(reserve = country) %>% 
    as_tibble(.), envir = .GlobalEnv)

  assign(paste0(tolower(country), "Income"), income %>% 
    select(date, country) %>% 
    rename(income = country) %>% 
    as_tibble(.), envir = .GlobalEnv)

  assign(paste0(tolower(country), "Open"), openness %>% 
    select(date, country) %>% 
    rename(openness = country) %>% 
    as_tibble(.), envir = .GlobalEnv)

  assign(paste0(tolower(country), "Rating"), ratings %>% 
    select(date, country) %>% 
    rename(ratings = country) %>% 
    as_tibble(.), envir = .GlobalEnv)

}

genTab <- function(country){

  assign(tolower(country), 
    inner_join(eval(parse(text = paste0(tolower(country),"Bond"))),
       eval(parse(text = paste0(tolower(country), "GDP"))),
        by = "date") %>%
    inner_join(.,eval(parse(text = paste0(tolower(country), "CPI"))), 
        by = "date") %>% 
    inner_join(.,eval(parse(text = paste0(tolower(country),"Debt"))), 
        by = "date") %>% 
    inner_join(.,eval(parse(text = paste0(tolower(country),"Deficit"))),
        by = "date") %>% 
    inner_join(.,eval(parse(text = paste0(tolower(country),"Exchange"))), 
        by = "date") %>% 
    inner_join(.,eval(parse(text = paste0(tolower(country),"Reserve"))), 
        by = "date") %>%
    inner_join(.,eval(parse(text = paste0(tolower(country),"Income"))), 
        by = "date") %>%
    inner_join(.,eval(parse(text = paste0(tolower(country),"Open"))), 
        by = "date") %>%
    inner_join(.,eval(parse(text = paste0(tolower(country),"Rating"))), 
        by = "date") %>%
    inner_join(.,VIX, by = "date") %>% 
    inner_join(.,EFFR, by = "date") %>%
    inner_join(.,T10Y, by = "date") %>% 
    inner_join(.,T10YFF, by = "date") %>%
    mutate(recession = ifelse(date %in% recessionIndicator,
     1, 0)) %>%
    mutate(greatRecession = ifelse(date %in% greatIndicator, 1, 0)) %>%
    mutate(taper = ifelse(date > (as.Date("5/22/2013", 
              format = "%M/%d/%Y")), 1, 0)) %>% 
        as_tibble(.), envir = .GlobalEnv)

}

genReg <- function(country){

  assign(paste0(tolower(country), "Int"), lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + 
    deficit + exchange + openness + ratings + log(reserve) + log(VIX) + recession + 
    greatRecession + taper + log(EFFR)*recession +
    log(EFFR)*greatRecession + log(EFFR)*taper,
    data = eval(parse(text = tolower(country)))), envir = .GlobalEnv)

  assign(paste0(tolower(country), "FFR"), lm(log(spread) ~ log(EFFR) + gdp + cpi + debt +
   deficit + exchange + openness + ratings + log(reserve) + log(VIX),
   data = eval(parse(text = tolower(country)))), envir = .GlobalEnv)

#checks
  assign(paste0(tolower(country), "Robust"), lm_robust(log(spread) ~ log(EFFR) + gdp + cpi + debt +
   deficit + exchange + openness + ratings + log(reserve) + log(VIX),
   data = eval(parse(text = tolower(country)))), envir = .GlobalEnv)

  assign(paste0(tolower(country), "T10Y"), lm(log(spread) ~ log(T10Y) + gdp + cpi + debt +
   deficit + exchange + openness + ratings + log(reserve) + log(VIX),
   data = eval(parse(text = tolower(country)))), envir = .GlobalEnv)

  assign(paste0(tolower(country), "T10YFF"), lm(log(spread) ~ T10YFF + gdp + cpi + debt +
   deficit + exchange + openness + ratings + log(reserve) + log(VIX),
   data = eval(parse(text = tolower(country)))), envir = .GlobalEnv)

  assign(paste0(tolower(country), "Checks"), lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + 
    deficit + exchange + openness + ratings + log(reserve) + income + log(VIX) + recession + 
    greatRecession + taper + log(EFFR)*recession +
    log(EFFR)*greatRecession + log(EFFR)*taper + log(EFFR)*income,
    data = eval(parse(text = tolower(country)))),
    envir = .GlobalEnv)

}

genRegSpecial <- function(country){

 assign(paste0(tolower(country), "Int"), lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + 
    deficit + exchange + openness + ratings + log(reserve) + log(VIX) + recession + 
    greatRecession + taper + log(EFFR)*recession +
    log(EFFR)*greatRecession + log(EFFR)*taper,
    data = eval(parse(text = tolower(country)))), envir = .GlobalEnv)

  assign(paste0(tolower(country), "FFR"), lm(log(spread) ~ log(EFFR) + gdp + cpi + debt +
   deficit + exchange + openness + ratings + log(reserve) + log(VIX),
   data = eval(parse(text = tolower(country)))), envir = .GlobalEnv)

#checks
  assign(paste0(tolower(country), "Robust"), lm_robust(log(spread) ~ log(EFFR) + gdp + cpi + debt +
   deficit + exchange + openness + ratings + log(reserve) + log(VIX),
   data = eval(parse(text = tolower(country)))), envir = .GlobalEnv)

  assign(paste0(tolower(country), "T10Y"), lm(log(spread) ~ log(T10Y) + gdp + cpi + debt +
   deficit + exchange + openness + ratings + log(reserve) + log(VIX),
   data = eval(parse(text = tolower(country)))), envir = .GlobalEnv)

  assign(paste0(tolower(country), "T10YFF"), lm(log(spread) ~ T10YFF + gdp + cpi + debt +
   deficit + exchange + openness + ratings + log(reserve) + log(VIX),
   data = eval(parse(text = tolower(country)))), envir = .GlobalEnv)

  assign(paste0(tolower(country), "Checks"), lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + 
    deficit + exchange + openness + ratings + log(reserve) + log(VIX) + recession + 
    greatRecession + taper + log(EFFR)*recession +
    log(EFFR)*greatRecession + log(EFFR)*taper,
    data = eval(parse(text = tolower(country)))),
    envir = .GlobalEnv)
  
}

# countryList <- c("Brazil", "Russia", "China", "southAfrica", "Mexico",
#   "Indonesia", "Nigeria", "Turkey", "Argentina", "Chile", "Colombia", "Peru",
#   "Egypt","Hungary", "Malaysia", "Philippines", "Poland", "Vietnam")

countryList2 <- c("Brazil", "Russia", "China", "southAfrica", "Mexico",
  "Indonesia", "Turkey", "Argentina", "Chile", "Colombia", "Peru",
  "Egypt","Hungary", "Malaysia", "Philippines", "Poland")
# lapply(countryList, assignName)
lapply(countryList2, assignName)

# lapply(countryList, genTab)
lapply(countryList2, genTab)

specialList <- c("Mexico", "Indonesia", #"Nigeria",
 "Peru", "Egypt",
  "Malaysia", "Philippines")
otherList <- c("Brazil", "Russia", "China", "southAfrica", "Turkey",
  "Argentina", "Chile", "Colombia", "Hungary", "Poland" #"Vietnam"
  )

lapply(otherList, genReg)
lapply(specialList, genRegSpecial)

# assignName("Argentina")
# genTab("Argentina")
# genReg("Vietnam")

# lm(log(spread) ~ log(T10Y)
#   + gdp
#   + cpi
#   + debt
#   + deficit
#   + exchange
#   + log(reserve)
#   + log(VIX)
#   + recession
#   + greatRecession
#   + taper
#   #+ income
#   # + log(EFFR)*recession  
#   # + log(EFFR)*greatRecession 
#   # + log(EFFR)*taper
#   # + log(EFFR)*income
#  , data = mexico)

#all the countries together
# combo <- combine(argentina, chile, colombia, egypt, hungary, malaysia,
#   peru, philippines, poland, vietnam, brazil, russia, china, southafrica, mexico, 
#   indonesia, nigeria, turkey)

combo <- combine(argentina, chile, colombia, egypt, hungary, malaysia,
  peru, philippines, poland, brazil, russia, china, southafrica, mexico, 
  indonesia, turkey)

combo <- combo %>% 
  mutate(bricsmint = ifelse(source == "brazil" | source == "russia" |
    source == "china" | source == "southAfrica" | source == "mexico" |
    source == "indonesia" | source == "nigeria" | source == "turkey", 1, 0)) %>% 
  mutate(region = factor(ifelse(source == "brazil" | source == "mexico" |
    source == "argentina" | source == "chile" | source == "colombia" |
    source == "peru", "Latin America", ifelse(source == "russia" | 
      source == "southAfrica" | source == "nigeria" | source == "turkey" |
      source == "egypt" | source == "hungary", "EMEA", "Asia")))) %>%
  mutate(spread = spread + 0.1)

comboFFR_fixed <- plm(log(spread) ~ log(EFFR) + gdp + cpi + debt +
 deficit + exchange + openness + ratings + log(reserve) + log(VIX) + recession + 
 greatRecession + taper + bricsmint + source,
    data = combo,
    index = "source", 
    model = "within")

comboInt <- plm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit +
  exchange + openness + ratings + log(reserve) + log(VIX) + recession +
  greatRecession + taper + income + log(EFFR)*recession + 
  log(EFFR)*greatRecession + log(EFFR)*taper + log(EFFR)*income + bricsmint + source,
 data = combo,
 index = "source",
 model = "within")

comboInt_brics <- plm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit +
  exchange + openness + ratings + log(reserve) + log(VIX) + recession +
  greatRecession + taper + income + log(EFFR)*recession + 
  log(EFFR)*greatRecession + log(EFFR)*taper + log(EFFR)*income + bricsmint + source,
  data = filter(combo, bricsmint == 1),
  index = "source",
  model = "within")

comboInt_ode <- plm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit +
  exchange + openness + ratings + log(reserve) + log(VIX) + recession +
  greatRecession + taper + income + log(EFFR)*recession + 
  log(EFFR)*greatRecession + log(EFFR)*taper + log(EFFR)*income + bricsmint + source,
  data = filter(combo, bricsmint == 0),
  index = "source",
  model = "within")

comboIntRegion <- plm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit +
  exchange + openness + ratings + log(reserve) + log(VIX) + recession +
  greatRecession + taper + income + log(EFFR)*recession + 
  log(EFFR)*greatRecession + log(EFFR)*taper + log(EFFR)*income + region,
 data = combo,
 index = "source",
 model = "within")

#variable robustness checks

comboT <- plm(log(spread) ~ log(T10Y) + gdp + cpi + debt + deficit +
  exchange + openness + ratings + log(reserve) + log(VIX) + recession +
  greatRecession + taper + income + log(T10Y)*recession + 
  log(T10Y)*greatRecession + log(T10Y)*taper + log(T10Y)*income + region,
 data = combo,
 index = "source",
 model = "within")

comboTF <- plm(log(spread) ~ log(T10YFF) + gdp + cpi + debt + deficit +
  exchange + openness + ratings + log(reserve) + log(VIX) + recession +
  greatRecession + taper + income + log(T10YFF)*recession + 
  log(T10YFF)*greatRecession + log(T10YFF)*taper + log(T10YFF)*income + region,
 data = combo,
 index = "source",
 model = "within")

#time period robustness checks

comboAFC <- plm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit +
  exchange + openness + ratings + log(reserve) + log(VIX) + recession +
  greatRecession + taper + income + log(EFFR)*recession + 
  log(EFFR)*greatRecession + log(EFFR)*taper + log(EFFR)*income + region,
 data = filter(combo, date > "2000-01-01"),
 index = "source",
 model = "within")

comboPreGR <- plm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit +
  exchange + openness + ratings + log(reserve) + log(VIX) + recession +
  greatRecession + taper + income + log(EFFR)*recession + 
  log(EFFR)*greatRecession + log(EFFR)*taper + log(EFFR)*income + region,
 data = filter(combo, date < "2007-10-01"),
 index = "source",
 model = "within")

comboPostGR <- plm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit +
  exchange + openness + ratings + log(reserve) + log(VIX) + recession +
  greatRecession + taper + income + log(EFFR)*recession + 
  log(EFFR)*greatRecession + log(EFFR)*taper + log(EFFR)*income + region,
 data = filter(combo, date > "2009-06-01"),
 index = "source",
 model = "within")

#lag robustness checks
genLag <- function(country){

  assign(paste0(tolower(country), "Lag"),
    eval(parse(text = paste0(tolower(country)))) %>%
    tk_xts(., date_var = date), envir = .GlobalEnv)

  assign(paste0(tolower(country), "Lag1"), lag.xts(eval(parse(text = paste0(tolower(country), "Lag","[,2:16]"))), k = 1) %>% 
    as_tibble(.) %>% 
    mutate(date = eval(parse(text = paste0(tolower(country))))$date) %>% 
    mutate(spread = eval(parse(text = paste0(tolower(country))))$spread) %>% 
    select(date, spread, eval(parse(text = paste0("everything()")))),
    envir = .GlobalEnv)

  assign(paste0(tolower(country), "Lag2"), lag.xts(eval(parse(text = paste0(tolower(country), "Lag","[,2:16]"))), k = 3) %>% 
    as_tibble(.) %>% 
    mutate(date = eval(parse(text = paste0(tolower(country))))$date) %>% 
    mutate(spread = eval(parse(text = paste0(tolower(country))))$spread) %>% 
    select(date, spread, eval(parse(text = paste0("everything()")))),
    envir = .GlobalEnv)

  assign(paste0(tolower(country), "Lag3"), lag.xts(eval(parse(text = paste0(tolower(country), "Lag","[,2:16]"))), k = 6) %>% 
    as_tibble(.) %>% 
    mutate(date = eval(parse(text = paste0(tolower(country))))$date) %>% 
    mutate(spread = eval(parse(text = paste0(tolower(country))))$spread) %>% 
    select(date, spread, eval(parse(text = paste0("everything()")))),
    envir = .GlobalEnv)

  assign(paste0(tolower(country), "Lag4"), lag.xts(eval(parse(text = paste0(tolower(country), "Lag","[,2:16]"))), k = 12) %>% 
    as_tibble(.) %>% 
    mutate(date = eval(parse(text = paste0(tolower(country))))$date) %>% 
    mutate(spread = eval(parse(text = paste0(tolower(country))))$spread) %>% 
    select(date, spread, eval(parse(text = paste0("everything()")))),
    envir = .GlobalEnv)

  assign(paste0(tolower(country), "Lag5"), lag.xts(eval(parse(text = paste0(tolower(country), "Lag","[,2:16]"))), k = 24) %>% 
    as_tibble(.) %>% 
    mutate(date = eval(parse(text = paste0(tolower(country))))$date) %>% 
    mutate(spread = eval(parse(text = paste0(tolower(country))))$spread) %>% 
    select(date, spread, eval(parse(text = paste0("everything()")))),
    envir = .GlobalEnv)
}

lapply(countryList2, genLag)

comboLag1 <- combine(argentinaLag1, chileLag1, colombiaLag1, egyptLag1, hungaryLag1, malaysiaLag1,
  peruLag1, philippinesLag1, polandLag1, brazilLag1, russiaLag1, chinaLag1, southafricaLag1, mexicoLag1, 
  indonesiaLag1, turkeyLag1)

comboLag2 <- combine(argentinaLag2, chileLag2, colombiaLag2, egyptLag2, hungaryLag2, malaysiaLag2,
  peruLag2, philippinesLag2, polandLag2, brazilLag2, russiaLag2, chinaLag2, southafricaLag2, mexicoLag2, 
  indonesiaLag2, turkeyLag2)

comboLag3 <- combine(argentinaLag3, chileLag3, colombiaLag3, egyptLag3, hungaryLag3, malaysiaLag3,
  peruLag3, philippinesLag3, polandLag3, brazilLag3, russiaLag3, chinaLag3, southafricaLag3, mexicoLag3, 
  indonesiaLag3, turkeyLag3)

comboLag4 <- combine(argentinaLag4, chileLag4, colombiaLag4, egyptLag4, hungaryLag4, malaysiaLag4,
  peruLag4, philippinesLag4, polandLag4, brazilLag4, russiaLag4, chinaLag4, southafricaLag4, mexicoLag4, 
  indonesiaLag4, turkeyLag4)

comboLag5 <- combine(argentinaLag5, chileLag5, colombiaLag5, egyptLag5, hungaryLag5, malaysiaLag5,
  peruLag5, philippinesLag5, polandLag5, brazilLag5, russiaLag5, chinaLag5, southafricaLag5, mexicoLag5, 
  indonesiaLag5, turkeyLag5)

comboInt1 <- plm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit +
  exchange + openness + ratings + log(reserve) + log(VIX) + recession +
  greatRecession + taper + log(EFFR)*recession + 
  log(EFFR)*greatRecession + log(EFFR)*taper + source,
 data = comboLag1,
 index = "source",
 model = "within")

comboInt2 <- plm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit +
  exchange + openness + ratings + log(reserve) + log(VIX) + recession +
  greatRecession + taper + log(EFFR)*recession + 
  log(EFFR)*greatRecession + log(EFFR)*taper + source,
 data = comboLag2,
 index = "source",
 model = "within")

comboInt3 <- plm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit +
  exchange + openness + ratings + log(reserve) + log(VIX) + recession +
  greatRecession + taper + log(EFFR)*recession + 
  log(EFFR)*greatRecession + log(EFFR)*taper + source,
 data = comboLag3,
 index = "source",
 model = "within")

comboInt4 <- plm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit +
  exchange + openness + ratings + log(reserve) + log(VIX) + recession +
  greatRecession + taper + log(EFFR)*recession + 
  log(EFFR)*greatRecession + log(EFFR)*taper + source,
 data = comboLag4,
 index = "source",
 model = "within")

comboInt5 <- plm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit +
  exchange + openness + ratings + log(reserve) + log(VIX) + recession +
  greatRecession + taper + log(EFFR)*recession + 
  log(EFFR)*greatRecession + log(EFFR)*taper + source,
 data = comboLag5,
 index = "source",
 model = "within")

comboInt_special <- plm(log(spread) ~ log(EFFR) + gdp + cpi + debt +
 deficit + exchange + openness + ratings + log(reserve) + log(VIX) + recession + 
 greatRecession + taper + log(EFFR)*recession + 
  log(EFFR)*greatRecession + log(EFFR)*taper + source,
    data = combo,
    index = "source", 
    model = "within")