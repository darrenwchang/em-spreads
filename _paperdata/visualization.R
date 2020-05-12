#visualization.R
#Darren Chang
#AEM 4545 Final Paper, FA19
#Cornell University, Department of Economics

#setup
library(tidyverse)
library(ggthemes)
library(reshape2)
library(stargazer)
library(estimatr)
library(gdata)
library(table1)
library(magrittr)
library(corrplot)
library(plm)
library(fredr)
library(ggpubr)
setwd("C:/Users/darre/Documents/_Cornell 19-20/AEM 4545/_paperdata")
fredr_set_key("390e3981b7db01b1b43d340104fdd153")

#furniture::table1(combo, spread, EFFR, gdp, cpi, debt, deficit, exchange, reserve, VIX,
#  output = "latex2",
#  FUN2 = function(x){paste(median(x) [min(x), max(x)])}
#  second = c(spread, EFFR, gdp, cpi, debt, deficit, exchange, reserve, VIX)
#  )

#summary stats
table1::label(combo$spread) <- "EMBI+ Spreads"
table1::label(combo$gdp) <- "GDP Growth"
table1::label(combo$cpi) <- "CPI Inflation"
table1::label(combo$debt) <- "Debt/GDP Ratio"
table1::label(combo$deficit) <- "Deficit/GDP Ratio"
table1::label(combo$exchange) <- "REER"
table1::label(combo$reserve) <- "Reserves (months of import cover)"
table1::label(combo$VIX)  <- "VIX"
table1::label(combo$EFFR) <- "Federal Funds Rate"
table1::label(combo$openness) <- "Openness"
table1::label(combo$ratings) <- "Credit Rating"
table1::label(combo$T10Y) <- "T10Y"
table1::label(combo$T10YFF) <- "T10YFF"
 
table1::table1(~spread + EFFR + gdp + cpi + debt + deficit + 
  exchange + openness + ratings + reserve + VIX, 
  data = combo,
  transpose = TRUE )

###Correlation
comboCorr <- combo[,c(2:8, 10:13)]
corr <- round(cor(comboCorr,  use = "complete.obs"),2)
upper.tri(corr)
corr[upper.tri(corr)] <- NA
corr

combofind <- combo %>% 
  mutate(spread = log(spread)) %>% 
  mutate(EFFR = log(EFFR)) %>% 
  mutate(reserve = log(reserve)) %>% 
  mutate(VIX = log(VIX)) %>% 
  mutate(ratings = log(ratings))

combofindCorr <- combofind[,c(2:8, 10:13)]
corr2 <- round(cor(combofindCorr,  use = "complete.obs"),2)
upper.tri(corr)
corr2[upper.tri(corr)] <- NA
corr2

stargazer(corr,
  title = "Pairwise Correlations",
  font.size = "scriptsize",
  label = "tbl2")

#Hausman test
wi <- plm(log(spread) ~ log(EFFR) + gdp + cpi + debt +
 deficit + exchange + openness + ratings + log(reserve) + log(VIX) + recession + 
 greatRecession + taper,
    data = combo,
    index = "source", 
    model = "within")

re <- plm(log(spread) ~ log(EFFR) + gdp + cpi + debt +
 deficit + exchange + openness + ratings + log(reserve) + log(VIX) + recession + 
 greatRecession + taper,
    data = combo,
    index = "source", 
    model = "random")

phtest(wi, re)

####TABLES
stargazer(comboFFR_fixed, comboInt, comboInt_brics, comboInt_ode,
  title = "Fixed Effects Estimation of Sovereign Bond Spreads",
  omit = "source",
  dep.var.labels = "log(EMBI+ Spreads)",
  no.space = TRUE,
  covariate.labels=c("log(EFFR)",
    "GDP Growth",
    "CPI Inflation",
    "Debt to GDP Ratio",
    "Deficit to GDP Ratio",
    "REER",
    "Openness",
    "Credit Rating",
    "log(Reserves)",
    "log(VIX)",
    "Recession Dummy",
    "Great Recession Dummy",
    "Taper Dummy",
    "Lower Middle Income Dummy",
    "Upper Middle Income Dummy",
    "Lower Income Dummy",
    "log(EFFR)*Recession",
    "log(EFFR)*Great Recession",
    "log(EFFR)*Taper",
    "log(EFFR)*Lower Middle Income",
    "log(EFFR)*Upper Middle Income",
    "log(EFFR)*Lower Income"
    ),
  omit.stat = "f",
  notes = "Standard errors are in parentheses. Source: author's calculations.",
  notes.align = "l",
  font.size = "small",
  label = "tbl3"
  )

#8 countries
# stargazer(brazilFFR, russiaFFR, chinaFFR, southAfricaFFR, mexicoFFR, 
#   indonesiaFFR, nigeriaFFR, turkeyFFR, 
#   title = "Sovereign Bond Spreads of BRICS and MINT Economies", 
#   column.labels=c("Brazil","Russia", "China", "South Africa", "Mexico", "Indonesia", "Nigeria","Turkey"),
#   covariate.labels=c("Federal Funds Rate","GDP Growth","CPI Inflation","Debt to GDP Ratio",
#     "Deficit to GDP Ratio","Real Effective Exchange Rate",
#     "Total Reserves (Months of Imports)", 
#     "CBOE Volatility Index"),
#   omit.stat=c("ser","f"),
#   dep.var.labels = "Country",
#   no.space=TRUE
#   )

#10 countries
# stargazer(argentinaFFR, chileFFR,  colombiaFFR, egyptFFR, hungaryFFR, malaysiaFFR,
#   peruFFR, philippinesFFR, polandFFR, vietnamFFR, 
#   title = "Sovereign Bond Spreads of Other Emerging/Developing Market Economies",
#   column.labels = c("Argentina","Chile","Colombia","Egypt","Hungary",
#     "Malaysia","Peru","Philippines","Poland","Vietnam"),
#   covariate.labels=c("Federal Funds Rate","GDP Growth","CPI Inflation","Debt to GDP Ratio",
#     "Deficit to GDP Ratio","Real Effective Exchange Rate",
#     "Total Reserves (Months of Imports)", 
#     "CBOE Volatility Index"),
#   omit.stat=c("ser","f"),
#   dep.var.labels = "JP Morgan Emerging Markets Bond Spread",
#   no.space=TRUE
#   )

#8 + interaction
stargazer(brazilInt, russiaInt, chinaInt, southafricaInt, mexicoInt, 
  indonesiaInt, turkeyInt, 
  title = "Sovereign Bond Spreads of BRICS and MINT Economies with Interactions", 
  column.labels=c("Brazil","Russia", "China", "South Africa",
   "Mexico", "Indonesia","Turkey"),
  covariate.labels=c("log(EFFR)",
    "GDP Growth",
    "CPI Inflation",
    "Debt to GDP Ratio",
    "Deficit to GDP Ratio",
    "REER",
    "Openness",
    "Credit Rating",
    "log(Reserves)",
    "log(VIX)",
    "Recession Dummy",
    "Great Recession Dummy",
    "Taper Dummy",
    "log(EFFR)*Recession",
    "log(EFFR)*Great Recession",
    "log(EFFR)*Taper"),
  omit.stat=c("ser","f"),
  dep.var.labels = "log(EMBI+ Spreads), by Country",
  no.space=TRUE,
  notes = "Standard errors are in parentheses. Source: author's calculations.",
  notes.align = "l",
  font.size = "scriptsize",
  label = "tbl4"
  )

#10 + interaction
stargazer(argentinaInt, chileInt,  colombiaInt, egyptInt, hungaryInt, malaysiaInt,
  peruInt, philippinesInt, polandInt, 
  title = "Sovereign Bond Spreads of Other Emerging/Developing Market Economies with Interactions",
  column.labels = c("Argentina","Chile","Colombia","Egypt","Hungary",
    "Malaysia","Peru","Philippines","Poland"),
  covariate.labels=c("log(EFFR)",
    "GDP Growth",
    "CPI Inflation",
    "Debt to GDP Ratio",
    "Deficit to GDP Ratio",
    "REER",
    "Openness",
    "Credit Rating",
    "log(Reserves)",
    "log(VIX)",
    "Recession Dummy",
    "Great Recession Dummy",
    "Taper Dummy",
    "log(EFFR)*Recession",
    "log(EFFR)*Great Recession",
    "log(EFFR)*Taper"),
  omit.stat=c("ser","f"),
  dep.var.labels = "log(EMBI+ Spreads), by Country",
  no.space=TRUE,
  notes = "Standard errors are in parentheses. Source: author's calculations.",
  notes.align = "l",
  font.size = "scriptsize",
  label = "tbl5"
  )

#robustness check w different indep vars
stargazer(comboInt, comboT, comboTF,
  title = "Variable Robustness Checks for Fixed Effects Estimation of Sovereign Bond Spreads",
  omit = "source",
  dep.var.labels = "log(EMBI+ Spreads)",
  no.space = TRUE,
  covariate.labels=c("log(EFFR)",
    "log(T10Y)",
    "log(T10YFF)",
    "GDP Growth",
    "CPI Inflation",
    "Debt to GDP Ratio",
    "Deficit to GDP Ratio",
    "REER",
    "Openness",
    "Credit Rating",
    "log(Reserves)",
    "log(VIX)",
    "Recession Dummy",
    "Great Recession Dummy",
    "Taper Dummy",
    "Lower Middle Income Dummy",
    "Upper Middle Income Dummy",
    "Lower Income Dummy",
    "log(EFFR)*Recession",
    "log(EFFR)*Great Recession",
    "log(EFFR)*Taper",
    "log(EFFR)*Lower Middle Income",
    "log(EFFR)*Upper Middle Income",
    "log(EFFR)*Lower Income",
    "log(T10Y)*Recession",
    "log(T10Y)*Great Recession",
    "log(T10Y)*Taper",
    "log(T10Y)*Lower Middle Income",
    "log(T10Y)*Upper Middle Income",
    "log(T10Y)*Lower Income",
    "log(T10YFF)*Recession",
    "log(T10YFF)*Great Recession",
    "log(T10YFF)*Taper",
    "log(T10YFF)*Lower Middle Income",
    "log(T10YFF)*Upper Middle Income",
    "log(T10YFF)*Lower Income"
    ),
  omit.stat = "f",
  notes = c("T10Y is the 10 year U.S. Treasury Bond Yield,", 
  "and T10YFF is the 10 year U.S. Treasury Bond",
  "Yield and Federal Funds Rate Spread.",
  "Standard errors are in parentheses.",
  "Source: author's calculations."),
  notes.align = "l",
  font.size = "tiny",
  label = "tbl7"
  )

#robustness check for time
stargazer(comboInt, comboAFC, comboPreGR, comboPostGR,
  title = "Time Robustness Checks Fixed Effects Estimation of Sovereign Bond Spreads",
  omit = "source",
  dep.var.labels = "log(EMBI+ Spreads)",
  no.space = TRUE,
  covariate.labels=c("log(EFFR)",
    "GDP Growth",
    "CPI Inflation",
    "Debt to GDP Ratio",
    "Deficit to GDP Ratio",
    "REER",
    "Openness",
    "Credit Rating",
    "log(Reserves)",
    "log(VIX)",
    "Recession Dummy",
    "Great Recession Dummy",
    "Taper Dummy",
    "Lower Middle Income Dummy",
    "Upper Middle Income Dummy",
    "Lower Income Dummy",
    "log(EFFR)*Recession",
    "log(EFFR)*Great Recession",
    "log(EFFR)*Taper",
    "log(EFFR)*Lower Middle Income",
    "log(EFFR)*Upper Middle Income",
    "log(EFFR)*Lower Income"
    ),
  omit.stat = "f",
  notes = c("Specification (1) is the original fixed effects model.",
    "Specification (2) estimates the model after the Asian Financial Crisis.",
    "Specification (3) estimates the model before the Great Recession.",
    "Specification (4) estimates the model after the Great Recession.",
    "Standard errors are in parentheses.",
    "Source: author's calculations."),
  notes.align = "l",
  font.size = "scriptsize",
  label = "tbl8"
  )

#robustness check for lag

stargazer(comboInt_special, comboInt1, comboInt2, comboInt3, comboInt4, comboInt5,
  title = "Lag Robustness Checks Fixed Effects Estimation of Sovereign Bond Spreads",
  omit = "source",
  dep.var.labels = "log(EMBI+ Spreads)",
  no.space = TRUE,
  covariate.labels=c("log(EFFR)",
    "GDP Growth",
    "CPI Inflation",
    "Debt to GDP Ratio",
    "Deficit to GDP Ratio",
    "REER",
    "Openness",
    "Credit Rating",
    "log(Reserves)",
    "log(VIX)",
    "Recession Dummy",
    "Great Recession Dummy",
    "Taper Dummy",
    "log(EFFR)*Recession",
    "log(EFFR)*Great Recession",
    "log(EFFR)*Taper"
    ),
  omit.stat = "f",
  notes = c("Specification (1) is the original fixed effects model.",
    "Specification (2) estimates the model with a 1 month lag.",
    "Specification (3) esimates the model with a 3 month lag.",
    "Specification (4) esimates the model with a 6 month lag.",
    "Specification (5) esimates the model with a 1 year lag.",
    "Specification (6) esimates the model with a 2 year lag.",
    "Standard errors are in parentheses.",
    "Source: author's calculations."),
  notes.align = "l",
  font.size = "scriptsize",
  label = "tbl9"
  )

####PLOTS
FFRm <- fredr_series_observations(
  series_id = "DFF",
  frequency = "m"
  )

AAA10Y <- fredr_series_observations(
  series_id = "AAA10Y",
  frequency = "m"
)
EM_Asia_Spread <- fredr_series_observations(
  series_id = "BAMLEMRACRPIASIAOAS",
  #frequency = "d"
  frequency = "m"
)
EM_EMEA_Spread <- fredr_series_observations(
  series_id = "BAMLEMRECRPIEMEAOAS",
  #frequency = "d"
  frequency = "m"
)

T1 <- rbind(FFRm, AAA10Y, EM_Asia_Spread, EM_EMEA_Spread)
pct <- scales::percent_format(accuracy = 1, scale = 1, suffix = "%", decimal.mark = ".", trim = TRUE)
lab <- scales::date_format(format = "%Y")

gg1 <- ggplot(filter(T1, date > as.Date("2000-07-03",
 format = "%Y-%m-%d")),
 mapping = aes(x = date, y = value, color = series_id,
  linetype = series_id))+
  geom_line()+
  theme_fivethirtyeight()+
  scale_y_continuous(labels = pct)+
  scale_x_date(breaks = "2 years", labels = lab)+
  labs(#title = "Global Effects of U.S. Central Banking",
       subtitle = "Regional Differences in Yield Spreads",
       caption = "Source: FRED, Moody's, and ICE Benchmark Administration Limited"
       )+
  scale_color_hue(name = "",
    labels = c("Moody's AAA Corporate Bond - 10 Year Treasury Spread",
   "Yield Spread: Asia",
   "Yield Spread: Europe, Middle East, and Africa (EMEA)",
   "Fed Funds Rate"))+
  scale_linetype_manual(values = c(1,2,3,4), name = "",labels = c("Moody's AAA Corporate Bond - 10 Year Treasury Spread",
   "Yield Spread: Asia", "Yield Spread: Europe, Middle East, and Africa (EMEA)", "Fed Funds Rate"))+
  theme(plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 10),
        legend.direction= "vertical",
        legend.key = element_rect(fill = "white"),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"))
  gg1

ggsave("globalfx.png", plot = gg1, device = png(), 
  path = "C:/Users/darre/Documents/_Cornell 19-20/AEM 4545/_paper", 
  width = 5.5, height = 4.5, units = "in")

###plot for DFF, DGS10, T10YFF
FFR <- fredr_series_observations(
  series_id = "DFF",
  frequency = "d"
  #frequency = "m"
  )

#10yr
T10Y <- fredr_series_observations(
  series_id = "DGS10",
  frequency = "d"
)

#10yr-FFR
T10YFF <- fredr_series_observations(
  series_id = "T10YFF",
  frequency = "d"
)

T10Ym <- fredr_series_observations(
  series_id = "DGS10",
  frequency = "m"
)

#10yr-FFR
T10YFFm <- fredr_series_observations(
  series_id = "T10YFF",
  frequency = "m"
)

T2 <- rbind(FFRm, T10Ym, T10YFFm)

gg2 <- ggplot(filter(T2, date > as.Date("1997-01-01",
 format = "%Y-%m-%d")),
 mapping = aes(x = date, y = value, color = series_id,
  linetype = series_id))+
  geom_line()+
  theme_fivethirtyeight()+
  scale_y_continuous(labels = pct)+
  scale_x_date(breaks = "2 years", labels = lab)+
  labs(#title = "Comparing Different Measures of U.S. Central Banking",
       #subtitle = "Regional Differences in Yield Spreads",
       caption = "Source: FRED"
       )+
  scale_color_manual(name = "",
    values = c("red", "blue", "green"),
    labels = c("Federal Funds Rate","T10Y","T10YFF")
    )+
   scale_linetype_stata(name = "",
        labels = c("Federal Funds Rate","T10Y","T10YFF")
     )+
  theme(plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 10),
        # legend.direction= "vertical", 
        # legend.position = "bottom",
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"))
  gg2

names(sovereignDebt) <- tolower(names(sovereignDebt))

T3 <- sovereignDebt %>%
  select(date, argentina, chile, colombia, egypt, hungary, malaysia,
  peru, philippines, poland, vietnam, brazil, russia, china, southafrica, mexico, 
  indonesia, nigeria, turkey) %>% 
  mutate(mean = rowMeans(.[2:19], na.rm = TRUE)) %>% 
  mutate(type1 = "mean") %>% 
  mutate(min = apply(.[2:19], 1, FUN = min, na.rm = TRUE)) %>% 
  mutate(type2 = "min") %>% 
  mutate(max = apply(.[2:19], 1, FUN = max, na.rm = TRUE)) %>% 
  mutate(type3 = "max")

debtmean <- T3 %>% 
  select(date, mean, type1) %>% 
  rename(series_id = type1) %>% 
  rename(value = mean)

debtmin <- T3 %>% 
  select(date, min, type2) %>% 
  rename(series_id = type2) %>% 
  rename(value = min)

debtmax <- T3 %>% 
  select(date, max, type3) %>% 
  rename(series_id = type3) %>% 
  rename(value = max)

T4 <- rbind(debtmean, debtmin, debtmax)

T5 <- T3 %>% 
  select(date, mean, min, max) %>% 
  inner_join(., FFRm, by = "date") %>% 
  rename(FFR = value) %>% 
  select(-series_id)

gg3 <- ggplot(filter(T5, date > "2000-01-01" & date < "2007-01-01"), aes(x = date))+
  theme_fivethirtyeight()+
  geom_line(aes(y = mean, color = "embi", linetype = "1"))+
  geom_line(aes(y = FFR, color = "ffr", linetype = "2"))+
  geom_ribbon(aes(ymin = min, ymax = max), fill = "gray80", alpha = 0.4)+
  scale_y_continuous(labels = pct)+
  scale_x_date(breaks = "2 years", labels = lab)+
  scale_color_manual(name="",
    values = c(embi = "blue", ffr = "red"),
    labels = c("Bond Spread", "Fed Funds Rate"))+
  scale_linetype_manual(name = "",
    values = c(1,2),
    labels = c("Bond Spread", "Fed Funds Rate"))+
  labs(#title = "EMBI+ Bond Spreads",
       subtitle = #"Shaded region denotes range
       "\nPre-Financial Crisis",
       caption = "",
       color = ""
       )+
  theme(plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 10),
        legend.key = element_rect(fill = "white"),
        #legend.direction= "vertical", 
        #legend.position = "bottom",
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"))+
  coord_cartesian(ylim = c(0, 35))
gg3

gg4 <- ggplot(filter(T5, date > "2006-12-30"), aes(x = date))+
  theme_fivethirtyeight()+
  geom_line(aes(y = mean, color = "embi", linetype = "1"))+
  geom_line(aes(y = FFR, color = "ffr", linetype = "2"))+
  geom_ribbon(aes(ymin = min, ymax = max), fill = "gray80", alpha = 0.4)+
  scale_y_continuous(labels = pct)+
  scale_x_date(breaks = "2 years", labels = lab)+
  scale_color_manual(name="",
    values = c(embi = "blue", ffr = "red"),
    labels = c("Bond Spread", "Fed Funds Rate"))+
  scale_linetype_manual(name = "",
    values = c(1,2),
    labels = c("Bond Spread", "Fed Funds Rate"))+
  labs(#title = "EMBI+ Bond Spreads",
       subtitle = "\nPost-Financial Crisis",
       caption = "J.P. Morgan and FRED",
       color = ""
       )+
  theme(plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 10),
        legend.key = element_rect(fill = "white"),
        #legend.direction= "vertical", 
        #legend.position = "bottom",
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"))+
  coord_cartesian(ylim = c(0, 35))
gg4

gg5 <- ggplot(filter(T5, date < "2000-01-01"), aes(x = date))+
  theme_fivethirtyeight()+
  geom_line(aes(y = mean, color = "embi", linetype = "1"))+
  geom_line(aes(y = FFR, color = "ffr", linetype = "2"))+
  geom_ribbon(aes(ymin = min, ymax = max), fill = "gray80", alpha = 0.4)+
  scale_y_continuous(labels = pct)+
  scale_x_date(breaks = "2 years", labels = lab)+
  scale_color_manual(name="",
    values = c(embi = "blue", ffr = "red"),
    labels = c("Bond Spread", "Fed Funds Rate"))+
  scale_linetype_manual(name = "",
    values = c(1,2),
    labels = c("Bond Spread", "Fed Funds Rate"))+
  labs(#title = "EMBI+ Bond Spreads",
       subtitle = "\nAsian Financial Crisis",
       caption = "",
       color = ""
       )+
  theme(plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 10),
        legend.key = element_rect(fill = "white"),
        #legend.direction= "vertical", 
        #legend.position = "bottom",
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"))+
  coord_cartesian(ylim = c(0, 35))
gg5

gg6 <- ggarrange(gg5, gg3, gg4, nrow = 1)
gg6

ggsave("time.png", plot = gg6, device = png(), 
  path = "C:/Users/darre/Documents/_Cornell 19-20/AEM 4545/_paper", 
  width = 9.5, height = 5, units = "in")