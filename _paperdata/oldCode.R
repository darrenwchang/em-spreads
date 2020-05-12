##Argentina = ARG
argentinaBond <- sovereignDebt %>% 
  select(date, Argentina) %>%
  rename(spread = Argentina) %>% 
  as_tibble(.)

argentinaGDP <- GDP %>% 
  select(date, Argentina) %>% 
  rename(gdp = Argentina) %>% 
  as_tibble(.)

argentinaCPI <- CPI %>% 
  select(date, Argentina) %>% 
  rename(cpi = Argentina) %>% 
  as_tibble(.)

argentinaDebt <- debt %>% 
  select(date, Argentina) %>% 
  rename(debt = Argentina) %>% 
  as_tibble(.)

argentinaDeficit <- deficit %>% 
  select(date, Argentina) %>% 
  rename(deficit = Argentina) %>% 
  as_tibble(.)

argentinaExchange <- exchange %>% 
  select(date, Argentina) %>% 
  rename(exchange = Argentina) %>% 
  as_tibble(.)

argentinaReserve <- reserves %>% 
  select(date, Argentina) %>% 
  rename(reserve = Argentina) %>% 
  as_tibble(.)

argentina <- inner_join(argentinaBond, argentinaGDP,
 by = "date") %>%
  inner_join(.,argentinaCPI, by = "date") %>% 
  inner_join(.,argentinaDebt, by = "date") %>% 
  inner_join(.,argentinaDeficit, by = "date") %>% 
  inner_join(.,argentinaExchange, by = "date") %>% 
  inner_join(.,argentinaReserve, by = "date") %>%
  inner_join(.,VIX, by = "date") %>% 
  inner_join(.,EFFR, by = "date") %>%
  inner_join(.,T10Y, by = "date") %>% 
  inner_join(.,T10YFF, by = "date") %>%
  mutate(recession = ifelse(date %in% recessionIndicator,
   1, 0)) %>%
  mutate(greatRecession = ifelse(date %in% greatIndicator, 1, 0)) %>%
  mutate(taper = ifelse(date > (as.Date("5/22/2013", 
            format = "%M/%d/%Y")), 1, 0)) %>% 
  as_tibble(.)

argentinaInt <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX) + recession + greatRecession + taper +
  log(EFFR)*recession + log(EFFR)*greatRecession + log(EFFR)*taper,
 data = argentina)

argentinaFFR <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = argentina)

argentinaRobust <- lm_robust(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = argentina)

argentinaT10Y <- lm(log(spread) ~ log(T10Y) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = argentina)

argentinaT10YFF <- lm(log(spread) ~ T10YFF + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = argentina)

##Brazil = BRA
brazilBond <- sovereignDebt %>% 
  select(date, Brazil) %>%
  rename(spread = Brazil) %>% 
  as_tibble(.)

brazilGDP <- GDP %>% 
  select(date, Brazil) %>% 
  rename(gdp = Brazil) %>% 
  as_tibble(.)

brazilCPI <- CPI %>% 
  select(date, Brazil) %>% 
  rename(cpi = Brazil) %>% 
  as_tibble(.)

brazilDebt <- debt %>% 
  select(date, Brazil) %>% 
  rename(debt = Brazil) %>% 
  as_tibble(.)

brazilDeficit <- deficit %>% 
  select(date, Brazil) %>% 
  rename(deficit = Brazil) %>% 
  as_tibble(.)

brazilExchange <- exchange %>% 
  select(date, Brazil) %>% 
  rename(exchange = Brazil) %>% 
  as_tibble(.)

brazilReserve <- reserves %>% 
  select(date, Brazil) %>% 
  rename(reserve = Brazil) %>% 
  as_tibble(.)

brazil <- inner_join(brazilBond, brazilGDP,
 by = "date") %>%
  inner_join(.,brazilCPI, by = "date") %>% 
  inner_join(.,brazilDebt, by = "date") %>% 
  inner_join(.,brazilDeficit, by = "date") %>% 
  inner_join(.,brazilExchange, by = "date") %>% 
  inner_join(.,brazilReserve, by = "date") %>%
  inner_join(.,VIX, by = "date") %>% 
  inner_join(.,EFFR, by = "date") %>%
  inner_join(.,T10Y, by = "date") %>% 
  inner_join(.,T10YFF, by = "date") %>%
  mutate(recession = ifelse(date %in% recessionIndicator,
   1, 0)) %>%
  mutate(greatRecession = ifelse(date %in% greatIndicator, 1, 0)) %>%
  mutate(taper = ifelse(date > (as.Date("5/22/2013", 
            format = "%M/%d/%Y")), 1, 0)) %>% 
  as_tibble(.)

brazilInt <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX) + recession + greatRecession + taper +
  log(EFFR)*recession + log(EFFR)*greatRecession + log(EFFR)*taper,
 data = brazil)

brazilFFR <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = brazil)

brazilRobust <- lm_robust(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = brazil)

brazilT10Y <- lm(log(spread) ~ log(T10Y) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = brazil)

brazilT10YFF <- lm(log(spread) ~ T10YFF + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = brazil)

#China = CHN
chinaBond <- sovereignDebt %>% 
  select(date, China) %>%
  rename(spread = China) %>% 
  as_tibble(.)

chinaGDP <- GDP %>% 
  select(date, China) %>% 
  rename(gdp = China) %>% 
  as_tibble(.)

chinaCPI <- CPI %>% 
  select(date, China) %>% 
  rename(cpi = China) %>% 
  as_tibble(.)

chinaDebt <- debt %>% 
  select(date, China) %>% 
  rename(debt = China) %>% 
  as_tibble(.)

chinaDeficit <- deficit %>% 
  select(date, China) %>% 
  rename(deficit = China) %>% 
  as_tibble(.)

chinaExchange <- exchange %>% 
  select(date, China) %>% 
  rename(exchange = China) %>% 
  as_tibble(.)

chinaReserve <- reserves %>% 
  select(date, China) %>% 
  rename(reserve = China) %>% 
  as_tibble(.)

china <- inner_join(chinaBond, chinaGDP,
 by = "date") %>%
  inner_join(.,chinaCPI, by = "date") %>% 
  inner_join(.,chinaDebt, by = "date") %>% 
  inner_join(.,chinaDeficit, by = "date") %>% 
  inner_join(.,chinaExchange, by = "date") %>% 
  inner_join(.,chinaReserve, by = "date") %>%
  inner_join(.,VIX, by = "date") %>% 
  inner_join(.,EFFR, by = "date") %>%
  inner_join(.,T10Y, by = "date") %>% 
  inner_join(.,T10YFF, by = "date") %>% 
  mutate(recession = ifelse(date %in% recessionIndicator,
   1, 0)) %>%
  mutate(greatRecession = ifelse(date %in% greatIndicator, 1, 0)) %>%
  mutate(taper = ifelse(date > (as.Date("5/22/2013", 
            format = "%M/%d/%Y")), 1, 0)) %>% 
  as_tibble(.)

chinaInt <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX) + recession + greatRecession + taper +
  log(EFFR)*recession + log(EFFR)*greatRecession + log(EFFR)*taper,
 data = china)

chinaFFR <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = china)

chinaRobust <- lm_robust(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = china)

chinaT10Y <- lm(log(spread) ~ log(T10Y) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = china)

chinaT10YFF <- lm(log(spread) ~ T10YFF + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = china)

#Indonesia = IDN
indonesiaBond <- sovereignDebt %>% 
  select(date, Indonesia) %>%
  rename(spread = Indonesia) %>%
  mutate(spread = spread + 0.1) %>% 
  as_tibble(.)

indonesiaGDP <- GDP %>% 
  select(date, Indonesia) %>% 
  rename(gdp = Indonesia) %>% 
  as_tibble(.)

indonesiaCPI <- CPI %>% 
  select(date, Indonesia) %>% 
  rename(cpi = Indonesia) %>% 
  as_tibble(.)

indonesiaDebt <- debt %>% 
  select(date, Indonesia) %>% 
  rename(debt = Indonesia) %>% 
  as_tibble(.)

indonesiaDeficit <- deficit %>% 
  select(date, Indonesia) %>% 
  rename(deficit = Indonesia) %>% 
  as_tibble(.)

indonesiaExchange <- exchange %>% 
  select(date, Indonesia) %>% 
  rename(exchange = Indonesia) %>% 
  as_tibble(.)

indonesiaReserve <- reserves %>% 
  select(date, Indonesia) %>% 
  rename(reserve = Indonesia) %>% 
  as_tibble(.)

indonesia <- inner_join(indonesiaBond, indonesiaGDP,
 by = "date") %>%
  inner_join(.,indonesiaCPI, by = "date") %>% 
  inner_join(.,indonesiaDebt, by = "date") %>% 
  inner_join(.,indonesiaDeficit, by = "date") %>% 
  inner_join(.,indonesiaExchange, by = "date") %>% 
  inner_join(.,indonesiaReserve, by = "date") %>%
  inner_join(.,VIX, by = "date") %>% 
  inner_join(.,EFFR, by = "date") %>%
  inner_join(.,T10Y, by = "date") %>% 
  inner_join(.,T10YFF, by = "date") %>% 
  mutate(recession = ifelse(date %in% recessionIndicator,
   1, 0)) %>%
  mutate(greatRecession = ifelse(date %in% greatIndicator, 1, 0)) %>%
  mutate(taper = ifelse(date > (as.Date("5/22/2013", 
            format = "%M/%d/%Y")), 1, 0)) %>% 
  as_tibble(.)

indonesiaInt <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX) + recession + greatRecession + taper +
  log(EFFR)*recession + log(EFFR)*greatRecession + log(EFFR)*taper,
 data = indonesia)

indonesiaFFR <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = indonesia)

indonesiaRobust <- lm_robust(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = indonesia)

indonesiaT10Y <- lm(log(spread) ~ log(T10Y) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = indonesia)

indonesiaT10YFF <- lm(log(spread) ~ T10YFF + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = indonesia)

#Mexico = MEX
mexicoBond <- sovereignDebt %>% 
  select(date, Mexico) %>%
  rename(spread = Mexico) %>% 
  as_tibble(.)

mexicoGDP <- GDP %>% 
  select(date, Mexico) %>% 
  rename(gdp = Mexico) %>% 
  as_tibble(.)

mexicoCPI <- CPI %>% 
  select(date, Mexico) %>% 
  rename(cpi = Mexico) %>% 
  as_tibble(.)

mexicoDebt <- debt %>% 
  select(date, Mexico) %>% 
  rename(debt = Mexico) %>% 
  as_tibble(.)

mexicoDeficit <- deficit %>% 
  select(date, Mexico) %>% 
  rename(deficit = Mexico) %>% 
  as_tibble(.)

mexicoExchange <- exchange %>% 
  select(date, Mexico) %>% 
  rename(exchange = Mexico) %>% 
  as_tibble(.)

mexicoReserve <- reserves %>% 
  select(date, Mexico) %>% 
  rename(reserve = Mexico) %>% 
  as_tibble(.)

mexico <- inner_join(mexicoBond, mexicoGDP,
 by = "date") %>%
  inner_join(.,mexicoCPI, by = "date") %>% 
  inner_join(.,mexicoDebt, by = "date") %>% 
  inner_join(.,mexicoDeficit, by = "date") %>% 
  inner_join(.,mexicoExchange, by = "date") %>% 
  inner_join(.,mexicoReserve, by = "date") %>%
  inner_join(.,VIX, by = "date") %>% 
  inner_join(.,EFFR, by = "date") %>%
  inner_join(.,T10Y, by = "date") %>% 
  inner_join(.,T10YFF, by = "date") %>% 
  mutate(recession = ifelse(date %in% recessionIndicator,
   1, 0)) %>%
  mutate(greatRecession = ifelse(date %in% greatIndicator, 1, 0)) %>%
  mutate(taper = ifelse(date > (as.Date("5/22/2013", 
            format = "%M/%d/%Y")), 1, 0)) %>% 
  as_tibble(.)

mexicoInt <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX) + recession + greatRecession + taper +
  log(EFFR)*recession + log(EFFR)*greatRecession + log(EFFR)*taper,
 data = mexico)

mexicoFFR <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = mexico)

mexicoRobust <- lm_robust(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = mexico)

mexicoT10Y <- lm(log(spread) ~ log(T10Y) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = mexico)

mexicoT10YFF <- lm(log(spread) ~ T10YFF + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = mexico)

#Russia = RUS
russiaBond <- sovereignDebt %>% 
  select(date, Russia) %>%
  rename(spread = Russia) %>% 
  as_tibble(.)

russiaGDP <- GDP %>% 
  select(date, Russia) %>% 
  rename(gdp = Russia) %>% 
  as_tibble(.)

russiaCPI <- CPI %>% 
  select(date, Russia) %>% 
  rename(cpi = Russia) %>% 
  as_tibble(.)

russiaDebt <- debt %>% 
  select(date, Russia) %>% 
  rename(debt = Russia) %>% 
  as_tibble(.)

russiaDeficit <- deficit %>% 
  select(date, Russia) %>% 
  rename(deficit = Russia) %>% 
  as_tibble(.)

russiaExchange <- exchange %>% 
  select(date, Russia) %>% 
  rename(exchange = Russia) %>% 
  as_tibble(.)

russiaReserve <- reserves %>% 
  select(date, Russia) %>% 
  rename(reserve = Russia) %>% 
  as_tibble(.)

russia <- inner_join(russiaBond, russiaGDP,
 by = "date") %>%
  inner_join(.,russiaCPI, by = "date") %>% 
  inner_join(.,russiaDebt, by = "date") %>% 
  inner_join(.,russiaDeficit, by = "date") %>% 
  inner_join(.,russiaExchange, by = "date") %>% 
  inner_join(.,russiaReserve, by = "date") %>%
  inner_join(.,VIX, by = "date") %>% 
  inner_join(.,EFFR, by = "date") %>%
  inner_join(.,T10Y, by = "date") %>% 
  inner_join(.,T10YFF, by = "date") %>% 
  mutate(recession = ifelse(date %in% recessionIndicator,
   1, 0)) %>%
  mutate(greatRecession = ifelse(date %in% greatIndicator, 1, 0)) %>%
  mutate(taper = ifelse(date > (as.Date("5/22/2013", 
            format = "%M/%d/%Y")), 1, 0)) %>% 
  as_tibble(.)

russiaInt <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX) + recession + greatRecession + taper +
  log(EFFR)*recession + log(EFFR)*greatRecession + log(EFFR)*taper,
 data = russia)

russiaFFR <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = russia)

russiaRobust <- lm_robust(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = russia)

russiaT10Y <- lm(log(spread) ~ log(T10Y) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = russia)

russiaT10YFF <- lm(log(spread) ~ T10YFF + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = russia)

#South Africa = ZAF
southAfricaBond <- sovereignDebt %>% 
  select(date, southAfrica) %>%
  rename(spread = southAfrica) %>% 
  as_tibble(.)

southAfricaGDP <- GDP %>% 
  select(date, southAfrica) %>% 
  rename(gdp = southAfrica) %>% 
  as_tibble(.)

southAfricaCPI <- CPI %>% 
  select(date, southAfrica) %>% 
  rename(cpi = southAfrica) %>% 
  as_tibble(.)

southAfricaDebt <- debt %>% 
  select(date, southAfrica) %>% 
  rename(debt = southAfrica) %>% 
  as_tibble(.)

southAfricaDeficit <- deficit %>% 
  select(date, southAfrica) %>% 
  rename(deficit = southAfrica) %>% 
  as_tibble(.)

southAfricaExchange <- exchange %>% 
  select(date, southAfrica) %>% 
  rename(exchange = southAfrica) %>% 
  as_tibble(.)

southAfricaReserve <- reserves %>% 
  select(date, southAfrica) %>% 
  rename(reserve = southAfrica) %>% 
  as_tibble(.)

southAfrica <- inner_join(southAfricaBond, southAfricaGDP,
 by = "date") %>%
  inner_join(.,southAfricaCPI, by = "date") %>% 
  inner_join(.,southAfricaDebt, by = "date") %>% 
  inner_join(.,southAfricaDeficit, by = "date") %>% 
  inner_join(.,southAfricaExchange, by = "date") %>% 
  inner_join(.,southAfricaReserve, by = "date") %>%
  inner_join(.,VIX, by = "date") %>% 
  inner_join(.,EFFR, by = "date") %>%
  inner_join(.,T10Y, by = "date") %>% 
  inner_join(.,T10YFF, by = "date") %>% 
  mutate(recession = ifelse(date %in% recessionIndicator,
   1, 0)) %>%
  mutate(greatRecession = ifelse(date %in% greatIndicator, 1, 0)) %>%
  mutate(taper = ifelse(date > (as.Date("5/22/2013", 
            format = "%M/%d/%Y")), 1, 0)) %>% 
  as_tibble(.)

southAfricaInt <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX) + recession + greatRecession + taper +
  log(EFFR)*recession + log(EFFR)*greatRecession + log(EFFR)*taper,
 data = southAfrica)

southAfricaFFR <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = southAfrica)

southAfricaRobust <- lm_robust(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = southAfrica)

southAfricaT10Y <- lm(log(spread) ~ log(T10Y) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = southAfrica)

southAfricaT10YFF <- lm(log(spread) ~ T10YFF + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = southAfrica)

#turkey = tur
turkeyBond <- sovereignDebt %>% 
  select(date, Turkey) %>%
  rename(spread = Turkey) %>% 
  as_tibble(.)

turkeyGDP <- GDP %>% 
  select(date, Turkey) %>% 
  rename(gdp = Turkey) %>% 
  as_tibble(.)

turkeyCPI <- CPI %>% 
  select(date, Turkey) %>% 
  rename(cpi = Turkey) %>% 
  as_tibble(.)

turkeyDebt <- debt %>% 
  select(date, Turkey) %>% 
  rename(debt = Turkey) %>% 
  as_tibble(.)

turkeyDeficit <- deficit %>% 
  select(date, Turkey) %>% 
  rename(deficit = Turkey) %>% 
  as_tibble(.)

turkeyExchange <- exchange %>% 
  select(date, Turkey) %>% 
  rename(exchange = Turkey) %>% 
  as_tibble(.)

turkeyReserve <- reserves %>% 
  select(date, Turkey) %>% 
  rename(reserve = Turkey) %>% 
  as_tibble(.)

turkey <- inner_join(turkeyBond, turkeyGDP,
 by = "date") %>%
  inner_join(.,turkeyCPI, by = "date") %>% 
  inner_join(.,turkeyDebt, by = "date") %>% 
  inner_join(.,turkeyDeficit, by = "date") %>% 
  inner_join(.,turkeyExchange, by = "date") %>% 
  inner_join(.,turkeyReserve, by = "date") %>%
  inner_join(.,VIX, by = "date") %>% 
  inner_join(.,EFFR, by = "date") %>%
  inner_join(.,T10Y, by = "date") %>% 
  inner_join(.,T10YFF, by = "date") %>% 
  mutate(recession = ifelse(date %in% recessionIndicator,
   1, 0)) %>%
  mutate(greatRecession = ifelse(date %in% greatIndicator, 1, 0)) %>%
  mutate(taper = ifelse(date > (as.Date("5/22/2013", 
            format = "%M/%d/%Y")), 1, 0)) %>% 
  as_tibble(.)

turkeyInt <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX) + recession + greatRecession + taper +
  log(EFFR)*recession + log(EFFR)*greatRecession + log(EFFR)*taper,
 data = turkey)

turkeyFFR <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = turkey)

turkeyRobust <- lm_robust(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = turkey)

turkeyT10Y <- lm(log(spread) ~ log(T10Y) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = turkey)

turkeyT10YFF <- lm(log(spread) ~ T10YFF + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = turkey)

# belarus
# belarusBond <- sovereignDebt %>% 
#   select(date, Belarus) %>%
#   rename(spread = Belarus) %>% 
#   as_tibble(.)

# belarusGDP <- GDP %>% 
#   select(date, Belarus) %>% 
#   rename(gdp = Belarus) %>% 
#   as_tibble(.)

# belarusCPI <- CPI %>% 
#   select(date, Belarus) %>% 
#   rename(cpi = Belarus) %>% 
#   as_tibble(.)

# belarusDebt <- debt %>% 
#   select(date, Belarus) %>% 
#   rename(debt = Belarus) %>% 
#   as_tibble(.)

# belarusDeficit <- deficit %>% 
#   select(date, Belarus) %>% 
#   rename(deficit = Belarus) %>% 
#   as_tibble(.)

# belarusExchange <- exchange %>% 
#   select(date, Belarus) %>% 
#   rename(exchange = Belarus) %>% 
#   as_tibble(.)

# belarusReserve <- reserves %>% 
#   select(date, Belarus) %>% 
#   rename(reserve = Belarus) %>% 
#   as_tibble(.)

# belarus <- inner_join(belarusBond, belarusGDP,
#  by = "date") %>%
#   inner_join(.,belarusCPI, by = "date") %>% 
#   inner_join(.,belarusDebt, by = "date") %>% 
#   inner_join(.,belarusDeficit, by = "date") %>% 
#   inner_join(.,belarusExchange, by = "date") %>% 
#   inner_join(.,belarusReserve, by = "date") %>%
#   inner_join(.,VIX, by = "date") %>% 
#   inner_join(.,EFFR, by = "date") %>%
#   inner_join(.,T10Y, by = "date") %>% 
#   inner_join(.,T10YFF, by = "date") %>% 
#   mutate(recession = ifelse(date %in% recessionIndicator,
#    1, 0)) %>%
#   mutate(greatRecession = ifelse(date %in% greatIndicator, 1, 0)) %>%
#   mutate(taper = ifelse(date > (as.Date("5/22/2013", 
#             format = "%M/%d/%Y")), 1, 0)) %>% 
#   as_tibble(.)

# belarusInt <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX) + recession + greatRecession + taper +
#   log(EFFR)*recession + log(EFFR)*greatRecession + log(EFFR)*taper,
#  data = belarus)

# belarusFFR <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
#  data = belarus)

# belarusRobust <- lm_robust(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
#  data = belarus)

# belarusT10Y <- lm(log(spread) ~ log(T10Y) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
#  data = belarus)

# belarusT10YFF <- lm(log(spread) ~ T10YFF + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
#  data = belarus)

 #chile
chileBond <- sovereignDebt %>% 
  select(date, Chile) %>%
  rename(spread = Chile) %>% 
  as_tibble(.)

chileGDP <- GDP %>% 
  select(date, Chile) %>% 
  rename(gdp = Chile) %>% 
  as_tibble(.)

chileCPI <- CPI %>% 
  select(date, Chile) %>% 
  rename(cpi = Chile) %>% 
  as_tibble(.)

chileDebt <- debt %>% 
  select(date, Chile) %>% 
  rename(debt = Chile) %>% 
  as_tibble(.)

chileDeficit <- deficit %>% 
  select(date, Chile) %>% 
  rename(deficit = Chile) %>% 
  as_tibble(.)

chileExchange <- exchange %>% 
  select(date, Chile) %>% 
  rename(exchange = Chile) %>% 
  as_tibble(.)

chileReserve <- reserves %>% 
  select(date, Chile) %>% 
  rename(reserve = Chile) %>% 
  as_tibble(.)

chile <- inner_join(chileBond, chileGDP,
 by = "date") %>%
  inner_join(.,chileCPI, by = "date") %>% 
  inner_join(.,chileDebt, by = "date") %>% 
  inner_join(.,chileDeficit, by = "date") %>% 
  inner_join(.,chileExchange, by = "date") %>% 
  inner_join(.,chileReserve, by = "date") %>%
  inner_join(.,VIX, by = "date") %>% 
  inner_join(.,EFFR, by = "date") %>%
  inner_join(.,T10Y, by = "date") %>% 
  inner_join(.,T10YFF, by = "date") %>% 
  mutate(recession = ifelse(date %in% recessionIndicator,
   1, 0)) %>%
  mutate(greatRecession = ifelse(date %in% greatIndicator, 1, 0)) %>%
  mutate(taper = ifelse(date > (as.Date("5/22/2013", 
            format = "%M/%d/%Y")), 1, 0)) %>% 
  as_tibble(.)

chileInt <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX) + recession + greatRecession + taper +
  log(EFFR)*recession + log(EFFR)*greatRecession + log(EFFR)*taper,
 data = chile)

chileFFR <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = chile)

chileRobust <- lm_robust(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = chile)

chileT10Y <- lm(log(spread) ~ log(T10Y) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = chile)

chileT10YFF <- lm(log(spread) ~ T10YFF + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = chile)

#colombia
colombiaBond <- sovereignDebt %>% 
  select(date, Colombia) %>%
  rename(spread = Colombia) %>% 
  as_tibble(.)

colombiaGDP <- GDP %>% 
  select(date, Colombia) %>% 
  rename(gdp = Colombia) %>% 
  as_tibble(.)

colombiaCPI <- CPI %>% 
  select(date, Colombia) %>% 
  rename(cpi = Colombia) %>% 
  as_tibble(.)

colombiaDebt <- debt %>% 
  select(date, Colombia) %>% 
  rename(debt = Colombia) %>% 
  as_tibble(.)

colombiaDeficit <- deficit %>% 
  select(date, Colombia) %>% 
  rename(deficit = Colombia) %>% 
  as_tibble(.)

colombiaExchange <- exchange %>% 
  select(date, Colombia) %>% 
  rename(exchange = Colombia) %>% 
  as_tibble(.)

colombiaReserve <- reserves %>% 
  select(date, Colombia) %>% 
  rename(reserve = Colombia) %>% 
  as_tibble(.)

colombia <- inner_join(colombiaBond, colombiaGDP,
 by = "date") %>%
  inner_join(.,colombiaCPI, by = "date") %>% 
  inner_join(.,colombiaDebt, by = "date") %>% 
  inner_join(.,colombiaDeficit, by = "date") %>% 
  inner_join(.,colombiaExchange, by = "date") %>% 
  inner_join(.,colombiaReserve, by = "date") %>%
  inner_join(.,VIX, by = "date") %>% 
  inner_join(.,EFFR, by = "date") %>%
  inner_join(.,T10Y, by = "date") %>% 
  inner_join(.,T10YFF, by = "date") %>% 
  mutate(recession = ifelse(date %in% recessionIndicator,
   1, 0)) %>%
  mutate(greatRecession = ifelse(date %in% greatIndicator, 1, 0)) %>%
  mutate(taper = ifelse(date > (as.Date("5/22/2013", 
            format = "%M/%d/%Y")), 1, 0)) %>% 
  as_tibble(.)

colombiaInt <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX) + recession + greatRecession + taper +
  log(EFFR)*recession + log(EFFR)*greatRecession + log(EFFR)*taper,
 data = colombia)

colombiaFFR <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = colombia)

colombiaRobust <- lm_robust(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = colombia)

colombiaT10Y <- lm(log(spread) ~ log(T10Y) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = colombia)

colombiaT10YFF <- lm(log(spread) ~ T10YFF + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = colombia)

#egypt
egyptBond <- sovereignDebt %>% 
  select(date, Egypt) %>%
  rename(spread = Egypt) %>% 
  as_tibble(.)

egyptGDP <- GDP %>% 
  select(date, Egypt) %>% 
  rename(gdp = Egypt) %>% 
  as_tibble(.)

egyptCPI <- CPI %>% 
  select(date, Egypt) %>% 
  rename(cpi = Egypt) %>% 
  as_tibble(.)

egyptDebt <- debt %>% 
  select(date, Egypt) %>% 
  rename(debt = Egypt) %>% 
  as_tibble(.)

egyptDeficit <- deficit %>% 
  select(date, Egypt) %>% 
  rename(deficit = Egypt) %>% 
  as_tibble(.)

egyptExchange <- exchange %>% 
  select(date, Egypt) %>% 
  rename(exchange = Egypt) %>% 
  as_tibble(.)

egyptReserve <- reserves %>% 
  select(date, Egypt) %>% 
  rename(reserve = Egypt) %>% 
  as_tibble(.)

egypt <- inner_join(egyptBond, egyptGDP,
 by = "date") %>%
  inner_join(.,egyptCPI, by = "date") %>% 
  inner_join(.,egyptDebt, by = "date") %>% 
  inner_join(.,egyptDeficit, by = "date") %>% 
  inner_join(.,egyptExchange, by = "date") %>% 
  inner_join(.,egyptReserve, by = "date") %>%
  inner_join(.,VIX, by = "date") %>% 
  inner_join(.,EFFR, by = "date") %>%
  inner_join(.,T10Y, by = "date") %>% 
  inner_join(.,T10YFF, by = "date") %>% 
  mutate(recession = ifelse(date %in% recessionIndicator,
   1, 0)) %>%
  mutate(greatRecession = ifelse(date %in% greatIndicator, 1, 0)) %>%
  mutate(taper = ifelse(date > (as.Date("5/22/2013", 
            format = "%M/%d/%Y")), 1, 0)) %>% 
  as_tibble(.)

egyptInt <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX) + recession + greatRecession + taper +
  log(EFFR)*recession + log(EFFR)*greatRecession + log(EFFR)*taper,
 data = egypt)

egyptFFR <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = egypt)

egyptRobust <- lm_robust(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = egypt)

egyptT10Y <- lm(log(spread) ~ log(T10Y) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = egypt)

egyptT10YFF <- lm(log(spread) ~ T10YFF + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = egypt)

 #hungary
hungaryBond <- sovereignDebt %>% 
  select(date, Hungary) %>%
  rename(spread = Hungary) %>% 
  as_tibble(.)

hungaryGDP <- GDP %>% 
  select(date, Hungary) %>% 
  rename(gdp = Hungary) %>% 
  as_tibble(.)

hungaryCPI <- CPI %>% 
  select(date, Hungary) %>% 
  rename(cpi = Hungary) %>% 
  as_tibble(.)

hungaryDebt <- debt %>% 
  select(date, Hungary) %>% 
  rename(debt = Hungary) %>% 
  as_tibble(.)

hungaryDeficit <- deficit %>% 
  select(date, Hungary) %>% 
  rename(deficit = Hungary) %>% 
  as_tibble(.)

hungaryExchange <- exchange %>% 
  select(date, Hungary) %>% 
  rename(exchange = Hungary) %>% 
  as_tibble(.)

hungaryReserve <- reserves %>% 
  select(date, Hungary) %>% 
  rename(reserve = Hungary) %>% 
  as_tibble(.)

hungary <- inner_join(hungaryBond, hungaryGDP,
 by = "date") %>%
  inner_join(.,hungaryCPI, by = "date") %>% 
  inner_join(.,hungaryDebt, by = "date") %>% 
  inner_join(.,hungaryDeficit, by = "date") %>% 
  inner_join(.,hungaryExchange, by = "date") %>% 
  inner_join(.,hungaryReserve, by = "date") %>%
  inner_join(.,VIX, by = "date") %>% 
  inner_join(.,EFFR, by = "date") %>%
  inner_join(.,T10Y, by = "date") %>% 
  inner_join(.,T10YFF, by = "date") %>% 
  mutate(recession = ifelse(date %in% recessionIndicator,
   1, 0)) %>%
  mutate(greatRecession = ifelse(date %in% greatIndicator, 1, 0)) %>%
  mutate(taper = ifelse(date > (as.Date("5/22/2013", 
            format = "%M/%d/%Y")), 1, 0)) %>% 
  as_tibble(.)

hungaryInt <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX) + recession + greatRecession + taper +
  log(EFFR)*recession + log(EFFR)*greatRecession + log(EFFR)*taper,
 data = hungary)

hungaryFFR <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = hungary)

hungaryRobust <- lm_robust(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = hungary)

hungaryT10Y <- lm(log(spread) ~ log(T10Y) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = hungary)

hungaryT10YFF <- lm(log(spread) ~ T10YFF + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = hungary)

#malaysia
malaysiaBond <- sovereignDebt %>% 
  select(date, Malaysia) %>%
  rename(spread = Malaysia) %>% 
  as_tibble(.)

malaysiaGDP <- GDP %>% 
  select(date, Malaysia) %>% 
  rename(gdp = Malaysia) %>% 
  as_tibble(.)

malaysiaCPI <- CPI %>% 
  select(date, Malaysia) %>% 
  rename(cpi = Malaysia) %>% 
  as_tibble(.)

malaysiaDebt <- debt %>% 
  select(date, Malaysia) %>% 
  rename(debt = Malaysia) %>% 
  as_tibble(.)

malaysiaDeficit <- deficit %>% 
  select(date, Malaysia) %>% 
  rename(deficit = Malaysia) %>% 
  as_tibble(.)

malaysiaExchange <- exchange %>% 
  select(date, Malaysia) %>% 
  rename(exchange = Malaysia) %>% 
  as_tibble(.)

malaysiaReserve <- reserves %>% 
  select(date, Malaysia) %>% 
  rename(reserve = Malaysia) %>% 
  as_tibble(.)

malaysia <- inner_join(malaysiaBond, malaysiaGDP,
 by = "date") %>%
  inner_join(.,malaysiaCPI, by = "date") %>% 
  inner_join(.,malaysiaDebt, by = "date") %>% 
  inner_join(.,malaysiaDeficit, by = "date") %>% 
  inner_join(.,malaysiaExchange, by = "date") %>% 
  inner_join(.,malaysiaReserve, by = "date") %>%
  inner_join(.,VIX, by = "date") %>% 
  inner_join(.,EFFR, by = "date") %>%
  inner_join(.,T10Y, by = "date") %>% 
  inner_join(.,T10YFF, by = "date") %>% 
  mutate(recession = ifelse(date %in% recessionIndicator,
   1, 0)) %>%
  mutate(greatRecession = ifelse(date %in% greatIndicator, 1, 0)) %>%
  mutate(taper = ifelse(date > (as.Date("5/22/2013", 
            format = "%M/%d/%Y")), 1, 0)) %>% 
  as_tibble(.)

malaysiaInt <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX) + recession + greatRecession + taper +
  log(EFFR)*recession + log(EFFR)*greatRecession + log(EFFR)*taper,
 data = malaysia)

malaysiaFFR <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = malaysia)

malaysiaRobust <- lm_robust(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = malaysia)

malaysiaT10Y <- lm(log(spread) ~ log(T10Y) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = malaysia)

malaysiaT10YFF <- lm(log(spread) ~ T10YFF + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = malaysia)

 #nigeria

nigeriaBond <- sovereignDebt %>% 
  select(date, Nigeria) %>%
  rename(spread = Nigeria) %>% 
  as_tibble(.)

nigeriaGDP <- GDP %>% 
  select(date, Nigeria) %>% 
  rename(gdp = Nigeria) %>% 
  as_tibble(.)

nigeriaCPI <- CPI %>% 
  select(date, Nigeria) %>% 
  rename(cpi = Nigeria) %>% 
  as_tibble(.)

nigeriaDebt <- debt %>% 
  select(date, Nigeria) %>% 
  rename(debt = Nigeria) %>% 
  as_tibble(.)

nigeriaDeficit <- deficit %>% 
  select(date, Nigeria) %>% 
  rename(deficit = Nigeria) %>% 
  as_tibble(.)

nigeriaExchange <- exchange %>% 
  select(date, Nigeria) %>% 
  rename(exchange = Nigeria) %>% 
  as_tibble(.)

nigeriaReserve <- reserves %>% 
  select(date, Nigeria) %>% 
  rename(reserve = Nigeria) %>% 
  as_tibble(.)

nigeria <- inner_join(nigeriaBond, nigeriaGDP,
 by = "date") %>%
  inner_join(.,nigeriaCPI, by = "date") %>% 
  inner_join(.,nigeriaDebt, by = "date") %>% 
  inner_join(.,nigeriaDeficit, by = "date") %>% 
  inner_join(.,nigeriaExchange, by = "date") %>% 
  inner_join(.,nigeriaReserve, by = "date") %>%
  inner_join(.,VIX, by = "date") %>% 
  inner_join(.,EFFR, by = "date") %>%
  inner_join(.,T10Y, by = "date") %>% 
  inner_join(.,T10YFF, by = "date") %>% 
  mutate(recession = ifelse(date %in% recessionIndicator,
   1, 0)) %>%
  mutate(greatRecession = ifelse(date %in% greatIndicator, 1, 0)) %>%
  mutate(taper = ifelse(date > (as.Date("5/22/2013", 
            format = "%M/%d/%Y")), 1, 0)) %>% 
  as_tibble(.)

nigeriaInt <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX) + recession + greatRecession + taper +
  log(EFFR)*recession + log(EFFR)*greatRecession + log(EFFR)*taper,
 data = nigeria)

nigeriaFFR <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = nigeria)

nigeriaRobust <- lm_robust(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = nigeria)

nigeriaT10Y <- lm(log(spread) ~ log(T10Y) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = nigeria)

nigeriaT10YFF <- lm(log(spread) ~ T10YFF + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = nigeria)

#peru
peruBond <- sovereignDebt %>% 
  select(date, Peru) %>%
  rename(spread = Peru) %>% 
  as_tibble(.)

peruGDP <- GDP %>% 
  select(date, Peru) %>% 
  rename(gdp = Peru) %>% 
  as_tibble(.)

peruCPI <- CPI %>% 
  select(date, Peru) %>% 
  rename(cpi = Peru) %>% 
  as_tibble(.)

peruDebt <- debt %>% 
  select(date, Peru) %>% 
  rename(debt = Peru) %>% 
  as_tibble(.)

peruDeficit <- deficit %>% 
  select(date, Peru) %>% 
  rename(deficit = Peru) %>% 
  as_tibble(.)

peruExchange <- exchange %>% 
  select(date, Peru) %>% 
  rename(exchange = Peru) %>% 
  as_tibble(.)

peruReserve <- reserves %>% 
  select(date, Peru) %>% 
  rename(reserve = Peru) %>% 
  as_tibble(.)

peru <- inner_join(peruBond, peruGDP,
 by = "date") %>%
  inner_join(.,peruCPI, by = "date") %>% 
  inner_join(.,peruDebt, by = "date") %>% 
  inner_join(.,peruDeficit, by = "date") %>% 
  inner_join(.,peruExchange, by = "date") %>% 
  inner_join(.,peruReserve, by = "date") %>%
  inner_join(.,VIX, by = "date") %>% 
  inner_join(.,EFFR, by = "date") %>%
  inner_join(.,T10Y, by = "date") %>% 
  inner_join(.,T10YFF, by = "date") %>% 
  mutate(recession = ifelse(date %in% recessionIndicator,
   1, 0)) %>%
  mutate(greatRecession = ifelse(date %in% greatIndicator, 1, 0)) %>%
  mutate(taper = ifelse(date > (as.Date("5/22/2013", 
            format = "%M/%d/%Y")), 1, 0)) %>% 
  as_tibble(.)

peruInt <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX) + recession + greatRecession + taper +
  log(EFFR)*recession + log(EFFR)*greatRecession + log(EFFR)*taper,
 data = peru)

peruFFR <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = peru)

peruRobust <- lm_robust(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = peru)

peruT10Y <- lm(log(spread) ~ log(T10Y) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = peru)

peruT10YFF <- lm(log(spread) ~ T10YFF + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = peru)

#philippines
philippinesBond <- sovereignDebt %>% 
  select(date, Philippines) %>%
  rename(spread = Philippines) %>% 
  as_tibble(.)

philippinesGDP <- GDP %>% 
  select(date, Philippines) %>% 
  rename(gdp = Philippines) %>% 
  as_tibble(.)

philippinesCPI <- CPI %>% 
  select(date, Philippines) %>% 
  rename(cpi = Philippines) %>% 
  as_tibble(.)

philippinesDebt <- debt %>% 
  select(date, Philippines) %>% 
  rename(debt = Philippines) %>% 
  as_tibble(.)

philippinesDeficit <- deficit %>% 
  select(date, Philippines) %>% 
  rename(deficit = Philippines) %>% 
  as_tibble(.)

philippinesExchange <- exchange %>% 
  select(date, Philippines) %>% 
  rename(exchange = Philippines) %>% 
  as_tibble(.)

philippinesReserve <- reserves %>% 
  select(date, Philippines) %>% 
  rename(reserve = Philippines) %>% 
  as_tibble(.)

philippines <- inner_join(philippinesBond, philippinesGDP,
 by = "date") %>%
  inner_join(.,philippinesCPI, by = "date") %>% 
  inner_join(.,philippinesDebt, by = "date") %>% 
  inner_join(.,philippinesDeficit, by = "date") %>% 
  inner_join(.,philippinesExchange, by = "date") %>% 
  inner_join(.,philippinesReserve, by = "date") %>%
  inner_join(.,VIX, by = "date") %>% 
  inner_join(.,EFFR, by = "date") %>%
  inner_join(.,T10Y, by = "date") %>% 
  inner_join(.,T10YFF, by = "date") %>% 
  mutate(recession = ifelse(date %in% recessionIndicator,
   1, 0)) %>%
  mutate(greatRecession = ifelse(date %in% greatIndicator, 1, 0)) %>%
  mutate(taper = ifelse(date > (as.Date("5/22/2013", 
            format = "%M/%d/%Y")), 1, 0)) %>% 
  as_tibble(.)

philippinesInt <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX) + recession + greatRecession + taper +
  log(EFFR)*recession + log(EFFR)*greatRecession + log(EFFR)*taper,
 data = philippines)

philippinesFFR <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = philippines)

philippinesRobust <- lm_robust(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = philippines)

philippinesT10Y <- lm(log(spread) ~ log(T10Y) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = philippines)

philippinesT10YFF <- lm(log(spread) ~ T10YFF + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = philippines)

#poland
polandBond <- sovereignDebt %>% 
  select(date, Poland) %>%
  rename(spread = Poland) %>% 
  as_tibble(.)

polandGDP <- GDP %>% 
  select(date, Poland) %>% 
  rename(gdp = Poland) %>% 
  as_tibble(.)

polandCPI <- CPI %>% 
  select(date, Poland) %>% 
  rename(cpi = Poland) %>% 
  as_tibble(.)

polandDebt <- debt %>% 
  select(date, Poland) %>% 
  rename(debt = Poland) %>% 
  as_tibble(.)

polandDeficit <- deficit %>% 
  select(date, Poland) %>% 
  rename(deficit = Poland) %>% 
  as_tibble(.)

polandExchange <- exchange %>% 
  select(date, Poland) %>% 
  rename(exchange = Poland) %>% 
  as_tibble(.)

polandReserve <- reserves %>% 
  select(date, Poland) %>% 
  rename(reserve = Poland) %>% 
  as_tibble(.)

poland <- inner_join(polandBond, polandGDP,
 by = "date") %>%
  inner_join(.,polandCPI, by = "date") %>% 
  inner_join(.,polandDebt, by = "date") %>% 
  inner_join(.,polandDeficit, by = "date") %>% 
  inner_join(.,polandExchange, by = "date") %>% 
  inner_join(.,polandReserve, by = "date") %>%
  inner_join(.,VIX, by = "date") %>% 
  inner_join(.,EFFR, by = "date") %>%
  inner_join(.,T10Y, by = "date") %>% 
  inner_join(.,T10YFF, by = "date") %>% 
  mutate(recession = ifelse(date %in% recessionIndicator,
   1, 0)) %>%
  mutate(greatRecession = ifelse(date %in% greatIndicator, 1, 0)) %>%
  mutate(taper = ifelse(date > (as.Date("5/22/2013", 
            format = "%M/%d/%Y")), 1, 0)) %>% 
  as_tibble(.)

polandInt <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX) + recession + greatRecession + taper +
  log(EFFR)*recession + log(EFFR)*greatRecession + log(EFFR)*taper,
 data = poland)

polandFFR <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = poland)

polandRobust <- lm_robust(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = poland)

polandT10Y <- lm(log(spread) ~ log(T10Y) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = poland)

polandT10YFF <- lm(log(spread) ~ T10YFF + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = poland)

#vietnam
vietnamBond <- sovereignDebt %>% 
  select(date, Vietnam) %>%
  rename(spread = Vietnam) %>% 
  as_tibble(.)

vietnamGDP <- GDP %>% 
  select(date, Vietnam) %>% 
  rename(gdp = Vietnam) %>% 
  as_tibble(.)

vietnamCPI <- CPI %>% 
  select(date, Vietnam) %>% 
  rename(cpi = Vietnam) %>% 
  as_tibble(.)

vietnamDebt <- debt %>% 
  select(date, Vietnam) %>% 
  rename(debt = Vietnam) %>% 
  as_tibble(.)

vietnamDeficit <- deficit %>% 
  select(date, Vietnam) %>% 
  rename(deficit = Vietnam) %>% 
  as_tibble(.)

vietnamExchange <- exchange %>% 
  select(date, Vietnam) %>% 
  rename(exchange = Vietnam) %>% 
  as_tibble(.)

vietnamReserve <- reserves %>% 
  select(date, Vietnam) %>% 
  rename(reserve = Vietnam) %>% 
  as_tibble(.)

vietnam <- inner_join(vietnamBond, vietnamGDP,
 by = "date") %>%
  inner_join(.,vietnamCPI, by = "date") %>% 
  inner_join(.,vietnamDebt, by = "date") %>% 
  inner_join(.,vietnamDeficit, by = "date") %>% 
  inner_join(.,vietnamExchange, by = "date") %>% 
  inner_join(.,vietnamReserve, by = "date") %>%
  inner_join(.,VIX, by = "date") %>% 
  inner_join(.,EFFR, by = "date") %>%
  inner_join(.,T10Y, by = "date") %>% 
  inner_join(.,T10YFF, by = "date") %>% 
  mutate(recession = ifelse(date %in% recessionIndicator,
   1, 0)) %>%
  mutate(greatRecession = ifelse(date %in% greatIndicator, 1, 0)) %>%
  mutate(taper = ifelse(date > (as.Date("5/22/2013", 
            format = "%M/%d/%Y")), 1, 0)) %>% 
  as_tibble(.)

vietnamInt <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX) + recession + greatRecession + taper +
  log(EFFR)*recession + log(EFFR)*greatRecession + log(EFFR)*taper,
 data = vietnam)

vietnamFFR <- lm(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = vietnam)

vietnamRobust <- lm_robust(log(spread) ~ log(EFFR) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = vietnam)

vietnamT10Y <- lm(log(spread) ~ log(T10Y) + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = vietnam)

vietnamT10YFF <- lm(log(spread) ~ T10YFF + gdp + cpi + debt + deficit + log(exchange) + log(reserve) + log(VIX),
 data = vietnam)