# loading packages
library(haven)
library(tidyverse)
library(stargazer)
library(ggrepel)
library(sandwich)
library(lmtest)

# Loading dataset
wdi_struc_change_assignment <- read_dta("wdi_struc_change_assignment.dta")
wdi_dtast <- wdi_struc_change_assignment

# filtering out Bangladesh and adding column for log pc gdp
wdi_dtast_bang <- wdi_dtast %>% filter(CountryName=="Bangladesh") %>% mutate(log_gdp_pc_ppp=log(gdp_pc_ppp))


## Share of gdp against year for different sectors

wdi2 <- wdi_dtast_bang %>%
  select(year, agri_gdp_sh, ind_gdp_sh, serv_gdp_sh) %>%
  pivot_longer(!year, names_to = "sectors", values_to = "shares1") %>% 
  mutate(shares= shares1*100)

graph_yo1 <- wdi2 %>% 
  ggplot(aes(year, shares, colour = sectors)) +
  geom_line() +
  ggtitle("Figure 1: Sectoral shares of output 1990-2020 in %")
graph_yo1

## Share of emp share against year for different sectors

# There's no data for employment share for any sector for 2020. Lets remove the row and make a new dataset
wdi_dtast_bang_emp <- wdi_dtast_bang %>% filter(!year=="2020")

wdi3 <- wdi_dtast_bang_emp %>%
  select(year, agri_emp_sh, ind_emp_sh, serv_emp_sh) %>%
  mutate(ind_emp_sh = 100*ind_emp_sh) %>% 
  pivot_longer(!year, names_to = "sectors", values_to = "shares1") %>% 
  mutate(shares = shares1*100)

graph_yo2 <- wdi3 %>% 
  ggplot(aes(year, shares, colour = sectors)) +
  geom_line() +
  ggtitle("Figure 2: Sectoral shares of employment 1990-2019 in %")
graph_yo2


## Elasticity

# Regressing agri share of gdp on log gdp pc
bangreg1 <- lm(agri_gdp_sh~log_gdp_pc_ppp, data = wdi_dtast_bang)

# Regressing manufacturing share of gdp on log gdp pc
bangreg2 <- lm(manu_gdp_sh~log_gdp_pc_ppp, data = wdi_dtast_bang)

# Regressing industry share of gdp on log gdp pc
bangreg3 <- lm(ind_gdp_sh~log_gdp_pc_ppp, data = wdi_dtast_bang)

# Regressing service share of gdp on log gdp pc
bangreg4 <- lm(serv_gdp_sh~log_gdp_pc_ppp, data = wdi_dtast_bang)

# Results in a single table
stargazer(bangreg1,bangreg3, bangreg4, type="html", title= "Share in GDP against log GDP pc ppp" ,
          out= "gdpshare.html")

# Regressing agri share of emp on log gdp pc
bangreg5 <- lm(agri_emp_sh~log_gdp_pc_ppp, data = wdi_dtast_bang)

# Regressing industry share of emp on log gdp pc
bangreg6 <- lm(100*ind_emp_sh~log_gdp_pc_ppp, data = wdi_dtast_bang)

# Regressing service share of emp on log gdp pc
bangreg7 <- lm(serv_emp_sh~log_gdp_pc_ppp, data = wdi_dtast_bang)

# Results in a single table
stargazer(bangreg5,bangreg6, bangreg7, type="html", title= "Share in employment against log GDP pc ppp" ,
          out= "empshare.html")