library(WDI)
library(magrittr)
library(dplyr)
library(owidR)
library(readxl)
library(openxlsx)
library(ggplot2)
library(gghighlight)
library(scales)
library(tidyr)


WDIsearch("emissions")

#WDI data on CO2 emissions
indicator <- c("CO2 emissions (metric tons per capita)"="EN.ATM.CO2E.PC", 
               "Poverty headcount ratio at national poverty lines (% of population)"= "SI.POV.NAHC", 
               "Poverty Headcount ($1.90 a day)"="1.0.HCount.1.90usd", 
               "Urban population (% of total population)"= "SP.URB.TOTL.IN.ZS", 
               "Gini index"="SI.POV.GINI", 
               "Political Stability and Absence of Violence/Terrorism: Estimate"="PV.EST", 
               "Rule of Law: Estimate" = "RL.EST", 
               "Control of Corruption (estimate)"  ="GV.CONT.CO.ES")

data <- WDI(country = "all", indicator = indicator, start = 1960, end = 2020) %>% 
  as_tibble()

data_df <- data %>% 
  filter(country%in%c("China", "India", "Japan", "Korea, Rep.", "United States", 
                      "Brazil", "Nigeria", "Mexico", "Turkiye","Russian Federation", "Indonesia",
                      "Egypt, Arab Rep.", "South Africa")) 

#write.xlsx(data_df, "data_df.xlsx")

data_df_new <- data_df %>% 
  filter(!iso2c %in% c("BRA", "EGY","MEX", "NGA", "ZAF")) 

data_df_new %>% 
  ggplot(aes(year, `Urban population (% of total population)` ))+
  geom_line(size = 1.1, alpha = 0.8, show.legend = F,  color="#42C2B8")+
  scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10))+
  facet_wrap(~country, ncol=4, scales = "free_y")+
  theme_light()+
  theme(
    axis.text.x  = element_text(size = 15),
    axis.text.y  = element_text(size = 15),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    strip.text.x = element_text(size = 18, face="bold")
  )+
  labs(x="Year")

ggsave("urbanization.png", width = 20, height = 14, dpi = 300)  


data_df_new <- data_df_new %>% 
  select(country, year, `Political Stability and Absence of Violence/Terrorism: Estimate`,
         `Rule of Law: Estimate`) %>% 
  filter(year>=2000) %>% 
  pivot_longer(!c("country", "year"), names_to = "fragility", values_to = "value") 


data_df_new %>% 
  ggplot(aes(year, value, color=fragility))+
  geom_line(alpha = 0.8, size=1.1)+
  facet_wrap(~country, ncol=4, scales = "free_y")+
  #scale_color_manual(values = c("#BE5B40", "#526177"))+
  theme_light()+
  theme(
    axis.text.x  = element_text(size = 15),
    axis.text.y  = element_text(size = 15),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    strip.text.x = element_text(size = 18, face="bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face="bold")
  )+
  labs(x="Years")

ggsave("fragility.png", width = 20, height = 14, dpi = 300)  



data_df %>% 
  select(c("country", "Gini index")) %>% View()

data_df %>%
  drop_na(`Gini index`) %>%
  ggplot(aes(year, `Gini index`))+
  geom_line(size = 1.1, alpha = 0.8, show.legend = F,  color="#42C2B8")+
  facet_wrap(~country, ncol=4, scales = "free_y")+
  theme_light()+
  theme(
    axis.text.x  = element_text(size = 15),
    axis.text.y  = element_text(size = 15),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    strip.text.x = element_text(size = 18, face="bold"))+
  labs(x="Years")

ggsave("gini.png", width = 20, height = 14, dpi = 300)  

#owid data on CO2 emissions
owid_search("emission")

annual_co2_emission <- owid("annual-co2-emissions-per-country") 
#ghg_percapita <- owid("ghg-emissions-per-capita") 
#write.xlsx(annual_co2_emission, "annual_co2_emission.xlsx")

annual_emission <- annual_co2_emission %>% 
  rename(country=entity) %>% 
  filter(country%in%c("China", "India", "Japan", "South Korea", "United States", 
                      "Brazil", "Nigeria", "Mexico", "Turkey","Russia", "Indonesia",
                      "Egypt", "South Africa")) %>% 
  filter(year>1900)  

ggplot(data = annual_emission ,aes(year, `Annual CO₂ emissions` ))+
  geom_line(size = 1.1, alpha = 0.8, show.legend = F,  color="#42C2B8")+
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6))+
  scale_x_continuous(limits = c(1900, 2025), breaks = seq(1900, 2025, by = 20))+
  #geom_line(data=annual_emission %>% filter(country=="China"), col = "black")
  facet_wrap(~country, ncol=4, scales = "free_y")+
  theme_light()+
  theme(
    axis.text.x  = element_text(size = 15),
    axis.text.y  = element_text(size = 15),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    strip.text.x = element_text(size = 18, face="bold")
  )+
  labs(x="Years", y="Annuaal C02 emissions (in million tonnes)")

ggsave("emission.png", width = 20, height = 14, dpi = 300)  

#reading he cities GDP data
CitiesGDP_df <- read_excel("Data.xlsx", sheet = "Sheet2", skip = 1) 

CitiesGDP_df <- CitiesGDP_df %>% 
  select(c("Metro area", "Country","GDP, 2014 (PPP, $Million)", "Population, 2014")) %>% 
  filter(Country %in% c("China", "India","South Africa", "Egypt", "Turkey", "Mexico",
                        "Indonesia", "Russia", "South Korea", "Japan")) %>% 
  group_by(Country) %>% 
  summarise(cities=n(),`Population, 2014`= sum(`Population, 2014`),
            `GDP, 2014 (PPP, $Million)`=sum(`GDP, 2014 (PPP, $Million)`))

#write.xlsx(CitiesGDP_df, "CitiesGDP_df.xlsx")

###Poverty data
poverty_rate <- read_excel("data_middle_poverty.xlsx", skip = 3)

poverty_rate <- poverty_rate %>% 
  pivot_longer(!country, names_to = "Year", values_to = "poverty_headcount")  

poverty_rate$Year <- as.numeric(poverty_rate$Year)

ggplot(data = poverty_rate, aes(x = Year, y = poverty_headcount))+
  geom_line(size = 1.1, alpha = 0.8,   color="#42C2B8")+
  facet_wrap(~country, ncol=4, scales = "free_y")+
  theme_light()+
  theme(
    axis.text.x  = element_text(size = 15),
    axis.text.y  = element_text(size = 15),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    strip.text.x = element_text(size = 18, face="bold")
  )+
  labs(x="Year", y="Poverty rate at $1.90/day (percent of population)")

ggsave("poverty.png", width = 22, height = 14, dpi = 300)  


##intergeneration mobility
intergeneration_mobility <- read_excel("data_middle.xlsx", sheet = "intergeneration_mobility (EDU)", skip = 1)

mobility_subset <- intergeneration_mobility %>% 
  filter(country%in%c("China", "India", "Japan", "Korea, Rep.", "United States", 
                      "Brazil", "Nigeria", "Mexico", "Turkey","Russian Federation", "Indonesia",
                      "Egypt, Arab Rep.", "South Africa")) %>% 
  select(c("country", "cohort", "parent", "child", "COR")) %>% 
  filter(parent=="avg") %>% 
  filter(child=="all") 

mobility_subset %>% ggplot(aes(cohort, COR))+
  geom_point(size=3, color="steelblue")+
  geom_line(color="steelblue")+
  facet_wrap(~country, ncol=4)+
  theme_light()+
  theme(
    axis.text.x  = element_text(size = 15),
    axis.text.y  = element_text(size = 15),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    strip.text.x = element_text(size = 18, face="bold")
  )+
  labs(x="Year", y="Correlation Coeff.")

ggsave("intergenrational_mobility.png", width = 22, height = 14, dpi = 300) 
