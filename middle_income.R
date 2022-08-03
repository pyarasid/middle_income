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
library(ggstatsplot)
library(haven)


WDIsearch("GDP per capita")

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


#GDP per capita---
indicator_GDP <- c("GDP per capita (constant 2015 US$)"="NY.GDP.PCAP.KD")

data_GDPpercap <-  WDI(country = "all", indicator = indicator_GDP, start = 1960, end = 2020) %>% 
  as_tibble()

data_GDPpercap %>% 
  filter(country%in%c("Low income", "Lower middle income", "Upper middle income", 
                      "High income")) %>% 
  ggplot(aes(year, `GDP per capita (constant 2015 US$)`))+
  geom_line(size = 1.1, alpha = 0.8, show.legend = F,)+
  facet_wrap(~country, scales="free_y")+
  theme_light()+
  theme(
    axis.text.x  = element_text(size = 15),
    axis.text.y  = element_text(size = 15),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    strip.text.x = element_text(size = 18, face="bold"))+
  labs(x="Years")

ggsave("gdp_percap.png", width = 20, height = 14, dpi = 300) 

#looking at all the above data for the four income categories
data_cor <- data %>% filter(country%in%c("Low income", "Lower middle income", "Upper middle income", 
                             "High income","China", "India", "Japan", "Korea, Rep.", "United States", 
                      "Brazil", "Nigeria", "Mexico", "Turkiye","Russian Federation", "Indonesia",
                      "Egypt, Arab Rep.", "South Africa")) %>% 
  filter(!iso2c %in% c("BRA", "EGY","MEX", "NGA", "ZAF")) %>% 
  select(-c("Poverty Headcount ($1.90 a day)", "iso2c"))

data_GDPpercapcor <- data_GDPpercap %>% 
  filter(country%in%c("Low income", "Lower middle income", "Upper middle income", 
                      "High income", "China", "India", "Japan", "Korea, Rep.", "United States", 
                      "Brazil", "Nigeria", "Mexico", "Turkiye","Russian Federation", "Indonesia",
                      "Egypt, Arab Rep.", "South Africa")) %>% 
  filter(!iso2c %in% c("BRA", "EGY","MEX", "NGA", "ZAF")) %>% 
  select(-c("iso2c"))


#owid data on CO2 emissions----------------
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
  labs(x="Years", y="Annual C02 emissions (in million tonnes)")

ggsave("emission.png", width = 20, height = 14, dpi = 300)  

#combined chart of total emissions and emissions per capita
#we first rename the countries in one df
annual_emission <- annual_emission %>% 
  mutate(country=recode(country,
                        "Egypt"="Egypt, Arab Rep.",
                        "South Korea"="Korea, Rep.",
                        "Russia"="Russian Federation",
                        "Turkey"="Turkiye")) 

data_joined <- annual_emission %>% 
  left_join(data_df_new, by = c("country", "year")) 

ggplot(data = data_joined  ,aes(x=year))+
  geom_line(aes(y=`Annual CO₂ emissions`),size = 1.1, alpha = 0.8, show.legend = F,  color="#42C2B8")+
  geom_line(aes(y=`CO2 emissions (metric tons per capita)`*100000000), size = 1.1, alpha = 0.8, show.legend = F, color= "#D6604D")+
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6),
                     name = "Annual C02 emissions (in million tonnes) (green)",
                     sec.axis =  sec_axis(trans = ~./1000000000, name = "CO2 emissions (metric tons per capita) (red)")
                     
                     )+
  scale_x_continuous(limits = c(1900, 2025), breaks = seq(1900, 2025, by = 20))+

  facet_wrap(~country, ncol=4, scales = "free_y")+
  theme_light()+
  theme(
    axis.text.x  = element_text(size = 15),
    axis.text.y  = element_text(size = 15),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    strip.text.x = element_text(size = 18, face="bold")
  )+
  labs(x="Years")

ggsave("combined_emission.png", width = 24, height = 15, dpi = 300)

#creating a wide dataframe to print as excel and create secondary axis chart in excel

data_joined_excel <- data_joined %>% 
  select(c("country", "year", `Annual CO₂ emissions`, "CO2 emissions (metric tons per capita)" )) %>% 
  pivot_longer(!c("country", "year"), names_to = "emissions", values_to = "value") %>% 
  pivot_wider(names_from = year, values_from = value) 

#write.xlsx(data_joined_excel, "data_joined_excel.xlsx")


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


#reading GDP per capita
gdp_1950 <- read.csv("gdp-per-capita-maddison-2020.csv") 

gdp_1940 <- gdp_1950 %>% 
  filter(Year>=1940) %>% 
  filter(Entity%in%c("China", "India", "Japan", "South Korea", "United States", 
                     "Brazil", "Nigeria", "Mexico", "Turkey","Russia", "Indonesia",
                     "Egypt", "South Africa")) %>% 
  select(c("Entity", "Year", "GDP.per.capita")) %>% 
  rename("country"="Entity") %>% 
  mutate(country=
           recode(country,
                  "South Korea"= "Korea, Rep.",
                  "Egypt"="Egypt, Arab Rep.",
                  "Russia"="Russian Federation",
           ))

gdp_1950_90 <- gdp_1940 %>% 
  filter(Year %in%c(1950, 1960, 1970, 1980, 1990)) 

gdp_1950_90 <- gdp_1950_90 %>% 
  mutate(Year=recode(Year,
                     `1950`=1940, 
                     `1960`=1950, 
                     `1970`=1960,
                     `1980`=1970, 
                     `1990`=1980)) %>% 
  rename("cohort"="Year")

#joining gdp per capita with inter generational data
intergeneration_mobility_gdp <- intergeneration_mobility %>% 
left_join(gdp_1950_90, by = c("country", "cohort")) 


intergeneration_mobility_gdp %>% 
  filter(country%in%c("China", "India", "Japan", "Korea, Rep.", "United States", 
                      "Brazil", "Nigeria", "Mexico", "Turkey","Russian Federation", "Indonesia",
                      "Egypt, Arab Rep.", "South Africa")) %>% 
  filter(parent=="avg") %>% 
  filter(child=="daughter") %>% 
  mutate(Relative_Mobility= 1-COR) %>% 
  select(c("country", "cohort", "CAT", "Relative_Mobility", "GDP.per.capita")) %>% 
  rename(Absolute_Mobility=CAT) %>% 
  pivot_longer(!c("country", "cohort", "GDP.per.capita"), names_to = "variable", values_to = "value") %>% 
  group_by(cohort) %>% 
  ggplot(aes(cohort, value, color=variable))+
  geom_point(size=2)+
  geom_line()+
  facet_wrap(~country, ncol=4, scales="free_y")+

  theme_light()+
  theme(
    axis.text.x  = element_text(size = 15),
    axis.text.y  = element_text(size = 15),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    strip.text.x = element_text(size = 18, face="bold")
  )+
  labs(x="Cohort", y="Probability")

ggsave("cohort_intergenerational.png", width = 22, height = 14, dpi = 300)  

intergeneration_mobility_gdp %>% 
  filter(country%in%c("China", "India", "Japan", "Korea, Rep.", "United States", 
                      "Brazil", "Nigeria", "Mexico", "Turkey","Russian Federation", "Indonesia",
                      "Egypt, Arab Rep.", "South Africa")) %>% 
  filter(parent=="avg") %>% 
  filter(child=="daughter") %>% 
  select(c("country", "cohort", "BHQ4")) %>%
  ggplot(aes(cohort, BHQ4))+
  geom_point(size=2, color="steelblue")+
  geom_line(color="steelblue")+
  facet_wrap(~country, ncol=4, scales="free_y")+
theme_light()+
  theme(
    axis.text.x  = element_text(size = 15),
    axis.text.y  = element_text(size = 15),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    legend.title = element_blank(),
    legend.text = element_blank(),
    strip.text.x = element_text(size = 18, face="bold")
  )+
  labs(x="Cohort", y="Pr child from bottom half ends up in Q4 (top quartile)")


intergeneration_mobility_gdp %>% 
  filter(country%in%c("China", "India", "Japan", "Korea, Rep.", "United States", 
                      "Brazil", "Nigeria", "Mexico", "Turkey","Russian Federation", "Indonesia",
                      "Egypt, Arab Rep.", "South Africa")) %>% 
  filter(parent=="avg") %>% 
  filter(child=="daughter") %>% 
  mutate(Relative_Mobility= 1-COR) %>% 
  select(c("country", "cohort", "CAT", "Relative_Mobility", "GDP.per.capita")) %>% 
  rename(Absolute_Mobility=CAT) %>% 
  pivot_longer(!c("country", "cohort", "GDP.per.capita"), names_to = "variable", values_to = "value") %>% 
  group_by(cohort) %>% 
  ggplot(aes(GDP.per.capita, value, color=variable))+
  geom_point(aes(size=cohort))+
  geom_line()+
  facet_wrap(~country, ncol=4, scales="free")+
  
  theme_light()+
  theme(
    axis.text.x  = element_text(size = 15),
    axis.text.y  = element_text(size = 15),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    strip.text.x = element_text(size = 18, face="bold")
  )+
  labs(x="GDP per capita (constant 2011 international-$)", y="Probability")


ggsave("gdp_intergenerational.png", width = 20, height = 14, dpi = 300)  

intergeneration_mobility_gdp <- intergeneration_mobility %>% 
  left_join(gdp_1950_90, by = c("country", "cohort")) 


intergeneration_mobility_gdp <- intergeneration_mobility_gdp %>% 
  filter(country%in%c("China", "India", "Japan", "Korea, Rep.", "United States", 
                      "Brazil", "Nigeria", "Mexico", "Turkey","Russian Federation", "Indonesia",
                      "Egypt, Arab Rep.", "South Africa")) %>% 
  filter(parent=="avg") %>% 
  filter(child=="daughter") %>% 
  mutate(Relative_Mobility= 1-COR) %>% 
  
  select(c("country", "cohort", "CAT", "Relative_Mobility", "GDP.per.capita", "BHQ4")) %>% 
  rename(Absolute_Mobility=CAT) 

write.xlsx(intergeneration_mobility_gdp, "intergeneration_mobility_gdp.xlsx")


#left_joining the data frames=====
poverty_rate <- poverty_rate %>% 
  mutate(country=recode(country,
                        "Egypt"="Egypt, Arab Rep.",
                        "South Korea"="Korea, Rep.",
                        "Russia"="Russian Federation",
                        "Turkey"="Turkiye")) %>% 
  rename(year=Year)
  
poverty_rate$year <- as.numeric(poverty_rate$year)

data_df_excel <- data_cor %>% 
  left_join(data_GDPpercapcor, by=c("country", "year")) %>% 
  left_join(poverty_rate, by=c("country", "year")) 

#convert long to wide format to create charts in excel
data_df_excel <- data_df_excel %>% 
  pivot_longer(!c("country", "year"), names_to = "variable", values_to = "value") %>% 
  pivot_wider(names_from = year, values_from = value) 

data_df_excel <- data_df_excel %>% 
  relocate(variable, .before = "country") 

write.xlsx(data_df_excel, "data_df_excel.xlsx")


#reading the SWIID data
load("swiid9_2.rda")

swiid_summary_df <- swiid_summary %>% 
  select(c("country", "year", "gini_disp", "gini_disp_se")) 


swiid_summary_excel <- swiid_summary_df %>% 
  pivot_longer(!c("country", "year"), names_to = "variable", values_to = "value") %>% 
  pivot_wider(names_from = year, values_from = value) %>% 
  pivot_longer(!c("country", "variable"), names_to = "year", values_to = "value") %>%
  mutate_at("year", as.numeric) %>% 
  filter(variable=="gini_disp") %>% 
  group_by(year) %>% 
  arrange(-desc(year)) %>% 
  pivot_wider(names_from = year, values_from = value) 

#write.xlsx(swiid_summary_excel, "gini_excel.xlsx")

  
#loading ILO data-------------- there is R api as well
#https://ilostat.github.io/Rilostat/articles/RilostatVisu.html  
ilo_labour <- readRDS("EMP_TEMP_SEX_ECO_NB_A-full-2022-07-31.rds")

ilo_labour_df <- ilo_labour %>% 
  select(c("ref_area.label", "sex.label", "classif1.label", "time", "obs_value")) %>% 
  rename(country=ref_area.label, emplyment_economic_activity=classif1.label, sex_label=sex.label, year=time,
         `obs_value(thousands)`=obs_value) %>%
  mutate_at("year", as.numeric) 

ilo_labour_excel <- ilo_labour_df %>% 
  filter(country%in%c("China", "India", "Japan", "Korea, Republic of", "United States", 
                      "Brazil", "Nigeria", "Mexico", "Türkiye","Russian Federation", "Indonesia",
                      "Egypt", "South Africa")) %>% 
  filter(emplyment_economic_activity%in%c("Economic activity (Broad sector): Total",
                                          "Economic activity (Broad sector): Agriculture",
                                          "Economic activity (Broad sector): Non-agriculture")) %>% 
  filter(sex_label%in% c("Sex: Total")) %>% 
  pivot_wider(names_from = "emplyment_economic_activity", values_from = "obs_value(thousands)") %>%
  mutate(agri_empmnt_share=`Economic activity (Broad sector): Agriculture`/`Economic activity (Broad sector): Total`,
         nonagri_empmnt_share=`Economic activity (Broad sector): Non-agriculture`/`Economic activity (Broad sector): Total`) %>% 
  select(c("country","sex_label", "year", "agri_empmnt_share", "nonagri_empmnt_share")) %>% 
  group_by(country) %>% 
  arrange(-desc(year)) %>% 
  pivot_longer(!c("country", "year", "sex_label"), names_to = "variable", values_to = "share") %>% 
  pivot_wider(names_from = year, values_from = `share`) 

write.xlsx(ilo_labour_excel,"ilo_labour_excel.xlsx")



#Labour force particpation (%)------
labour_participation <- read.csv("EAP_DWAP_SEX_AGE_RT_A-full-2022-07-31.csv")

labour_participation_excel <- labour_participation %>% 
    filter(classif1.label=="Age (Youth, adults): 15-64") %>% 
  select(c("ref_area.label", "sex.label", "time", "obs_value")) %>% 
  rename(country=ref_area.label,sex_label=sex.label, year=time,
         `participation_rate(%)`=obs_value) %>%
   mutate_at("year", as.numeric) %>% 
    group_by(country) %>% 
    arrange(-desc(year)) %>% 
   pivot_wider(names_from=year, values_from = `participation_rate(%)`) %>% 
  relocate(sex_label, .before=country) 

write.xlsx(labour_participation_excel, "labour_participation_excel.xlsx")


## Penn World Tables--------------------
tfp_penn <- read_xlsx("pwt100.xlsx", sheet = "Data")

tfp_penn_excel <- tfp_penn %>% 
  select(c("country", "year", "hc", "rtfpna")) %>% 
  pivot_longer(!c("country", "year"), names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from=year, values_from = value) %>% 
  filter(country%in%c("China", "India", "Japan", "Republic of Korea", "United States", 
                      "Brazil", "Nigeria", "Mexico", "Turkey","Russian Federation", "Indonesia",
                      "Egypt", "South Africa")) 
write.xlsx(tfp_penn_excel, "tfp_penn_excel.xlsx")

#WDI dependency ratio----
WDIsearch("age dependency")

indicator_age <- c("Age dependency ratio (% of working-age population)"="SP.POP.DPND")

data_dependency <- WDI(country = "all", indicator = indicator_age, start = 1960, end = 2020) %>% 
  as_tibble()

data_dependency_ratio <- data_dependency %>% 
  filter(country%in%c("China", "India", "Japan", "Korea, Rep.", "United States", 
                      "Brazil", "Nigeria", "Mexico", "Turkiye","Russian Federation", "Indonesia",
                      "Egypt, Arab Rep.", "South Africa")) 

dependency_ratio_excel <- data_dependency_ratio %>% 
  select(-"iso2c") %>% 
  group_by(country) %>% 
  arrange(-desc(year)) %>%
  pivot_wider(names_from=year, values_from = `Age dependency ratio (% of working-age population)`)

write.xlsx(dependency_ratio_excel, "dependency_ratio_excel.xlsx")
  
