---
Title: "Jail Deaths in America"
Author: "Eugene Ohba"
Date: "2022-12-05"
Output: html_document
---



### Introduction
Whenever thinking about safety, people can always come up with crime and prisoners, and the potential reason might be because one of the measurements that help to analyze the safety of a city or state is the average daily amount of prisoners in the jail. As people care about the amount of prisoners in the jail, the government not only needs to consider the amount of prisoners, but also need to manage the jail. For one aspect, they can’t make the jail very dangerous, which means the death rate in the jail is very high. In my analysis, I will be focusing on the state of Wisconsin. My primary question is the average death rate among Wisconsin prisoners higher than the average death rate among all US prisoners from 2008 to 2019? I will also consider if the average daily amount of prisoners in Wisconsin is the same or different than the average daily amount of prisoners in all other US jails from 2008 to 2019.

### Data
The data set I am analyzing was collected by Reuters journalists in 2020 ([reuters.com](https://www.reuters.com/investigates/special-report/usa-jails-graphic/)). It contains information about all reported inmate deaths in the largest US jails from 2008 to 2019. The original data set contains information about 253 US jails, where each row corresponds to a jail. The number of total deaths for that jail is broken down into years, from 2008 to 2019. The death counts are further broken down into the amount of deaths caused by certain causes of death. The causes of death included were suicide, acute drug or alcohol related deaths, illness/natural death, homicides, and other causes.The average daily inmate population per year, from 2008-2019, is also included for each jail, as well as the jails' medical provider(s) for each year.
For the purposes of my analyses, I will be using the total deaths and average daily inmate populations for each year. The processed data set contains information about the average prison for each state. Each row corresponds to one state where the data is averaged between all jails observed and all years of observation in that state. Columns for total deaths, and deaths by each type (suicide, drug/alcohol, etc). For example, In Alabama there are 12 jails, so I took the means of the death statistics (total deaths, suicide, drug/alcohol, etc) in each jail from years 2008-2019, and then I took the total mean of the 12 jails, to collect our variable mean. For some jails, there are some years with missing death rate data. I will compensate for this missing data by only taking the means of the years that are given. This could potentially affect my interpretation or results because I am taking smaller samples of those jails, and it has different proportions.


### Analysis 
```{r Setup, include = FALSE}
library(tidyverse)
library(modelr)
source("../scripts/viridis.R")
source("../scripts/ggprob.R")
all_jails = read_csv("all_states_data/all_jails.csv")
```

#### Comparing the States
```{r Data Processing, echo=FALSE, warning= FALSE}
prison_summaries <- all_jails %>% 
  select(-state_notes, -jail_notes, -c(med2008:med2019)) %>% 
  pivot_longer(cols = d2008:adp2019, names_to = "statistic", 
               values_to = "vals") %>% 
  drop_na() %>% 
  mutate(statistic = case_when(
    str_detect(statistic, "^d20") ~ "avg_yearly_total_deaths",
    str_detect(statistic, "^su2") ~ "avg_yearly_suicides",
    str_detect(statistic, "^da2") ~ "avg_yearly_drug_alcohol",
    str_detect(statistic, "^il2") ~ "avg_yearly_illness",
    str_detect(statistic, "^o20") ~ "avg_yearly_other",
    str_detect(statistic, "^ho2") ~ "avg_yearly_homocides",
    str_detect(statistic, "^ac2") ~ "avg_yearly_accidents",
    str_detect(statistic, "^adp2") ~ "avg_inmate_pop"
  )) %>% 
  group_by(statecode, state, county, jail, statistic) %>% 
  summarize(avgs = mean(vals)) %>% 
  pivot_wider(names_from = statistic, values_from = avgs)
  
## prison summaries:
# get rid of unneeded columns
# pivot longer to do across column calculations
# rename the observation titles to allow for grouping
# summarize by the different observations, all in averages
# pivot wider to get to one row for each prison

state_summaries <- prison_summaries %>% 
  group_by(statecode, state) %>% 
  drop_na() %>% 
  summarize(total_inmate_population = sum(avg_inmate_pop), avg_inmate_pop = mean(avg_inmate_pop),
            avg_yearly_accidents = mean(avg_yearly_accidents), 
            avg_yearly_drug_alcohol = mean(avg_yearly_drug_alcohol), 
            avg_yearly_homocides = mean(avg_yearly_homocides), 
            avg_yearly_illness = mean(avg_yearly_illness),
            avg_yearly_other = mean(avg_yearly_other), 
            avg_yearly_suicides = mean(avg_yearly_suicides),
            avg_yearly_total_deaths = mean(avg_yearly_total_deaths),
            death_rate = avg_yearly_total_deaths/avg_inmate_pop)

##State summaries:
# group by statecode and state
# drop na values to allow for summarizing
# summarize averages of each column for each jail in a state
 
ggplot(state_summaries, aes(x = reorder(state, -100*(death_rate)), 
                            y = 100*(death_rate))) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  xlab("State") + 
  ylab("Average Yearly Inmate Percent Death Rate") + 
  ggtitle("Average Yearly Inmate Death Rate by State",
          subtitle = "2008-2019")
       


```
![image](https://github.com/Eugenefut19/Eugene-Portfolio/assets/134546229/b733d2dd-044b-438a-80ae-b943ae2c51a6)

This graph shows that Nevada had the highest average death rate over the course of the data collection, shown by my ordered bar graph. This bar graph gives me some information on how different states’ prisons compare to each other and which might be safer. Overall we see that Nevada, Oklahoma, Maryland, and DC have the highest death rates for inmates; and then Wisconsin, Kentucky, Iowa, Alabama, and South Dakota. This shows us that Wisconsin is actually in the 5 states with lower death rates compared to the other 45. 

#### Correlation of Population and Death Rate

## `geom_smooth()` using formula 'y ~ x'
```{r Linear Modeling, echo=FALSE, warning= FALSE}
state_prison_lm = lm(death_rate ~ avg_inmate_pop, data = state_summaries)

summary(state_prison_lm)

ggplot(state_summaries, aes(x = avg_inmate_pop, y = death_rate)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm") +
  xlab("Average Yearly Inmate Population") +
  ylab("Average Yearly Inmate Death Rate") +
  ggtitle("Linear Model Death Rate vs Population")
```
![image](https://github.com/Eugenefut19/Eugene-Portfolio/assets/134546229/86bbef92-b155-4538-aa92-4c1c0595f653)

## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```{r}
state_summaries %>% 
  add_residuals(state_prison_lm) %>% 
  ggplot(aes(x = avg_inmate_pop, y = resid)) +
  geom_point() + 
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  xlab("Average Yearly Inmate Population") +
  ggtitle("Residual plot to check the linear model")
```
![image](https://github.com/Eugenefut19/Eugene-Portfolio/assets/134546229/90dc9647-6c10-4fbf-9c2a-37b545b526c1)

Although this linear model shows a slight negative correlation between inmate population and death rate, as well as having a good residual plot with consistent variance and mean of zero, the test to see if the slope is significantly different than zero was not able to reject the null (p = 0.7601, t-test, df = 42). So we are able to say that there is no clear evidence that death rate increases or decreases with an change in inmate population. This tells us that differences in death rate are not necessarily due to differences in inmate populations but are actually more likely due to how different states treat prisoners and the conditions they live in. 


#### Hypothysis Test

```{r, include=FALSE}
state_summaries %>% 
  filter(statecode == "WI") %>% 
  select(avg_inmate_pop)
```

$$ X1∣p∼Binomial(707,p) $$

$$
H_0: p = 0.010751 \\
H_a: p > 0.010751
$$

```{r Hypothysis Test, echo=FALSE, warning = FALSE}
test_df <- state_summaries %>%
  filter(statecode == "WI") %>%
  select(avg_inmate_pop, avg_yearly_total_deaths) %>%
  summarize(prison_deaths = avg_yearly_total_deaths, 
            p_tilde = (prison_deaths+2)/(avg_inmate_pop+4), 
            se = sqrt((p_tilde*(1-p_tilde))/(avg_inmate_pop+4)),
            z_score = (p_tilde - 0.010751) / se,
            p_value = pnorm(z_score, lower.tail = FALSE)
            )
test_df
```

We want to test if the average yearly death rate for Wisconsin prisons in this data set is greater than that of the general population of Wisconsin, which is 1,075.1 per 100,000 ([cdc.gov](https://www.cdc.gov/nchs/pressroom/states/wisconsin/wisconsin.htm)). Since our data is per capita and we need to do a test comparing proportions, we needed to scale down the state death rate by 100000, giving us a death rate of 0.010751. We were not able to reject the null hypothesis, the average yearly death rate for Wisconsin prisons in this data set was not significantly greater than the Wisconsin death rate (p = 0.99).



### Discussion

In regards to comparing Wisconsin inmate versus civilian deaths, the hypothesis test I have conducted showed that there was no significant statistical information to suggest that Wisconsin inmate deaths occur more often than civilian deaths. A broader interpretation of this conclusion could be that Wisconsin jails aren’t mistreating their inmates, which could cause them to die more often than regular civilians. Other states with higher death rates would suggest that something about the jails is putting the inmates more at risk of death. Additionally, I have found that Wisconsin had some of the lowest death rates among inmates among all states in the US. This could potentially explain why we don’t see noticeably higher deaths among Wisconsin inmates vs civilians, as the inmate deaths occur at a very low rate. A potential shortcoming of the analysis would be a lack of complete data for every jail. For whatever reason, my source didn’t track every jail for the same time frame. Some jails started being tracked earlier than others and some stopped being tracked before the last year of data collection. Another short-coming that has arisen from our analysis is that the death rate data for Wisconsin civilians was taken from 2020. The data set I have used for the jails ends in 2019, so COVID-19 hadn’t yet had an impact on the jail data. The death rate for the Wisconsin jails would likely be slightly higher due to the prevalence of COVID in 2020. Regardless, the p value being 0.99 means that this short-coming probably won’t have much of an effect on the data. A potential future direction for additional work could involve working with the types of deaths found in the jails. The source I have used for this analysis also included data about how the prisoners died, so that could be something to pivot towards. My hypothesis test concluded that there wasn’t sufficient evidence to say that Wisconsin jails have a higher death rate than the general population. To find something statistically significant about Wisconsin jails, the next step would be to look at specific causes of deaths in the jails and see if there’s a certain type of death more prevalent in comparison to other states. Overall we have found that, in terms of overall death rate, Wisconsin jails are fairly safe not only compared to other US states, but also compared to Wisconsin's overall death rate.




