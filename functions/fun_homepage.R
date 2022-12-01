library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)
library(here)
library(reactable)
library(reactablefmtr)
library(htmltools)
library(crosstalk)
library(echarts4r)
library(echarty) 
library(ggplot2)
library(sunburstR)
library(d3r)
library(g2r)
library(colorspace)
library(kableExtra)
library(visNetwork)

##
#?
tot_country_value_B <-  fifa_df %>%
                        group_by(nationality_name)%>%
                        summarize(tot_val = sum(value_eur, na.rm=TRUE))%>%
                        ungroup()%>%
                        mutate(tot_val_B = round(tot_val/1e9, 2))%>%
                        arrange(desc(tot_val_B)) %>%
                        #slice(1:10) %>%
                        pull(tot_val_B)%>%
                        sum()
#?
tot_numb_players <- fifa_df %>%
                    distinct(sofifa_id)%>%
                    pull(sofifa_id)%>%
                    length()

#?
avg_val <- fifa_df %>% pull(value_eur) %>% mean(na.rm=TRUE)
avg_val_mil <- round((avg_val/1e6),2)

avg_wage <- fifa_df %>% pull(wage_eur) %>% mean(na.rm=TRUE)
avg_wage_K <- round((avg_wage/1e3),2)

#?
skill_dd1 <- fifa_df %>%
             select(short_name, c(pace:goalkeeping_speed))

skill_dd1$tot_scores <- rowSums(skill_dd1[,c(-1)], na.rm=TRUE)            
top_pointer <- skill_dd1 %>% 
               arrange(desc(tot_scores)) %>% 
               select(short_name, tot_scores)%>%
               slice(1)
#?
gp1 <- fifa_df %>%
       select(sofifa_id, club_position, value_eur, wage_eur, age, preferred_foot) %>%
        mutate(value_mil= value_eur/(1e6)) %>%
        mutate(wage_K = wage_eur/1e3) %>%
        mutate(age_bins = cut(age, breaks = c(16,18,23,30,35,38,45))) %>%
        left_join(club_pos, by = c("club_position" = "club_position"))

post_pp2 <- gp1 %>%
          group_by(position_group) %>%
          summarize(cnt=n(), 
          avg_value_mil=mean(value_mil, na.rm=TRUE),
          avg_wage_K=mean(wage_K, na.rm=TRUE),
           .groups = 'drop'
          ) %>%
          ungroup()          

#   position_group        cnt avg_value_mil avg_wage_K
#   <chr>               <int>         <dbl>      <dbl>
# 1 Attacking Positions    96          32.6       75.7
# 2 Defensive Positions   211          29.0       69.3
# 3 Midfield Positions    570          27.3       70.8

#?
loan_df1 <- fifa_df %>%
            filter(!is.na(club_loaned_from) | club_loaned_from != "")%>%
            select(short_name, value_eur, overall, club_name, club_loaned_from)%>%
            mutate(val_mil=round(value_eur/1e6, 2)) %>%
            select(-value_eur)%>%
            rename(Name=short_name, OVP= overall, Value=val_mil,
                   From=club_loaned_from, To=club_name)%>%
                   arrange(desc(Value))
         