# library(dplyr)
# library(tidyr)
# library(readr)
# library(lubridate)
# library(stringr)
# library(echarts4r)
# library(here)
# library(reactable)
# library(reactablefmtr)
# library(colorspace)
# library(htmltools)
# library(crosstalk)
# library(echarts4r)
# library(apexcharter)
# library(echarty) 

# ###
# players_22 <-read_csv("./data/players_22.csv", locale = locale(encoding = "UTF-8"))

# fifa_df <- players_22 %>%
#            arrange(desc(overall)) %>%
#            filter(wage_eur>22000 & overall > 75 & value_eur >= 10000000)

###
fun_sun2b_fifa <- function(){

gp1 <- fifa_df %>%
       select(sofifa_id, club_position, value_eur, wage_eur, age, preferred_foot) %>%
        mutate(value_mil= value_eur/(1e6)) %>%
        mutate(wage_K = wage_eur/1e3) %>%
        mutate(age_bins = cut(age, breaks = c(16,18,23,30,35,38,45)))  

pl_pos_df1 <- gp1 %>%
              left_join(club_pos, by = c("club_position" = "club_position"))

post_pp2 <- pl_pos_df1 %>%
          group_by(position_group, position_exp, age_bins, preferred_foot) %>%
          summarize(cnt=n(), 
          avg_value_mil=mean(value_mil, na.rm=TRUE),
          avg_wage_K=mean(wage_K, na.rm=TRUE),
           .groups = 'drop'
          ) %>%
          ungroup()         

pp2_sub <- post_pp2 %>% 
          select(position_group, position_exp, age_bins, preferred_foot, cnt) %>%
           rename(level1=position_group, level2=position_exp, level3=age_bins, 
           level4=preferred_foot, size=cnt)

tree <- d3_nest(pp2_sub, value_cols = "size")

colorspace_seq <- qualitative_hcl(8, palette = "Warm")
sb1 <-  sund2b(
  tree,
  #colors = list(range = RColorBrewer::brewer.pal(9, "Set3")),
  #colors = colorspace_seq,
  width = "95%"
  #height = 700
)

}


#############
fun_position_cor <- function(){

cor_data <- fifa_df %>%
            dplyr::select(c(overall, potential, pace:defending_sliding_tackle)) %>%
            dplyr::select(-starts_with('mentality_'))%>%
            cor(use = "complete.obs") 
            
cor_data<-round(cor_data,4)

cor_data %>% 
  e_charts() %>% 
  e_correlations(
    visual_map = FALSE,
    order = "hclust",
    itemStyle = list(
      borderWidth = 2,
      borderColor = "#fff"
    ),
       emphasis=list(
        itemStyle=list(
          shadowBlur= 10,
          shadowColor= 'rgba(0, 0, 0, 0.5)'
       )
       )
  ) %>% 
  e_x_axis_(axisLabel=list(interval= 0, rotate= 45, fontSize= 7))%>%
  e_y_axis_(axisLabel=list(fontSize= 7))%>%
  e_grid(left= '25%', bottom='25%')%>%
    e_visual_map(
    min= -1,
    max= 1,
    calculable= TRUE,
    orient= 'horizontal',
    left= 'center',
    bottom= '2%',
    inRange = list(color = c('#E186A7', '#D6D2E3','#82BDC9')), # scale colors
    )%>%
      e_tooltip(formatter = htmlwidgets::JS("
      function(params){
        return(params.value[0] + '<br />' + params.value[1] + ' : '+ params.value[2])
      }
    ") 
     )
    #e_tooltip() #%>%
    #e_title("Correlation") 

}  