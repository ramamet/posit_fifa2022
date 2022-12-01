#?

fun_compare_df <- function(){

compare_df <- fifa_df %>%
  arrange(desc(value_eur))%>%
  dplyr::select(short_name, club_name, league_name, height_cm, weight_kg, preferred_foot, c(pace:defending_sliding_tackle)) %>%
  dplyr::select(-starts_with('mentality_'))%>%
  slice(1:200) %>%
  pivot_longer(!c(short_name, club_name,league_name, height_cm, weight_kg, preferred_foot), names_to = "scores_grp", values_to = "scores_val")%>%
  rename(name_var = short_name, grp = scores_grp, value= scores_val)

#?
sdf2 <- SharedData$new(compare_df)  

gg1 <- g2(sdf2, asp(grp, value, color = name_var)) %>% 
  fig_area() %>% 
  fig_line(asp(size = 2)) %>% 
  fig_point(asp(size = 4, shape = "circle")) %>% 
  coord_type("polar", radius = .8) %>% 
  tooltip(
    shared = TRUE, 
    showCrosshairs = TRUE,
    crosshairs = list(
      line = list(
        style = list(
          lineDash = c(4, 4),
          stroke = "#333"
        )
      )
    )
  )


#?
widgets <- htmltools::div(
  br(),br(),
  div(class='div_name_var', filter_select("name_var", "Name (select)", sdf2, ~name_var)),
  div(class='div_name_var', filter_select("club_name", "Club (select)", sdf2, ~club_name)),
  div(class='div_name_var', filter_select("league_name", "League (select)", sdf2, ~league_name)),
  filter_checkbox("preferred_foot", "Preferred Foot", sdf2, ~preferred_foot, inline=TRUE),
  filter_slider("height_cm", "Height (cm)", sdf2, ~height_cm, width = "85%", step=1, ticks = FALSE),
  filter_slider("weight_kg", "Weight (Kg)", sdf2, ~weight_kg, width = "85%", step=1, ticks = FALSE)
)

# arrange
bscols(
  widths = c(4, 8),
  list(widgets), 
  list(
  div(class='div_ct_tit3', paste0('Top 200 Players by Value')),
  gg1)
)



}