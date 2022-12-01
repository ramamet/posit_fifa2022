#?
fun_gg <- function(id) {

sub1 <- df[id,]
sub2 <- sub1 %>% dplyr::select(int_crossing:int_long_shots)
sub3 <- sub2 %>% 
        pivot_longer(names_to='rating', values_to='val',
        int_crossing:int_long_shots) %>%
        mutate(rating=gsub('int_','',rating))

g2(sub3, asp(rating, val)) %>% 
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
          lineDash = c(3),
          stroke = "#e63946"
        )
      )
    )
  ) %>%
   gauge_label(textStyle = list(rotate = 30))%>%   

    info_text(
    position = c("50%", "50%"),
    content = glue::glue(df$str_player_name[id]),
    style = list(
      textAlign = "center",
      fontSize = 20,
      fill="#ef476f",
      fillOpacity= 0.5,
      textBaseline= 'bottom'
    )
  )


}

####
fun_card_container2 <- function(img_name='life.svg', user_name='Ram', id=1) {
  
  div(class= 'card',
        div(class='front',
            div(class = "card-img-top", img(src = paste0("./data/images/",{img_name},'.png',sep=""))),
            div(class='card-body text-center',
            div(class = 'card-title', (glue::glue({paste0(df$int_overall_rating[id],' (',df$str_nationality[id],')')})))            
            )
        ),
        div(class='back', fun_gg(id))
       )
}

####

fun_card <- function(i){
fun_card_container2(img_name=df$int_player_id[i], user_name=df$str_player_name[i], id=i)
}

#? nov 2022

fun_nat_pl_positions <- function(country = 'Germany'){

nat_pos_df <- fifa_df %>%
            left_join(club_pos, by = c("club_position" = "club_position")) %>%
            filter(nationality_name %in% c(country))%>%
            mutate(value_mil= round(value_eur/1e6, 2),
            wage_K= round(wage_eur/1e3, 2))%>%
            arrange(desc(value_mil)) %>%
            select(short_name, value_mil, overall, club_name, club_position, position_exp, position_group)


sdf3 <- SharedData$new(nat_pos_df)

field_check <- filter_checkbox("position_group", NULL, sdf3, ~position_group, inline=TRUE)

sp1 <- sdf3 %>%
        reactable( 
          columns = list(     
          short_name = colDef(name = "Name",
                html = TRUE,
                align = "left",
                width = 90),
          value_mil = colDef(
                    name = "Value",
                    cell = function(value) {
                    tagList(div(class='div_txt_value2', glue::glue('€',value), span(class = "units", "M")))
                    },
                    align = "left",
                    width = 50
                    ),
          club_name = colDef(name = "Club", align = "left", width = 90),
          overall = colDef(name = "OVP", align = "center", width = 50),
          club_position = colDef(
                name = 'Position',
                cell = function(value) {   
                img_src <- knitr::image_uri(sprintf("./data/positions/%s.svg", value))
                image <- img(src = img_src, height = "20px", alt = "")
                tagList(
                  tags$div(style = "width:50%;display:block;float:left;", image),
                  tags$div(style = "width:50%;display:block;float:left;", value)                  
                )
              },
              width = 60
              ),
          position_group = colDef(show=FALSE),
          position_exp = colDef(show = FALSE)
          ),  
          
        pagination = FALSE, 
        bordered = TRUE, striped = TRUE, 
        highlight = TRUE,
        compact = TRUE,
        defaultColDef = colDef(headerClass = "line-score-header", align = "center", minWidth = 50),
        style = list(fontSize = '8px',
        fontFamily = "Work Sans, Helvetica Neue, Helvetica, Arial, sans-serif",
         maxWidth = 380),
        height = 350
        )  


styl1 <- 'width:80%;display:block;float:center;font-size:8px;'
crosstalk::bscols( div(class='div_tab_card_back',
  list(
   div(style=styl1,field_check),  
   sp1)
)
)
}

##
fun_card_country <- function(id=1) {

   div(class= 'card',
        div(class='front',
            div(class = "card-img-top", img(src = paste0("./data/flag/",nat_df$nationality_abb[id],'.png',sep="")))
            ,div(class='card-body text-center'
            ,div(class = 'card-title',
             (glue::glue({paste0(nat_df$nationality_name[id],' (€',nat_df$tot_val_B[id],'B)')})))            
            )
        )
        ,div(class='back', fun_nat_pl_positions(nat_df$nationality_name[id]))
       )     

}

###