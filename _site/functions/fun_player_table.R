##
#?
get_score_color1 <- function(score) {
  blue_pal <- function(x) rgb(colorRamp(rev(colorspace_seq1))(x), maxColorValue = 255)
  #normalized <- (score - min(score)) / (max(score) - min(score))
  normalized <- (score) / 100
  blue_pal(normalized)
}

get_score_color2 <- function(score) {
  blue_pal <- function(x) rgb(colorRamp(rev(colorspace_seq2))(x), maxColorValue = 255)
  #normalized <- (score - min(score)) / (max(score) - min(score))
  normalized <- (score) / 100
  blue_pal(normalized)
}

# Show the user score in a donut chart like TMDb does. Since donut charts
# are hard to compare, apply a color scale as well.
# donut
fun_donut1 <- function(){
  JS("function(cellInfo) {
        const sliceColor = cellInfo.row['score_color1']
        const sliceLength = 2 * Math.PI * 24
        const sliceOffset = sliceLength * (1 - cellInfo.value/100)
        const donutChart = (
          '<svg width=60 height=60 style=\"transform: rotate(-90deg)\" focusable=false>' +
            '<circle cx=30 cy=30 r=24 fill=none stroke-width=6 stroke=#e5e5e5 box-shadow= 10px></circle>' +
            '<circle cx=30 cy=30 r=24 fill=none stroke-width=6 stroke=' + sliceColor +
            ' stroke-dasharray=' + sliceLength + ' stroke-dashoffset=' + sliceOffset + '></circle>' +
          '</svg>'
        )
        const label = '<div style=\"position: absolute; top: 50%; left: 50%; font-size:12px; ' +
          'transform: translate(-50%, -50%)\">' + cellInfo.value + '</div>'
        return '<div style=\"display: inline-flex; position: relative\">' + donutChart + label + '</div>'
      }")
}

fun_donut2 <- function(){
  JS("function(cellInfo) {
        const sliceColor = cellInfo.row['score_color2']
        const sliceLength = 2 * Math.PI * 24
        const sliceOffset = sliceLength * (1 - cellInfo.value/100)
        const donutChart = (
          '<svg width=60 height=60 style=\"transform: rotate(-90deg)\" focusable=false>' +
            '<circle cx=30 cy=30 r=24 fill=none stroke-width=6 stroke=#e5e5e5></circle>' +
            '<circle cx=30 cy=30 r=24 fill=none stroke-width=6 stroke=' + sliceColor +
            ' stroke-dasharray=' + sliceLength + ' stroke-dashoffset=' + sliceOffset + '></circle>' +
          '</svg>'
        )
        const label = '<div style=\"position: absolute; top: 50%; left: 50%; font-size:12px; ' +
          'transform: translate(-50%, -50%)\">' + cellInfo.value + '</div>'
        return '<div style=\"display: inline-flex; position: relative\">' + donutChart + label + '</div>'
      }")
}

###################

fun_data <- function(){

sub1 <- fifa_df %>% 
           dplyr::select(sofifa_id, short_name, age, overall, potential, club_name, club_joined,
           club_contract_valid_until,  value_eur, wage_eur, 
           pace, shooting, passing,dribbling, defending, physic) %>%
           mutate(club_joined=ymd(club_joined))%>%
           mutate(club_joined_yr = lubridate::year(club_joined)) %>%
           mutate(contract = paste0(club_joined_yr, ' ~ ', club_contract_valid_until)) %>%
           dplyr::select(-club_joined, -club_joined_yr, -club_contract_valid_until) %>%
           dplyr::relocate(contract, .after=club_name)%>%
           arrange(desc(value_eur))
     
sub2 <- sub1 %>% 
       #arrange(desc(overall)) %>%
       slice(1:200) %>%
       #slice(1:20) %>%
       as_tibble()

sub3 <- sub2 %>%
 mutate(score_color1 = get_score_color1(overall)) %>%
 mutate(score_color2 = get_score_color2(potential)) %>%
 mutate(name_flag = paste0(sofifa_id,'_',short_name)) %>%
 mutate(club_contract = paste0(sofifa_id, '_', club_name,'_',contract)) %>%
 relocate(name_flag, .after=sofifa_id)%>%
 select(-contract) 

return(sub3)

}

#?

react_theme = reactableTheme( 
    headerStyle = list(color = "#bbb8b5", fontSize = '14px'))

#?
fun_ct_tab <- function(fifa_data = dat) {


react_tab <- fifa_data %>% 
 reactable(
  columns = list(              
             sofifa_id = colDef(
                name = '',
                cell = function(value) {   
                img_src <- knitr::image_uri(sprintf("./data/player_images/%s.png", value))
                image <- img(src = img_src, height = "60px", alt = "")
                tagList(
                  tags$div(class='div_player_image',style = "display: inline-block; width: 64px;", image)
                )
              },
              width = 70
              )
             , age = colDef(
                    name = 'Age',
                    width = 60,
                    align = "center",
                    cell= function(value){
                        tagList(div(class='div_txt_age2',value))
                    }
                )
               , overall = colDef(
                name = "OVA",
                defaultSortOrder = "desc",
                cell = fun_donut1() ,
                html = TRUE,
                align = "center",
                width = 90,
                class = "user-score"
                )
                , potential = colDef(
                name = "POT",
                defaultSortOrder = "desc",
                cell = fun_donut2() ,
                html = TRUE,
                align = "center",
                width = 90,
                class = "user-score"
                )
              , value_eur = colDef(
                    name = "Value",
                    cell = function(value) {
                    value <- format(round(value / 1e6, 1), nsmall = 1)
                    tagList(div(class='div_txt_value2', glue::glue('€',value), span(class = "units", "M")))
                    } 
                    , align = "center"
                    , maxWidth = 80
                    )
              , wage_eur = colDef(
                    name = "Wage",
                    cell = function(value) {
                    value <- format(round(value / 1e3, 1), nsmall = 0)
                    tagList(div(class='div_txt_wage2', glue::glue('€',value), span(class = "units", "K")))
                    } 
                    , align = "center"
                    , maxWidth = 80
                    )       
        
                , score_color1 = colDef(show = FALSE)
                , score_color2 = colDef(show = FALSE)  
                , club_name = colDef(show = FALSE)   
                , short_name = colDef(show = FALSE)   
                , shooting = colDef(show = FALSE)
                , passing = colDef(show = FALSE)
                , dribbling = colDef(show = FALSE)
                , defending= colDef(show = FALSE)
                , physic = colDef(show = FALSE)
                , pace = colDef(show = FALSE)  
                , name_flag = colDef(     
                name = 'Name',         
                cell = function(value) {   
                player_id <- word(value, 1, sep = "_") 
                player_name <- word(value, 2, sep = "_")   
                img_src <- knitr::image_uri(sprintf("./data/national_flag/%s.png", player_id))
                image <- img(src = img_src, height = "24px", alt = "")
                tagList( div(class='div_pl_country_back',
                  tags$div(class = "div_pl_name", player_name),
                  tags$div(style = "margin-top:2%; display: inline-block; width: 64px;", image)
                ))
              },
              width = 125
              )
              , club_contract = colDef(   
                name = 'Team & Contract',           
                cell = function(value) {  
                player_id <- word(value, 1, sep = "_")      
                club <- word(value, 2, sep = "_") 
                contract <- word(value, 3, sep = "_")  
                img_src <- knitr::image_uri(sprintf("./data/club_logo/%s.png", player_id))
                image <- img(src = img_src, height = "50px", alt = "") 
                tagList(
                  tags$div(class='div_club_cont_back',style = "white-space: nowrap;",
                  tags$div(style = "display: inline-block;   
                    height: 50px;
                    width:  50px;
                    border-radius: 50%;
                    margin-top: 0%;
                    margin-left: -2%;
                    vertical-align: middle", image) , 
                  tags$div(style = "
                  display: inline-block;
                   width: calc(100% - 60px); 
                    white-space: normal;
                    margin-top:-4%;
                    vertical-align: middle
                  ", tags$div(class = "div_club_name", club)
                  ,  tags$div(class = "div_contract", contract)))
                )
              },
              width = 190,
              align = "center"
          
              )
  )  
  , searchable = FALSE
  , compact = TRUE
  , defaultColDef = colDef(vAlign = "center", headerClass = "header")
  , fullWidth = TRUE
  , defaultPageSize = 5
  , showPageSizeOptions = TRUE,
  , pageSizeOptions = c(5, 10, 20)
  , details = row_details
  #? reactable
  )
   
}

