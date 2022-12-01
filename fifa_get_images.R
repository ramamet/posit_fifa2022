library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(here)

## data ref: https://www.kaggle.com/datasets/stefanoleone992/fifa-22-complete-player-dataset
players_22 <-read_csv("./data/players_22.csv", locale = locale(encoding = "UTF-8"))

fifa_df <- players_22 %>%
           filter(wage_eur>22000 & overall > 75 & value_eur >= 10e6)

fifa_top <- fifa_df %>% 
           dplyr::select(sofifa_id, short_name, age, overall, potential, club_name, club_joined,
           club_contract_valid_until,  value_eur, wage_eur, player_face_url, club_logo_url, nation_flag_url) %>%
           mutate(club_joined=ymd(club_joined))%>%
           mutate(club_joined_yr = lubridate::year(club_joined)) %>%
           mutate(contract = paste0(club_joined_yr, ' - ', club_contract_valid_until)) %>%
           dplyr::select(-club_joined, -club_joined_yr, -club_contract_valid_until) %>%
           dplyr::relocate(contract, .after=club_name)%>%
           arrange(desc(value_eur))

ps1 <- fifa_top %>% 
       slice(1:200)%>%
       as_tibble()

# write.csv(ps1, './data/top200_overall.csv', row.names=FALSE)

NR <- nrow(ps1) 

##
# url1 <- ps1$player_face_url[1]
# download.file(url1, "test.png", mode = "wb")

### get player face images
fun_player_image <- function(row_id=1) {
url1 <- ps1$player_face_url[row_id]
sofifa_id <- ps1$sofifa_id[row_id]
file_name <- glue::glue('./data/player_images/', sofifa_id, '.png')
download.file(url1, file_name, mode = "wb")
}


for(i in 1:NR){
    print(i)
    fun_player_image(i)
    print('done!')  
}

###
### get club_logo_url
fun_club_logo <- function(row_id=1) {
url1 <- ps1$club_logo_url[row_id]
sofifa_id <- ps1$sofifa_id[row_id]
file_name <- glue::glue('./data/club_logo/', sofifa_id, '.png')
download.file(url1, file_name, mode = "wb")
}


for(i in 1:NR){
    print(i)
    fun_club_logo(i)
    print('done!')  
}

###

###
### get national flag
#ps2 <- dd1 %>% arrange(desc(value_eur))
fun_flag <- function(row_id=1) {
url1 <- ps1$nation_flag_url[row_id]
sofifa_id <- ps1$sofifa_id[row_id]
file_name <- glue::glue('./data/national_flag/', sofifa_id, '.png')
download.file(url1, file_name, mode = "wb")
}


for(i in 1:NR){
    print(i)
    fun_flag(i)
    print('done!')  
}
