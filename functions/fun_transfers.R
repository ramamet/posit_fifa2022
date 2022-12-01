
fun_players_transfer <- function(){

loan_df1 <- fifa_df %>%
           filter(!is.na(club_loaned_from) | club_loaned_from != "")

loan_df2 <- loan_df1 %>%
            select(short_name, value_eur, overall, club_name, club_loaned_from)%>%
            mutate(val_mil=round(value_eur/1e6, 2)) 

#?
fun_tab  <- function(club_var = 'Inter'){
temp1 <- loan_df2 %>% 
         filter(club_name %in% c(club_var)) %>%
         dplyr::select(short_name, val_mil, overall, club_loaned_from, club_name) %>%
         mutate(val_mil=paste0('€ ',val_mil,'M'))%>%
         rename(Name=short_name, OVP= overall, Value=val_mil,
                From=club_loaned_from, To=club_name)

knitr::kable(temp1, align = "llrcc", "html", caption = "Players Transfer") %>%
kable_styling()
}

edges <- loan_df2 %>%         
         group_by(club_loaned_from, club_name) %>%
         summarize(cnt=n(), 
         tot_val_mil = sum(val_mil, na.rm=TRUE),
          .groups = 'drop')%>%
         ungroup()%>%
         mutate(label=paste0('€ ',tot_val_mil,'M'))%>%
         rename(from=club_loaned_from, to=club_name, value=tot_val_mil) %>%
         mutate(color='#e73725')

##         
club_df <- data.frame(
           id= c(loan_df2$club_name, loan_df2$club_loaned_from))%>%
           distinct(id)   
     
list_dd1 <- list()

for(i in 1:nrow(club_df)){
list_dd1[[i]] <- fun_tab(club_df$id[i])
}

bind_club <- do.call(rbind, list_dd1)

nodes_df <- cbind(club_df, bind_club) %>%
            mutate(title = str_c('<div>',bind_club,'</div>'))%>%
            dplyr::select(-bind_club) %>%
            mutate(shape=c('circularImage')) %>%
            mutate(color = '#ffffff',
            shadow=TRUE)
 
#?
uniq_club <- players_22 %>% 
              dplyr::select(club_name, club_logo_url) %>%
              filter(club_name %in% c(nodes_df$id)) %>%
              distinct(club_name, club_logo_url)

nodes <- nodes_df %>%
        left_join(uniq_club, by=c('id'='club_name'))%>%
        rename(image=club_logo_url)
####
 visNetwork(nodes, edges,  height = "520px", width = "95%")  %>%
               visOptions(
                highlightNearest = list(enabled = T, degree = 1, hover = F)
                ) %>%
                visNodes(size=20, shapeProperties = list(useBorderWithImage = FALSE)) %>%
                visEdges(arrows = "to") %>%
                visLayout(randomSeed = 29) %>%
                visInteraction(navigationButtons = TRUE)          


}               