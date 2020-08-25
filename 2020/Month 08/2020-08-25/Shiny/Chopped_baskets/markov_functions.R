#' fetch the data from the repo
#' 
#' @return a tbl.
get_data <- function(){
  chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')
  return(chopped)
}

#' Clean the data 
#' 
#' @param .data a data frame with column items
#' 
#' @return a tbl
clean_ingredients <- function(.data){
  
  .data %>%
    mutate(items = tolower(items),
           items = str_remove(items, "baby"),
           items = str_remove(items, "jumbo"),
           items = str_remove(items, "assorted"),
           items = str_remove(items, "\\)"),
           items = str_remove(items, "\\("),
           items = str_replace(items, "&",                 "and"),
           items = str_replace(items, "jalapeno",          "jalapeño"),
           items = str_replace(items, "halloumi cheese",   "halloumi"),
           items = str_replace(items, "onions",            "onion"),
           items = str_replace(items, "carcasses",         "carcass"),
           items = str_replace(items, "chocolate covered", "chocolate-covered"),
           items = str_replace(items, "sausages",          "sausage"),
           items = str_replace(items, "steaks",            "steak"),
           items = str_replace(items, "cucumbers",         "cucumber"),
           items = str_replace(items, "country style",     "country-style"),
           items = str_replace(items, "avocados",          "avocado"),
           items = str_replace(items, "french fried",      "french-fried"),
           items = str_replace(items, "tomatoes",          "tomato"),
           items = str_replace(items, "breasts",           "breast"),
           items = str_replace(items, "chops",             "chop"),
           items = str_replace(items, "hot cocoa mix",     "hot chocolate mix"),
           items = str_replace(items, "chew candy",        "chews"),
           items = str_replace(items, "eggs",              "egg"),
           items = str_replace(items, "coquitos",          "coquito"),
           items = str_replace(items, "bread and butter",  "bread-and-butter"),
           items = trimws(items))
}


#' Get and clean apps
#' 
#' @param chopped a tbl made from get_data
#' 
#' @return a tbl.
get_apps <- function(chopped = get_data()){
  appetizer_ingredients <- chopped %>%
    select(season, season_episode, series_episode, appetizer) %>%
    separate( appetizer, into = c("ing1","ing2","ing3","ing4"), sep = ",") %>%
    pivot_longer( 
      cols = starts_with("ing"),
      names_to = "item_order", 
      values_to = "items"
    )%>%
    drop_na(items) %>%
    clean_ingredients() %>%
    mutate(dish_type = "appetizer") 
  
  return(appetizer_ingredients)
}


#' Get and clean entre ingredients 
#' 
#' @param chopped a tbl made from get_data
#' 
#' @return a tbl.

get_entres <- function(chopped = get_data()){
  entre_ingredients <- chopped %>%
    select(season, season_episode, series_episode, entree) %>%
    separate( entree, into = c("ing1","ing2","ing3","ing4", "ing5", "ing6"), sep = ",") %>%
    pivot_longer( 
      cols = starts_with("ing"),
      names_to = "item_order", 
      values_to = "items"
    )%>%
    drop_na(items) %>%
    clean_ingredients() %>%
    mutate(dish_type = "entre")
  
  return(entre_ingredients)
  
}


#' Get and clean dessert ingredients
#' 
#' @param chopped a tbl made from get_data
#' 
#' @return a tbl.
#' 
get_dessert <- function(){
  dessert_ingredients <- chopped %>%
    select(season, season_episode, series_episode, dessert) %>%
    separate( dessert, into = c("ing1","ing2","ing3","ing4", "ing5", "ing6"), sep = ",") %>%
    pivot_longer( 
      cols = starts_with("ing"),
      names_to = "item_order", 
      values_to = "items"
    )%>%
    drop_na(items) %>%
    clean_ingredients() %>%
    mutate(dish_type = "dessert")
  
  return(dessert_ingredients)
}

#' Get all three courses as one DF
#' 
#' @return a tbl with apps, entres, and desserts
get_3_course <- function(){
  all_ingredients <- appetizer_ingredients %>%
    rbind(entre_ingredients) %>%
    rbind(dessert_ingredients)
  return(all_ingredients)
}




raw_text <- all_ingredients %>% 
  pivot_wider(values_from = items, 
              names_from = item_order) %>%
  unite(col = "list", starts_with("ing"), sep = " ", na.rm = TRUE) %>%
  unite(col = "text", dish_type, list, sep = " ") %>%
  mutate(text = tolower(text) %>%
           replace_white() 
  ) %>%
  pull(text) %>% 
  strsplit(" ") 


fit_markov <- markovchainFit(raw_text)

create_basket <- function(num = 1, first_word = "appetizer", n_ingredients = 5){
  
  for(i in 1:num){
    
    basket <- markovchainSequence(n = n_ingredients, 
                                  markovchain = fit_markov$estimate,
                                  t0 = tolower(first_word), include.t0 = FALSE) %>%
      paste(collapse = " ") 
    
  }
  
  return(basket)
  
}

