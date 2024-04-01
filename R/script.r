
library(dplyr)


speciesLink_mioto_genus <- function(genus, offset) {

  genus_name <- genus %>%
    stringr::str_to_lower()

  url <- glue::glue("https://specieslink.net/ws/1.0/search?genus={genus_name}&offset={offset}&limit=5000&apikey=hBVvfER8vHrcD6w9yLsb")

  df <- jsonlite::fromJSON(url)

  df2 <- df$features$properties %>%
    dplyr::tibble()

  return(df2)
}


characidium_all_1 <- speciesLink_mioto_genus("characidium", 0)
characidium_all_2 <- speciesLink_mioto_genus("characidium", 5000)

characidium_all_3 <- bind_rows(characidium_all_1,characidium_all_2)
View(characidium_all_1)


characidium_all_3 %>%
select(country, collectioncode, catalognumber, locality, scientificname,
decimallatitude, decimallongitude, modified)