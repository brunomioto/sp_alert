library(telegram)
library(dplyr)
library(readr)
library(stringr)
library(glue)
library(jsonlite)

#connect spalert_bot
bot <- TGBot$new(token = bot_token('SPALERT_BOT'))

bot$set_default_chat_id(302461245)


speciesLink_mioto_genus <- function(genus, offset) {

  genus_name <- genus %>%
    stringr::str_to_lower()

  url <- glue::glue("https://specieslink.net/ws/1.0/search?genus={genus_name}&offset={offset}&limit=5000&apikey=hBVvfER8vHrcD6w9yLsb")

  df <- jsonlite::fromJSON(url)

  df2 <- df$features$properties %>%
    dplyr::tibble()

  return(df2)
}


characidium_all_1 <- speciesLink_mioto_genus("characidium", 1)
characidium_all_2 <- speciesLink_mioto_genus("characidium", 5000)

characidium_all_3 <- bind_rows(characidium_all_1,characidium_all_2)%>%
  select(country, collectioncode, catalognumber, locality, scientificname,
         decimallatitude, decimallongitude, modified) %>%
  distinct()

database_csv <- read_csv("https://raw.githubusercontent.com/brunomioto/sp_alert/master/dados/characidium_database.csv")

new_records <- setdiff(characidium_all_3, database_csv)

for (i in 1:nrow(new_records)) {

  registro_unico <- new_records %>%
    filter(row_number()==i) %>%
    select(country, collectioncode, catalognumber, locality, scientificname,
           decimallatitude, decimallongitude, modified)

  bot$sendMessage(glue::glue("Espécie: {registro_unico$scientificname}\n
                             Localidade: {registro_unico$locality}\n
                             País: {registro_unico$country}\n
                             Voucher: {registro_unico$collectioncode} {registro_unico$catalognumber}\n
                             Coordenadas (lat-lon): {registro_unico$decimallatitude}, {registro_unico$decimallongitude}\n
                             Data: {registro_unico$modified}")
  )
}

# salvar a versao csv
write_csv(characidium_all_3, "dados/characidium_database.csv")

