library(telegram)
library(dplyr)
library(readr)
library(stringr)
library(glue)
library(purrr)
library(httr2)

#connect spalert_bot
bot <- TGBot$new(token = bot_token('SPALERT_BOT'))

bot$set_default_chat_id(302461245)

speciesLink_mioto_genus <- function(genus, offset = 0) {

  genus_name <- genus %>%
    stringr::str_to_lower()

  url <- "https://specieslink.net/ws/1.0/search"

  req <- httr2::request(url)

  resp <- req %>%
    httr2::req_headers("Accept" = "application/json") %>%
    httr2::req_url_query(genus = genus_name,
                         offset = offset,
                         limit = 5000,
                         apikey = "hBVvfER8vHrcD6w9yLsb") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()

  resp_properties <- resp$features %>%
    purrr::map(purrr::pluck, "properties")

  resp_df <- dplyr::bind_rows(resp_properties)

  return(resp_df)

}


characidium_all_1 <- speciesLink_mioto_genus("characidium", 0)
characidium_all_2 <- speciesLink_mioto_genus("characidium", 5000)

characidium_all_3 <- bind_rows(characidium_all_1,characidium_all_2)%>%
  select(country,stateprovince, county, collectioncode, catalognumber, locality, scientificname,
         decimallatitude, decimallongitude, daycollected, monthcollected, yearcollected) %>%
  distinct() %>%
  mutate(decimallatitude = as.numeric(decimallatitude),
         decimallongitude = as.numeric(decimallongitude),
         yearcollected = as.numeric(yearcollected))

database_csv <- read_csv("https://raw.githubusercontent.com/brunomioto/sp_alert/master/dados/characidium_database.csv")

new_records <- setdiff(characidium_all_3, database_csv)

bot$sendMessage("Registros de _Characidium_ no *SpeciesLink* no último dia:",
                parse_mode = "Markdown")

# if(nrow(new_records)>0){
#   for (i in 1:nrow(new_records)) {
#
#     registro_unico <- new_records %>%
#       filter(row_number()==i) %>%
#       select(country, stateprovince, county, collectioncode, catalognumber, locality, scientificname,
#              decimallatitude, decimallongitude, daycollected, monthcollected, yearcollected)
#
#     bot$sendMessage(glue::glue("
#                              *Espécie:* {registro_unico$scientificname}\n
#                              *País:* {registro_unico$country}\n
#                              *Estado:* {registro_unico$stateprovince}\n
#                              *Cidade:* {registro_unico$county}\n
#                              *Localidade:* {registro_unico$locality}\n
#                              *Voucher:* {registro_unico$collectioncode} {registro_unico$catalognumber}\n
#                              *Coordenadas (lat-lon):* {registro_unico$decimallatitude}, {registro_unico$decimallongitude}\n
#                              *Coleta:* {registro_unico$daycollected}/{registro_unico$monthcollected}/{registro_unico$yearcollected}"),
#                     parse_mode = "Markdown"
#     )
#   }
# }else{
#   bot$sendMessage("Nenhum registro novo")
# }

# salvar a versao csv
write_csv(characidium_all_3, "dados/characidium_database.csv")

