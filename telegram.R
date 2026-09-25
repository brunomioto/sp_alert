library(telegram)
library(dplyr)
library(readr)
library(stringr)
library(glue)
library(purrr)
library(httr2)
library(piggyback)

#connect spalert_bot
bot <- TGBot$new(token = bot_token('SPALERT_BOT'))

bot$set_default_chat_id(302461245)

#get splink token
apikey <- Sys.getenv("SPECIESLINK_API_KEY")

speciesLink_mioto_genus <- function(genus, offset = 0) {
  genus_name <- genus %>%
    stringr::str_to_lower()

  url <- "https://specieslink.net/ws/1.0/search"

  req <- httr2::request(url)

  resp <- req %>%
    httr2::req_headers("Accept" = "application/json") %>%
    httr2::req_url_query(
      genus = genus_name,
      offset = offset,
      limit = 5000,
      apikey = apikey
    ) %>%
    httr2::req_timeout(120) %>%
    httr2::req_retry(max_tries = 3) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()

  resp_properties <- resp$features %>%
    purrr::map(purrr::pluck, "properties") %>%
    purrr::map(function(props) {
      purrr::map(props, function(v) {
        if (is.null(v) || length(v) == 0) {
          NA_character_
        } else {
          paste(as.character(unlist(v)), collapse = " | ")
        }
      })
    })

  if (length(resp_properties) == 0) return(tibble::tibble())

  dplyr::bind_rows(resp_properties) %>%
    dplyr::mutate(dplyr::across(
      dplyr::any_of(c(
        "decimallatitude", "decimallongitude",
        "year", "month", "day",
        "coordinateprecision", "elevation", "maximumelevation", "minimumelevation"
      )),
      function(x) suppressWarnings(as.numeric(x))
    ))
}


# busca todas as paginas ate vir uma pagina incompleta
PAGE_SIZE <- 5000
pages <- list()
offset <- 0
repeat {
  page <- tryCatch(
    speciesLink_mioto_genus("characidium", offset),
    error = function(e) {
      bot$sendMessage(glue::glue("Erro ao consultar speciesLink: {conditionMessage(e)}"))
      quit(status = 1)
    }
  )
  pages <- c(pages, list(page))
  if (nrow(page) < PAGE_SIZE) break
  offset <- offset + PAGE_SIZE
}

characidium_api <- dplyr::bind_rows(pages)

if (nrow(characidium_api) == 0) {
  bot$sendMessage("speciesLink retornou 0 registros, base não foi atualizada")
  quit(status = 1)
}

# tudo como texto para comparar e salvar com tipos estaveis
characidium_all_3 <- characidium_api |>
  dplyr::select(
    country,
    stateprovince,
    county,
    collectioncode,
    catalognumber,
    locality,
    scientificname,
    decimallatitude,
    decimallongitude,
    daycollected,
    monthcollected,
    yearcollected
  ) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
  dplyr::distinct()

# base acumulativa: guarda tudo que ja foi visto, nunca remove.
# assim registros que somem da API por um dia (colecao fora do ar)
# nao voltam como "novos" no dia seguinte
database_path <- "dados/characidium_database.csv"

database_csv <- readr::read_csv(
  database_path,
  col_types = readr::cols(.default = readr::col_character())
)

new_records <- characidium_all_3 |>
  dplyr::anti_join(
    database_csv,
    by = c("collectioncode", "catalognumber", "scientificname")
  )


bot$sendMessage(
  "Registros de _Characidium_ no *SpeciesLink* no último dia:",
  parse_mode = "Markdown"
)

if (nrow(new_records) > 10) {
  spp_counts <- new_records |>
    dplyr::count(scientificname, name = "n") |>
    dplyr::arrange(dplyr::desc(n))

  coll_counts <- new_records |>
    dplyr::count(collectioncode, name = "n") |>
    dplyr::arrange(dplyr::desc(n))

  MAX_SPP <- 10
  MAX_COLL <- 10

  spp_lines <- paste0("- ", spp_counts$scientificname, " (", spp_counts$n, ")")
  coll_lines <- paste0(
    "- ",
    coll_counts$collectioncode,
    " (",
    coll_counts$n,
    ")"
  )

  spp_text <- paste0(head(spp_lines, MAX_SPP), collapse = "\n")
  coll_text <- paste0(head(coll_lines, MAX_COLL), collapse = "\n")

  if (nrow(spp_counts) > MAX_SPP) {
    spp_text <- paste0(spp_text, "\n... e mais")
  }
  if (nrow(coll_counts) > MAX_COLL) {
    coll_text <- paste0(coll_text, "\n... e mais")
  }

  msg1 <- glue::glue(
    "*Existem {nrow(new_records)} novos registros de Characidium*"
  )
  msg2 <- glue::glue("*Espécies (n = {nrow(spp_counts)}):*\n{spp_text}")
  msg3 <- glue::glue("*Coleções (n = {nrow(coll_counts)}):*\n{coll_text}")

  bot$sendMessage(msg1, parse_mode = "Markdown")
  bot$sendMessage(msg2, parse_mode = "Markdown")
  bot$sendMessage(msg3, parse_mode = "Markdown")
}

if (nrow(new_records) > 0 & nrow(new_records) <= 10) {
  msg1 <- glue::glue(
    "
*Existem {nrow(new_records)} novos registros de Characidium*
"
  )

  bot$sendMessage(msg1, parse_mode = "Markdown")

  for (i in 1:nrow(new_records)) {
    registro_unico <- new_records %>%
      dplyr::filter(dplyr::row_number() == i) %>%
      dplyr::select(
        country,
        stateprovince,
        county,
        collectioncode,
        catalognumber,
        locality,
        scientificname,
        decimallatitude,
        decimallongitude,
        daycollected,
        monthcollected,
        yearcollected
      )

    bot$sendMessage(
      glue::glue(
        "
                             *Espécie:* {registro_unico$scientificname}\n
                             *País:* {registro_unico$country}\n
                             *Estado:* {registro_unico$stateprovince}\n
                             *Cidade:* {registro_unico$county}\n
                             *Localidade:* {registro_unico$locality}\n
                             *Voucher:* {registro_unico$collectioncode} {registro_unico$catalognumber}\n
                             *Coordenadas (lat-lon):* {registro_unico$decimallatitude}, {registro_unico$decimallongitude}\n
                             *Coleta:* {registro_unico$daycollected}/{registro_unico$monthcollected}/{registro_unico$yearcollected}"
      ),
      parse_mode = "Markdown"
    )
  }
}

if (nrow(new_records) == 0) {
  bot$sendMessage("Nenhum registro novo")
}

if (nrow(new_records) > 0) {
  today_str <- format(Sys.Date(), "%Y_%m_%d")
  csv_name <- paste0("new_records_", today_str, ".csv")
  tmp_path <- file.path(tempdir(), csv_name)
  readr::write_csv(new_records, tmp_path)
  piggyback::pb_upload(tmp_path, repo = "brunomioto/sp_alert", tag = "latest")
}

# salvar a versao csv (base antiga + novos registros com data da primeira aparicao)
database_updated <- dplyr::bind_rows(
  database_csv,
  new_records |> dplyr::mutate(first_seen = as.character(Sys.Date()))
)
readr::write_csv(database_updated, database_path)
