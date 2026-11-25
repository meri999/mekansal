# Ortak yardımcı: token al, SDMX isteği yap
# Önce yollar.R yüklenmiş olmalı (PORTAL değişkeni).
library(httr2)

# API anahtarı PORTAL/.Renviron içinde durur; TUIK_API_KEY zaten
# tanımlıysa dokunulmaz.
renviron <- file.path(PORTAL, ".Renviron")
if (!nzchar(Sys.getenv("TUIK_API_KEY")) && file.exists(renviron)) {
  readRenviron(renviron)
}
if (!nzchar(Sys.getenv("TUIK_API_KEY"))) {
  stop("TUIK_API_KEY bulunamadi: ", renviron,
       " icine TUIK_API_KEY=... yazin")
}

tuik_token <- function() {
  adres <- "https://giris.tuik.gov.tr/realms/web/protocol/openid-connect/token"
  r <- request(adres) |>
    req_body_form(grant_type = "password",
                  client_id = "nsi-ws-consumer",
                  api_key = Sys.getenv("TUIK_API_KEY")) |>
    req_timeout(60) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  if (resp_status(r) != 200) {
    stop("Token alinamadi: ", resp_status(r))
  }
  resp_body_json(r)$access_token
}

tuik_get <- function(yol, token = tuik_token(), accept = NULL) {
  rq <- request(paste0("https://nsiws.tuik.gov.tr/rest/", yol)) |>
    req_headers(Authorization = paste("Bearer", token)) |>
    req_timeout(240) |>
    req_error(is_error = \(r) FALSE)
  if (!is.null(accept)) {
    rq <- req_headers(rq, Accept = accept)
  }
  rq |> req_perform()
}
