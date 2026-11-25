.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))

# Her tablonun son gözlemini çeker; coğrafya sütunlarındaki kodlardan
# gerçek düzeyleri ölçer.
# Düzey kuralı:
#   TR = ülke
#   TR1..TR9, TRA..TRC = IBBS1
#   TR10, TRA1...      = IBBS2
#   TR100, TRA11...    = IBBS3 (il)
#   4 haneli sayı      = ilçe
# Kullanım: Rscript betikler/02_duzey_olc.R <parça> <parça_sayısı> [DF_ID ...]

source(file.path(PORTAL, "betikler", "tuik_api.R"))


# --- girdiler ----------------------------------------------------------

a <- commandArgs(trailingOnly = TRUE)
ids_arg <- grep("^DF_", a, value = TRUE)
nums <- as.integer(setdiff(a, ids_arg))

kat <- read.csv(file.path(PORTAL, "ayar/katalog.csv"),
                fileEncoding = "UTF-8", stringsAsFactors = FALSE)
kes <- read.csv(file.path(PORTAL, "veri/cografya_birlesik.csv"),
                fileEncoding = "UTF-8", stringsAsFactors = FALSE)
kes <- kes[kes$durum == "ok" & nzchar(kes$cografya_boyut) &
             !is.na(kes$cografya_boyut), ]
kes$ver <- kat$ver[match(kes$id, kat$id)]
if (length(ids_arg)) kes <- kes[kes$id %in% ids_arg, ]

cikti <- sprintf(file.path(PORTAL, "veri/duzey_parca%d.csv"), nums[1] + 100)

# Daha önce ölçülmüş tablolar atlanır
parcalar <- list.files(file.path(PORTAL, "veri"), "^duzey_parca.*csv$",
                       full.names = TRUE)
yapilan <- unlist(lapply(parcalar, function(f) {
  x <- read.csv(f, fileEncoding = "UTF-8")
  x$id[x$durum == "ok" & !is.na(x$geo_sutun) & nzchar(x$geo_sutun)]
}))
kes <- kes[!kes$id %in% yapilan, ]

if (length(nums) == 2) {
  kes <- kes[seq_len(nrow(kes)) %% nums[2] == nums[1] - 1, ]
}


# --- yardımcı fonksiyonlar ---------------------------------------------

# Kodları düzeylerine göre sayar
sinif <- function(k) {
  k <- unique(k[nzchar(k)])
  c(ulke = sum(k == "TR"),
    ibbs1 = sum(grepl("^TR[0-9A-C]$", k)),
    ibbs2 = sum(grepl("^TR[0-9A-C][0-9]$", k)),
    il = sum(grepl("^TR[0-9A-C][0-9]{2}$", k)),
    ilce = sum(grepl("^[0-9]{4}$", k)))
}

tok_al <- function() {
  for (k in 1:6) {
    t <- tryCatch(tuik_token(), error = function(e) NULL)
    if (!is.null(t)) return(t)
    Sys.sleep(15 * k)
  }
  stop("token alinamadi")
}


# --- ölçüm -------------------------------------------------------------

tok <- tok_al()
t0 <- Sys.time()

for (i in seq_len(nrow(kes))) {
  if (difftime(Sys.time(), t0, units = "mins") > 2) {
    tok <- tok_al()
    t0 <- Sys.time()
  }

  id <- kes$id[i]
  yol <- sprintf("data/TR,%s,%s?format=SDMX-CSV&lastNObservations=1",
                 id, kes$ver[i])
  r <- tryCatch(
    tuik_get(yol, tok, accept = "application/vnd.sdmx.data+csv"),
    error = function(e) NULL)

  s <- data.frame(id = id, durum = "hata", ulke = NA, ibbs1 = NA,
                  ibbs2 = NA, il = NA, ilce = NA, son_donem = NA,
                  satir = NA, geo_sutun = NA)

  if (!is.null(r) && resp_status(r) == 200 &&
      !grepl("^<!DOCTYPE|^<html", resp_body_string(r))) {
    d <- read.csv(text = resp_body_string(r), check.names = FALSE,
                  colClasses = "character", encoding = "UTF-8")
    gs <- strsplit(kes$cografya_boyut[i], ";")[[1]]
    gs <- gs[gs %in% names(d)]
    kodlar <- unique(unlist(lapply(gs, function(g) d[[g]])))

    s[1, c("ulke", "ibbs1", "ibbs2", "il", "ilce")] <- as.list(sinif(kodlar))
    s$durum <- "ok"
    s$son_donem <- max(d$TIME_PERIOD)
    s$satir <- nrow(d)
    s$geo_sutun <- paste(gs, collapse = ";")
  }

  write.table(s, cikti, sep = ",", row.names = FALSE,
              append = file.exists(cikti), col.names = !file.exists(cikti),
              fileEncoding = "UTF-8")

  gecen <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
  cat(sprintf("[%d/%d] %s %s il=%s ibbs1=%s ilce=%s %ss\n",
              i, nrow(kes), id, s$durum, s$il, s$ibbs1, s$ilce, gecen))
  flush.console()
}

cat("Bitti.\n")
