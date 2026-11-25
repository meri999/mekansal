.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))

# Göç tablolarını lastNObservations olmadan TAM çekmeyi dener; veri varsa
# veri/ham/ altına kaydeder ve özetini veri/goc_yoklama.csv dosyasına yazar
# (id, durum, satır, coğrafya sütunları, düzey sayıları, dönem aralığı).
# Kullanım: Rscript betikler/02b_goc_yokla.R [--yenile]

source(file.path(PORTAL, "betikler", "tuik_api.R"))


# --- girdiler ----------------------------------------------------------

kat <- read.csv(file.path(PORTAL, "ayar/katalog.csv"),
                fileEncoding = "UTF-8", stringsAsFactors = FALSE)

ids <- c("DF_UGOC_T1", "DF_UGOC_T2", "DF_UGOC_T3", "DF_UGOC_T4",
         "DF_UGOC_T5", "DF_UGOC_T6", "DF_UGOC_T8",
         "DF_ADNKS_T01", "DF_ADNKS_T02", "DF_ADNKS_T03", "DF_ADNKS_T05",
         "DF_ADNKS_T09", "DF_ADNKS_T12", "DF_ADNKS_T13",
         "DF_ICGOC_T01", "DF_GOC_ULKE_DAGILIM",
         "DF_TAMAMLANAN_BOLUM_BGO", "DF_YO_MEZUN_BEYIN_GOCU_ORAN",
         "DF_EGITIM_OGRETIM_ALAN", "DF_UNIVERSITE_TUR_BURS_DURUM",
         "DF_YO_OGRENIM_DIL")

yenile <- "--yenile" %in% commandArgs(trailingOnly = TRUE)
cikti <- file.path(PORTAL, "veri/goc_yoklama.csv")
dir.create(file.path(PORTAL, "veri/ham"), showWarnings = FALSE)


# --- yardımcı fonksiyonlar ---------------------------------------------

html <- function(b) grepl("^<!DOCTYPE|^<html", b)

tok_al <- function() {
  for (k in 1:6) {
    t <- tryCatch(tuik_token(), error = function(e) NULL)
    if (!is.null(t)) return(t)
    Sys.sleep(15 * k)
  }
  stop("token alinamadi")
}

# Kodları düzeylerine göre sayar
sinif <- function(k) {
  k <- unique(k[nzchar(k)])
  c(ulke = sum(k == "TR"),
    ibbs1 = sum(grepl("^TR[0-9A-C]$", k)),
    il = sum(grepl("^TR[0-9A-C][0-9]{2}$", k)),
    ilce = sum(grepl("^[0-9]{4}$", k)))
}


# --- yoklama -----------------------------------------------------------

tok <- tok_al()
t0 <- Sys.time()
sonuc <- list()

for (id in ids) {
  if (difftime(Sys.time(), t0, units = "mins") > 2) {
    tok <- tok_al()
    t0 <- Sys.time()
  }

  dosya <- file.path(PORTAL, "veri/ham", paste0(id, ".csv"))
  ver <- kat$ver[match(id, kat$id)]
  s <- data.frame(id = id, durum = "bos", satir = 0, geo_sutun = "",
                  ulke = 0, ibbs1 = 0, il = 0, ilce = 0,
                  ilk_donem = "", son_donem = "", boyutlar = "")

  # Dosya varsa onu oku, yoksa en çok 3 kez indirmeyi dene
  b <- ""
  if (!yenile && file.exists(dosya)) {
    b <- paste(readLines(dosya, encoding = "UTF-8", warn = FALSE),
               collapse = "\n")
  } else {
    for (deneme in 1:3) {
      yol <- sprintf("data/TR,%s,%s?format=SDMX-CSV", id, ver)
      r <- tryCatch(
        tuik_get(yol, tok, accept = "application/vnd.sdmx.data+csv"),
        error = function(e) NULL)
      durum <- if (is.null(r)) NA else resp_status(r)
      b <- if (!is.null(r) && durum == 200) resp_body_string(r) else ""
      if (nzchar(b) && !html(b)) break

      cat(sprintf("%s deneme %d: HTTP %s\n", id, deneme, durum))
      b <- ""
      Sys.sleep(20 * deneme)
      tok <- tok_al()
      t0 <- Sys.time()
    }
  }

  if (nzchar(b)) {
    d <- tryCatch(
      read.csv(text = b, check.names = FALSE, colClasses = "character",
               encoding = "UTF-8"),
      error = function(e) NULL)

    if (!is.null(d) && nrow(d) > 0) {
      geo_re <- "^TR([0-9A-C]([0-9]{0,2}))?$"
      gs <- names(d)[vapply(d, function(x) any(grepl(geo_re, x)), TRUE)]
      kod <- unique(unlist(lapply(gs, function(g) d[[g]])))
      meta <- c("STRUCTURE", "STRUCTURE_ID", "ACTION", "TIME_PERIOD",
                "OBS_VALUE")

      s[1, c("ulke", "ibbs1", "il", "ilce")] <- as.list(sinif(kod))
      s$durum <- "ok"
      s$satir <- nrow(d)
      s$geo_sutun <- paste(gs, collapse = ";")
      s$ilk_donem <- min(d$TIME_PERIOD)
      s$son_donem <- max(d$TIME_PERIOD)
      s$boyutlar <- paste(setdiff(names(d), meta), collapse = ";")

      # Yarım dosya kalmasın diye önce geçici dosyaya yazılır
      if (!file.exists(dosya)) {
        gecici <- paste0(dosya, ".tmp")
        writeLines(b, gecici, useBytes = TRUE)
        file.rename(gecici, dosya)
      }
    }
  }

  sonuc[[id]] <- s
  cat(sprintf("%-32s %-4s satir=%-7s ulke=%s ibbs1=%s il=%s %s-%s geo=%s\n",
              id, s$durum, s$satir, s$ulke, s$ibbs1, s$il,
              s$ilk_donem, s$son_donem, s$geo_sutun))
  flush.console()

  write.csv(do.call(rbind, sonuc), cikti, row.names = FALSE,
            fileEncoding = "UTF-8")
  Sys.sleep(3)
}

cat("Bitti.\n")
