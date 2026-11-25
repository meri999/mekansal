.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))

# Katalogdaki TÜM tabloları tam veriyle çeker (lastNObservations=1 ölçümü
# güvenilir değildi) ve gerçek kapsamı ölçer.
#   - Ham veri: veri/ham_tum/<ID>.csv.gz (gzip; read.csv doğrudan okur)
#   - Daha önce inen veri/ham/<ID>.csv ve veri/ham_ekonomi/<ID>.csv yeniden
#     indirilmez, yalnız özetlenir
#   - Sonuç: veri/tarama.csv (id, kategori, durum, satır, boyut sayısı,
#     coğrafya sütunları, ülke/ibbs1/ibbs2/il/ilçe sayıları, dönem aralığı,
#     boyutlar)
# Yavaş, tek tek, tekrar denemeli; kaldığı yerden devam eder (tarama.csv'de
# olan id'ler atlanır).
#
# Kullanım:
#   Rscript betikler/02c_tam_tarama.R                   (hepsi)
#   Rscript betikler/02c_tam_tarama.R --id=DF_A,DF_B    (yalnız bunlar,
#                                                        sonucu yeniden yazar)
#   Rscript betikler/02c_tam_tarama.R --kategori="Doğum İstatistikleri"

source(file.path(PORTAL, "betikler", "tuik_api.R"))


# --- girdiler ----------------------------------------------------------

kat <- read.csv(file.path(PORTAL, "ayar/katalog.csv"),
                fileEncoding = "UTF-8", stringsAsFactors = FALSE)

arg <- commandArgs(TRUE)
sec_id <- sub("^--id=", "", grep("^--id=", arg, value = TRUE))
sec_kat <- sub("^--kategori=", "", grep("^--kategori=", arg, value = TRUE))
if (length(sec_id)) kat <- kat[kat$id %in% strsplit(sec_id, ",")[[1]], ]
if (length(sec_kat)) kat <- kat[kat$kategori == sec_kat, ]

# "_O" ekleri önceki betiklerle aynı şekilde dışarıda
kat <- kat[!grepl("_O$", kat$id), ]

cikti <- file.path(PORTAL, "veri/tarama.csv")
dir.create(file.path(PORTAL, "veri/ham_tum"), showWarnings = FALSE)

yapilan <- character()
if (file.exists(cikti) && !length(sec_id)) {
  yapilan <- read.csv(cikti, fileEncoding = "UTF-8")$id
}
kat <- kat[!kat$id %in% yapilan, ]
cat("Taranacak tablo:", nrow(kat), "\n")


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
    ibbs2 = sum(grepl("^TR[0-9A-C][0-9]$", k)),
    il = sum(grepl("^TR[0-9A-C][0-9]{2}$", k)),
    ilce = sum(grepl("^[0-9]{4}$", k)))
}

# Daha önce inmiş ham dosyanın yolunu verir, yoksa NA
mevcut <- function(id) {
  for (d in c("ham", "ham_ekonomi", "ham_tum")) {
    for (e in c(".csv", ".csv.gz")) {
      f <- file.path(PORTAL, "veri", d, paste0(id, e))
      if (file.exists(f)) return(f)
    }
  }
  NA_character_
}

ozet <- function(id, d, kategori, kaynak) {
  meta <- c("STRUCTURE", "STRUCTURE_ID", "ACTION", "TIME_PERIOD",
            "OBS_VALUE", "OBS_STATUS", "CONF_STATUS", "DECIMALS",
            "UNIT_MEASURE", "UNIT_MULT", "TIME_FORMAT", "DATAFLOW")
  idler <- setdiff(names(d), meta)

  geo_re <- "^(TR([0-9A-C]([0-9]{0,2}))?|[0-9]{4})$"
  gs <- idler[vapply(idler, function(g) {
    any(grepl(geo_re, unique(d[[g]])))
  }, TRUE)]
  s <- sinif(unlist(lapply(gs, function(g) d[[g]])))

  data.frame(id = id, kategori = kategori, durum = "ok", kaynak = kaynak,
             satir = nrow(d), boyut_sayisi = length(idler),
             cografya = paste(gs, collapse = ";"),
             ulke = s[["ulke"]], ibbs1 = s[["ibbs1"]],
             ibbs2 = s[["ibbs2"]], il = s[["il"]], ilce = s[["ilce"]],
             ilk_donem = min(d$TIME_PERIOD), son_donem = max(d$TIME_PERIOD),
             boyutlar = paste(idler, collapse = ";"),
             stringsAsFactors = FALSE)
}

# Veri alınamayan tablo için boş özet satırı
bos <- function(id, kategori, durum) {
  data.frame(id = id, kategori = kategori, durum = durum, kaynak = NA,
             satir = 0, boyut_sayisi = NA, cografya = "",
             ulke = 0, ibbs1 = 0, ibbs2 = 0, il = 0, ilce = 0,
             ilk_donem = NA, son_donem = NA, boyutlar = "",
             stringsAsFactors = FALSE)
}


# --- tarama ------------------------------------------------------------

tok <- tok_al()
t0 <- Sys.time()
ardarda <- 0

for (i in seq_len(nrow(kat))) {
  id <- kat$id[i]
  ver <- kat$ver[i]

  if (difftime(Sys.time(), t0, units = "mins") > 2) {
    tok <- tok_al()
    t0 <- Sys.time()
  }

  # Önce diskte var mı diye bak
  f <- mevcut(id)
  s <- NULL
  if (!is.na(f)) {
    d <- tryCatch(
      read.csv(f, colClasses = "character", check.names = FALSE,
               encoding = "UTF-8"),
      error = function(e) NULL)
    if (!is.null(d) && nrow(d) > 0) s <- ozet(id, d, kat$kategori[i], "var")
  }

  # Yoksa indir
  if (is.null(s)) {
    govde <- NULL
    kod <- NA
    for (deneme in 1:3) {
      yol <- sprintf("data/TR,%s,%s?format=SDMX-CSV", id, ver)
      r <- tryCatch(
        tuik_get(yol, tok, accept = "application/vnd.sdmx.data+csv"),
        error = function(e) NULL)
      kod <- if (is.null(r)) NA else resp_status(r)
      b <- if (!is.null(r) && kod == 200) resp_body_string(r) else ""

      if (nzchar(b) && !html(b)) {
        govde <- b
        break
      }
      # İki 404 = TÜİK bu tablo için veri tutmuyor
      if (!is.na(kod) && kod == 404 && deneme >= 2) break

      Sys.sleep(20 * deneme)
      tok <- tok_al()
      t0 <- Sys.time()
    }

    if (is.null(govde)) {
      durum <- if (!is.na(kod) && kod == 404) "yok_404" else "hata"
      s <- bos(id, kat$kategori[i], durum)
      ardarda <- if (identical(s$durum, "hata")) ardarda + 1 else 0
    } else {
      ardarda <- 0
      d <- tryCatch(
        read.csv(text = govde, colClasses = "character",
                 check.names = FALSE, encoding = "UTF-8"),
        error = function(e) NULL)

      if (is.null(d) || nrow(d) == 0) {
        s <- bos(id, kat$kategori[i], "bos")
      } else {
        s <- ozet(id, d, kat$kategori[i], "indirildi")
        g <- gzfile(file.path(PORTAL, "veri/ham_tum",
                              paste0(id, ".csv.gz")), "wb")
        writeLines(govde, g, useBytes = TRUE)
        close(g)
      }
    }
    Sys.sleep(3)
  }

  write.table(s, cikti, sep = ",", row.names = FALSE,
              append = file.exists(cikti), col.names = !file.exists(cikti),
              fileEncoding = "UTF-8", qmethod = "double")
  if (exists("d")) rm(d)
  invisible(gc())

  cat(sprintf("[%d/%d] %-42s %-9s satir=%-8s il=%s ibbs1=%s ulke=%s %s-%s\n",
              i, nrow(kat), id, s$durum, s$satir, s$il, s$ibbs1, s$ulke,
              s$ilk_donem, s$son_donem))
  flush.console()

  if (ardarda >= 4) {
    cat("Ust uste 4 hata: TUIK erisimi kapanmis olabilir, duruyorum.",
        "Tekrar calistirin; kaldigi yerden devam eder.\n")
    quit(status = 2)
  }
}

cat("Bitti.\n")
