.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))

# Seçilen konulardaki il düzeyli tabloları TEK TEK (paralel değil) indirir,
# ham SDMX-CSV olarak saklar.
# Kullanım:
#   Rscript betikler/03_indir_secili.R           (var olan dosyaları atlar;
#                                                 yarım kalırsa kaldığı
#                                                 yerden devam eder)
#   Rscript betikler/03_indir_secili.R --yenile  (hepsini yeniden indirir;
#                                                 hata olursa eski dosya
#                                                 korunur, çıkış kodu 3)

source(file.path(PORTAL, "betikler", "tuik_api.R"))


# --- indirilecek tablolar ----------------------------------------------

kat <- read.csv(file.path(PORTAL, "ayar/katalog.csv"),
                fileEncoding = "UTF-8", stringsAsFactors = FALSE)
o <- read.csv(file.path(PORTAL, "veri/duzey_sonuc.csv"),
              fileEncoding = "UTF-8", stringsAsFactors = FALSE)

konular <- c("Adrese Dayalı Nüfus Kayıt Sistemi", "Boşanma İstatistikleri",
             "Doğum İstatistikleri", "Evlenme İstatistikleri",
             "Bölgesel Hesaplar", "Girişimcilik ve İş Demografisi",
             "İş Kayıtları", "Beşeri Kalkınma Endeksi",
             "Muhtemel Eğitim Süresi", "Ulusal Eğitim İstatistikleri")

# --id=DF_A,DF_B : konu listesi dışındaki tabloyu da indir
ekid <- sub("^--id=", "", grep("^--id=", commandArgs(TRUE), value = TRUE))
ekid <- if (length(ekid)) strsplit(ekid, ",")[[1]] else character()

liste <- o[o$durum == "ok" & (o$il > 0 | o$ibbs1 > 0) &
             (o$kategori %in% konular | o$id %in% ekid), ]

# Göstergeler için gereken ama düzey ölçümünde "yanit_yok" çıkmış
# tablolar: her koşulda indirilir
zorunlu <- c("DF_SATIS_SEKLI_DURUMU_ILILCE_V3", "DF_UH_BH_KISI_BASI")

# Göç göstergelerinin tabloları (DF_UGOC_* TÜİK API'sinde veri döndürmüyor:
# 404, bu yüzden listede yok)
zorunlu <- c(zorunlu, "DF_ADNKS_T01", "DF_ADNKS_T05", "DF_ADNKS_T09",
             "DF_ADNKS_T12", "DF_ADNKS_T13", "DF_ICGOC_T01",
             "DF_GOC_ULKE_DAGILIM", "DF_YO_MEZUN_BEYIN_GOCU_ORAN")

# Türkiye düzeyli göstergelerin tabloları (duzey_sonuc.csv dışında olabilir)
gostergeler_tr <- read.csv(file.path(PORTAL, "ayar/gostergeler_tr.csv"),
                           fileEncoding = "UTF-8", stringsAsFactors = FALSE)
zorunlu <- unique(c(zorunlu, gostergeler_tr$df_id))

ek <- setdiff(zorunlu, liste$id)
liste <- data.frame(id = c(liste$id, ek))
liste$ver <- kat$ver[match(liste$id, kat$id)]

dir.create(file.path(PORTAL, "veri/ham"), showWarnings = FALSE)
yenile <- "--yenile" %in% commandArgs(trailingOnly = TRUE)
basarisiz <- character()


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


# --- indirme -----------------------------------------------------------

tok <- tok_al()
t0 <- Sys.time()
ardarda_hata <- 0

for (i in seq_len(nrow(liste))) {
  id <- liste$id[i]
  dosya <- sprintf(file.path(PORTAL, "veri/ham/%s.csv"), id)

  if (!yenile && file.exists(dosya)) {
    cat(sprintf("[%d/%d] %s zaten var\n", i, nrow(liste), id))
    next
  }
  if (difftime(Sys.time(), t0, units = "mins") > 2) {
    tok <- tok_al()
    t0 <- Sys.time()
  }

  govde <- NULL
  for (deneme in 1:3) {
    yol <- sprintf("data/TR,%s,%s?format=SDMX-CSV", id, liste$ver[i])
    r <- tryCatch(
      tuik_get(yol, tok, accept = "application/vnd.sdmx.data+csv"),
      error = function(e) NULL)
    b <- if (!is.null(r) && resp_status(r) == 200) resp_body_string(r) else ""
    if (nzchar(b) && !html(b)) {
      govde <- b
      break
    }
    Sys.sleep(30 * deneme)
    tok <- tok_al()
    t0 <- Sys.time()
  }

  if (is.null(govde)) {
    ardarda_hata <- ardarda_hata + 1
    basarisiz <- c(basarisiz, id)
    cat(sprintf("[%d/%d] %s HATA\n", i, nrow(liste), id))
  } else {
    # Yarım dosya kalmasın diye önce geçici dosyaya yazılır
    ardarda_hata <- 0
    gecici <- paste0(dosya, ".tmp")
    writeLines(govde, gecici, useBytes = TRUE)
    file.rename(gecici, dosya)
    cat(sprintf("[%d/%d] %s ok %.1f MB\n", i, nrow(liste), id,
                nchar(govde, "bytes") / 1e6))
  }
  flush.console()

  if (ardarda_hata >= 3) {
    cat("Ust uste 3 hata: TUIK erisimi kapanmis olabilir, duruyorum.\n")
    quit(status = 2)
  }
  Sys.sleep(3)
}

if (length(basarisiz)) {
  cat("INDIRILEMEYEN:", paste(basarisiz, collapse = ", "), "\n")
  quit(status = 3)
}
cat("Bitti.\n")
