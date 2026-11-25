.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))

# Ekonomi konulu TÜM tabloları (Türkiye düzeyli dahil) en güncel haliyle
# indirir -> veri/ham_ekonomi/<ID>.csv
# Sırayla, yavaş ve tekrar denemeli (TÜİK yükte HTML 404 döndürür).
# Var olan dosyayı atlar; --yenile hepsini yeniler.

source(file.path(PORTAL, "betikler", "tuik_api.R"))


# --- indirilecek tablolar ----------------------------------------------

kat <- read.csv(file.path(PORTAL, "ayar/katalog.csv"),
                fileEncoding = "UTF-8", stringsAsFactors = FALSE)

kon <- c(
  "Konut ve İş Yeri Satış İstatistikleri",
  "Mali Aracı Kuruluş İstatistikleri",
  "Yıllık Sanayi ve Hizmet İstatistikleri",
  "Bölgesel Hesaplar",
  "Bölgesel SGP",
  "SGP",
  "Girişimcilik ve İş Demografisi",
  "İş Kayıtları",
  "Kazanç Yapısı İstatistikleri",
  "Ücretli Çalışan İstatistikleri",
  "Dönemsel İşgücü İstatistikleri",
  "Aylık İşgücü İstatistikleri",
  "Dış Ticaret Endeksleri",
  "Sanayi Sektörü Nihai Enerji Tüketim İstatistikleri",
  "Hayvansal Üretim İstatistikleri",
  "Tarımsal Alet ve Makine İstatistikleri",
  "Tarımsal İşletme İşgücü Ücret Yapısı",
  "Motorlu Kara Taşıtları",
  "Tüketici Fiyat Endeksi",
  "Üretim Endeksleri",
  "Uluslararası Hizmet Ticareti İstatistikleri",
  "Güven Endeksleri",
  "Sektör Bilançoları",
  "Devlet Hesapları",
  "İşgücü Girdi Endeksleri",
  "Yurt İçi Üretici Fiyat Endeksi",
  "Tarım Ürünleri Üretici Fiyat Endeksi",
  "İnşaat Maliyet Endeksi",
  "Ticaret Satış Hacim Endeksi",
  "Tarımsal Girdi Fiyat Endeksi",
  "Sanayi Üretici Fiyat Endeksleri",
  "Hizmet Üretici Fiyat Endeksi",
  "Yurt Dışı Üretici Fiyat Endeksi",
  "Ciro Endeksleri (Sanayi, İnşaat, Ticaret, Hizmet)",
  "Finansal Yatırım Araçlarının Getiri Oranları",
  "Kurumsal Sektör Hesapları",
  "Çevresel Ekonomik Hesaplar"
)

l <- kat[kat$kategori %in% kon & !grepl("_O$", kat$id), ]

# Kategorisi dışında kalan ama gereken tablo
ek <- c("DF_SATIS_SEKLI_DURUMU_ILILCE_V3")
l <- rbind(l, kat[kat$id %in% ek & !kat$id %in% l$id, ])

# İki sayı verilirse (parça, parça sayısı) listeyi böler
a <- suppressWarnings(as.integer(commandArgs(TRUE)))
a <- a[!is.na(a)]
if (length(a) == 2) l <- l[seq_len(nrow(l)) %% a[2] == a[1] - 1, ]

dir.create(file.path(PORTAL, "veri/ham_ekonomi"), showWarnings = FALSE)
yenile <- "--yenile" %in% commandArgs(TRUE)
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
cat("Tablo:", nrow(l), "\n")

for (i in seq_len(nrow(l))) {
  id <- l$id[i]
  f <- file.path(PORTAL, "veri/ham_ekonomi", paste0(id, ".csv"))
  if (!yenile && file.exists(f)) next

  if (difftime(Sys.time(), t0, units = "mins") > 2) {
    tok <- tok_al()
    t0 <- Sys.time()
  }

  g <- NULL
  for (d in 1:3) {
    yol <- sprintf("data/TR,%s,%s?format=SDMX-CSV", id, l$ver[i])
    r <- tryCatch(
      tuik_get(yol, tok, accept = "application/vnd.sdmx.data+csv"),
      error = function(e) NULL)
    b <- if (!is.null(r) && resp_status(r) == 200) resp_body_string(r) else ""
    if (nzchar(b) && !html(b)) {
      g <- b
      break
    }
    Sys.sleep(20 * d)
    tok <- tok_al()
    t0 <- Sys.time()
  }

  if (is.null(g)) {
    basarisiz <- c(basarisiz, id)
    cat(sprintf("[%d/%d] %s YANIT_YOK\n", i, nrow(l), id))
  } else {
    # Yarım dosya kalmasın diye önce geçici dosyaya yazılır
    writeLines(g, paste0(f, ".tmp"), useBytes = TRUE)
    file.rename(paste0(f, ".tmp"), f)

    x <- tryCatch(
      read.csv(text = g, colClasses = "character", check.names = FALSE,
               encoding = "UTF-8"),
      error = function(e) NULL)
    satir <- if (is.null(x)) NA else nrow(x)
    son <- if (is.null(x)) NA else max(x$TIME_PERIOD)

    cat(sprintf("[%d/%d] %s ok %.2f MB satir=%d son=%s | %s\n",
                i, nrow(l), id, nchar(g, "bytes") / 1e6, satir, son,
                substr(l$ad_tr[i], 1, 55)))
  }
  flush.console()
  Sys.sleep(4)
}

cat("INDIRILEMEYEN:", length(basarisiz), paste(basarisiz, collapse = " "),
    "\nBitti.\n")
