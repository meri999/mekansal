.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))

# TÜİK bülten ek tabloları (xls)  ->  veri/ham/BULTEN_ISGUCU_IL.csv
#
# API'de olmayan il düzeyi işgücü tabloları (Yıllık ve Bölgesel İşgücü
# İstatistikleri bülteni). Dosyaları tarayıcıdan indirip veri/manuel_indirilenler/
# içine koyun (adlar: isgucu_il_<issizlik|istihdam|katilma>_<yil>.xls).
# Her yeni bülten (Mart) için yeni xls eklenir; aynı yıl ve gösterge için
# en yeni dosya geçerlidir.
#
# Kullanım: Rscript betikler/05a_bulten_oku.R

suppressMessages(library(readxl))

GOSTERGE <- c(issizlik = "ISSIZLIK", istihdam = "ISTIHDAM", katilma = "KATILMA")
YIL_RE <- "^(19|20)[0-9]{2}$"


# --- tek dosyayı oku ---------------------------------------------------

oku <- function(f, g) {
  d <- as.data.frame(
    read_excel(f, sheet = 1, col_names = FALSE, .name_repair = "minimal"),
    stringsAsFactors = FALSE)
  d[] <- lapply(d, as.character)

  # yılların yazıldığı başlık satırı
  baslik <- which(apply(d, 1, function(r) {
    sum(grepl(YIL_RE, trimws(r)), na.rm = TRUE) >= 1
  }))[1]
  if (is.na(baslik)) stop("yil basligi bulunamadi: ", basename(f))
  yilc <- which(grepl(YIL_RE, trimws(unlist(d[baslik, ]))))

  kodlar <- trimws(d[[1]])
  satir <- which(grepl("^TR[0-9A-C]?[0-9]{0,2}$", kodlar))
  if (!length(satir)) stop("TR kodlu satir yok: ", basename(f))

  # güven aralığı (alt sınır) sütunları
  ust <- as.matrix(d[baslik + 1:2, ])
  alt_c <- sort(unique(col(ust)[grepl("Alt s", ust, fixed = TRUE)]))

  out <- list()
  for (j in yilc) {
    ac <- alt_c[alt_c > j][1]
    v <- suppressWarnings(as.numeric(d[satir, j]))
    alt <- NA
    ust_sinir <- NA
    if (!is.na(ac)) {
      alt <- suppressWarnings(as.numeric(d[satir, ac]))
      ust_sinir <- suppressWarnings(as.numeric(d[satir, ac + 1]))
    }
    out[[length(out) + 1]] <- data.frame(
      REF_AREA = kodlar[satir],
      TIME_PERIOD = trimws(d[baslik, j]),
      GOSTERGE = g,
      OBS_VALUE = v,
      ALT = alt,
      UST = ust_sinir,
      DIPNOT = ifelse(grepl("^\\(1\\)$", trimws(d[satir, j + 1])),
                      "ihtiyatli", ""),
      KAYNAK_DOSYA = basename(f),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}


# --- tüm dosyaları birleştir -------------------------------------------

dosyalar <- list.files(file.path(PORTAL, "veri/manuel_indirilenler"),
                       "^isgucu_il_.*\\.xls$", full.names = TRUE)
if (!length(dosyalar)) stop("veri/manuel_indirilenler/ icinde isgucu_il_*.xls yok")

# eski -> yeni sırayla okunur; yeni dosya üstüne yazar
tum <- list()
for (f in dosyalar[order(file.mtime(dosyalar))]) {
  g <- GOSTERGE[sub("^isgucu_il_([a-z]+)_.*$", "\\1", basename(f))]
  if (is.na(g)) {
    cat("atlandi (gosterge adi taninmadi):", basename(f), "\n")
    next
  }
  x <- oku(f, g)
  cat(sprintf("%-34s %-9s %d satir, yillar %s\n", basename(f), g, nrow(x),
              paste(unique(x$TIME_PERIOD), collapse = ",")))
  tum[[length(tum) + 1]] <- x
}
tum <- do.call(rbind, tum)

anahtar <- c("REF_AREA", "TIME_PERIOD", "GOSTERGE")
tum <- tum[!duplicated(tum[, anahtar], fromLast = TRUE), ]
tum <- tum[order(tum$GOSTERGE, tum$TIME_PERIOD, tum$REF_AREA), ]

dir.create(file.path(PORTAL, "veri/ham"), showWarnings = FALSE)
write.csv(tum, file.path(PORTAL, "veri/ham/BULTEN_ISGUCU_IL.csv"),
          row.names = FALSE, fileEncoding = "UTF-8", na = "")
cat(sprintf("Yazildi: veri/ham/BULTEN_ISGUCU_IL.csv (%d satir; %d yer)\n",
            nrow(tum), length(unique(tum$REF_AREA))))
