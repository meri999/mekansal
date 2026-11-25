.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))

# İl-il göç ağı matrisleri -> docs/veri/goc_ag.js
# (window.GOC_AG; goc-agi.html sayfası yükler)
#   dogum : DF_ADNKS_T05  satır = doğum yeri ili,
#                         sütun = ikamet edilen il   (2014-)
#   kutuk : DF_ADNKS_T09  satır = nüfus kütüğüne kayıtlı il,
#                         sütun = ikamet edilen il   (2014- ile sınırlı)
# Matris düz dizidir: indeks = satir_sirasi * 81 + sutun_sirasi;
# il sırası = kod sırası (iller dizisi). Köşe (kendi ili) dahildir.
# Bunlar STOK ölçüsüdür (yıllık göç akışı değil).

suppressMessages(library(jsonlite))

il_re <- "^TR[0-9A-C][0-9]{2}$"
ilk_yil <- "2014"


# --- il listesi --------------------------------------------------------

yer <- read.csv(file.path(PORTAL, "ayar/yer_adlari.csv"),
                fileEncoding = "UTF-8", stringsAsFactors = FALSE)
yer <- yer[grepl(il_re, yer$kod), ]
yer <- yer[order(yer$kod), ]

n <- nrow(yer)
if (n != 81) {
  stop("81 il bekleniyordu, bulunan: ", n)
}


# --- matris üretimi ----------------------------------------------------

# Her yıl için n x n matrisi satır satır açılmış vektöre çevirir
matris <- function(df, satir_sutun) {
  d <- read.csv(file.path(PORTAL, "veri/ham", paste0(df, ".csv")),
                colClasses = "character", check.names = FALSE,
                encoding = "UTF-8")
  d <- d[grepl(il_re, d[[satir_sutun]]) &
           grepl(il_re, d$IKAMET_YERI) &
           d$TIME_PERIOD >= ilk_yil, ]
  d$v <- suppressWarnings(as.numeric(d$OBS_VALUE))
  d <- d[!is.na(d$v), ]

  if (anyDuplicated(d[, c(satir_sutun, "IKAMET_YERI", "TIME_PERIOD")])) {
    stop(df, ": yinelenen gozlem")
  }

  yillar <- sort(unique(d$TIME_PERIOD))
  v <- lapply(yillar, function(y) {
    x <- d[d$TIME_PERIOD == y, ]
    m <- matrix(0, n, n)
    sira <- cbind(match(x[[satir_sutun]], yer$kod),
                  match(x$IKAMET_YERI, yer$kod))
    m[sira] <- x$v
    as.vector(t(m))   # satır satır
  })
  list(yillar = yillar, v = v)
}

cikti <- list(
  iller = data.frame(k = yer$kod, ad = yer$ad_tr),
  mat = list(dogum = matris("DF_ADNKS_T05", "DOGUM_YERI_IL"),
             kutuk = matris("DF_ADNKS_T09", "NUFUS_KAYIT_IL")),
  uretim = format(Sys.time(), "%Y-%m-%d %H:%M")
)


# --- yaz ---------------------------------------------------------------

dir.create(file.path(PORTAL, "docs/veri"), recursive = TRUE,
           showWarnings = FALSE)
yol <- file.path(PORTAL, "docs/veri/goc_ag.js")
writeLines(paste0("window.GOC_AG=",
                  toJSON(cikti, auto_unbox = TRUE, digits = NA), ";"),
           yol, useBytes = TRUE)

dogum <- cikti$mat$dogum
kutuk <- cikti$mat$kutuk
cat(sprintf("goc_ag.js: %.0f KB | dogum %s-%s, kutuk %s-%s\n",
            file.size(yol) / 1024,
            dogum$yillar[1], tail(dogum$yillar, 1),
            kutuk$yillar[1], tail(kutuk$yillar, 1)))

# Kontrol: son yıl İstanbul sütun toplamı = ilde yaşayanlar (T09 için)
m <- matrix(kutuk$v[[length(kutuk$v)]], n, n, byrow = TRUE)
i <- match("TR100", yer$kod)
mesaj <- paste0("Kontrol kutuk %s: Istanbul'da yasayan (sutun toplami) = %s;",
                " Istanbul kutuklu ve Istanbul'da = %s\n")
cat(sprintf(mesaj,
            tail(kutuk$yillar, 1),
            format(sum(m[, i]), big.mark = "."),
            format(m[i, i], big.mark = ".")))
