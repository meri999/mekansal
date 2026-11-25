.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))

# ayar/kirilim_tablolar.csv şemasını ve ham dosyaların varlığını denetler.
konular <- c("Nüfus ve demografi", "Ekonomi", "Eğitim", "Kalkınma",
             "Yaşam koşulları", "Göç")
sutunlar <- c("kod", "konu", "baslik", "df_id", "filtre", "cografya",
              "kirilimlar", "turkiye", "olcu", "birim", "son_yil_sayisi",
              "aciklama", "toplanamaz", "baglanti_df")

t <- read.csv(file.path(PORTAL, "ayar/kirilim_tablolar.csv"),
              fileEncoding = "UTF-8", stringsAsFactors = FALSE,
              na.strings = character())
stopifnot("sütunlar şemaya uymuyor" = identical(names(t), sutunlar))
stopifnot("kod benzersiz değil" = !anyDuplicated(t$kod))
hata <- character()
for (i in seq_len(nrow(t))) {
  x <- t[i, ]
  if (!x$konu %in% konular) hata <- c(hata, paste(x$kod, ": konu geçersiz"))
  if (!x$turkiye %in% c("var", "topla")) hata <- c(hata, paste(x$kod, ": turkiye geçersiz"))
  if (!x$olcu %in% c("sayi", "oran")) hata <- c(hata, paste(x$kod, ": olcu geçersiz"))
  yol <- NULL
  for (k in c("ham", "ham_ekonomi", "ham_tum"))
    for (u in c(".csv", ".csv.gz")) {
      f <- file.path(PORTAL, "veri", k, paste0(x$df_id, u))
      if (is.null(yol) && file.exists(f)) yol <- f
    }
  if (is.null(yol)) { hata <- c(hata, paste(x$kod, ": ham dosya yok")); next }
  baslik <- names(read.csv(if (grepl("gz$", yol)) gzfile(yol) else yol,
                           nrows = 1, check.names = FALSE))
  kir <- strsplit(x$kirilimlar, ";")[[1]]
  gerek <- c(kir, if (x$cografya != "-") x$cografya)
  if (nzchar(x$filtre)) gerek <- c(gerek, sub("=.*", "", strsplit(x$filtre, ";")[[1]]))
  eksik <- setdiff(gerek, baslik)
  if (length(eksik)) hata <- c(hata, paste(x$kod, ": ham dosyada sütun yok:", paste(eksik, collapse = ",")))
  if (length(kir) < 1 || length(kir) > 3) hata <- c(hata, paste(x$kod, ": kırılım sayısı 1-3 olmalı"))
}
if (length(hata)) stop(paste(hata, collapse = "\n"))
cat(sprintf("%d tablo, ayar tamam\n", nrow(t)))
