.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
BETIK_DIZIN <- dirname(normalizePath(.f, winslash = "/"))
source(file.path(BETIK_DIZIN, "yollar.R"))
source(file.path(BETIK_DIZIN, "bulten_yardimci.R"))
source(file.path(BETIK_DIZIN, "bulten_kirilim.R"))

# Bülten Excel çapraz tabloları -> Kırılım Gezgini ham girdisi.
#   Tablo tanımları: betikler/kirilim_bulten/*.R  (her dosya KIR_TABLOLAR listesine ekler)
#   Çıktı: veri/ham/BULTEN_KIR_<ad>.csv, ayar/kirilim_etiket_duzeltme.csv, ayar/kirilim_boyut_ad.csv,
#          ayar/kirilim_tablolar.csv (kod'a göre ekler/yeniler)
#   Sonra: PORTAL_SITE=site-lab Rscript betikler/05f_kirilim_uret.R
# Kullanım: Rscript betikler/05j_bulten_kirilim_oku.R [--kod=a,b]
#
# Tanım biçimi (her eleman): list(ad, kod, konu, baslik, oku = function() <capraz_oku sonucu>,
#   boyut_ad = c(EKSEN = "Ad", ...) (kırılım eksenleri, sırayla), turkiye = "var"|"topla",
#   olcu = "sayi"|"oran", birim, son_yil_sayisi, aciklama, toplanamaz = "", baglanti_df = "")
KIR_TABLOLAR <- list()
for (f in sort(list.files(file.path(BETIK_DIZIN, "kirilim_bulten"), "[.]R$", full.names = TRUE))) source(f, encoding = "UTF-8")

KIR_TABLO_DOSYA <- file.path(PORTAL, "ayar/kirilim_tablolar.csv")
KIR_TABLO_SUTUN <- c("kod", "konu", "baslik", "df_id", "filtre", "cografya", "kirilimlar", "turkiye", "olcu",
                     "birim", "son_yil_sayisi", "aciklama", "toplanamaz", "baglanti_df")

kayit_yaz <- function(t, df_id) {
  yeni <- data.frame(kod = t$kod, konu = t$konu, baslik = t$baslik, df_id = df_id, filtre = "",
                     cografya = "REF_AREA", kirilimlar = paste(names(t$boyut_ad), collapse = ";"),
                     turkiye = t$turkiye, olcu = t$olcu, birim = t$birim, son_yil_sayisi = as.character(t$son_yil_sayisi),
                     aciklama = t$aciklama, toplanamaz = if (is.null(t$toplanamaz)) "" else t$toplanamaz,
                     baglanti_df = if (is.null(t$baglanti_df)) "" else t$baglanti_df, stringsAsFactors = FALSE)
  eski <- read.csv(KIR_TABLO_DOSYA, fileEncoding = "UTF-8", colClasses = "character", na.strings = character())
  eski <- eski[eski$kod != t$kod, KIR_TABLO_SUTUN]
  utils::write.csv(rbind(eski, yeni[, KIR_TABLO_SUTUN]), KIR_TABLO_DOSYA, row.names = FALSE, fileEncoding = "UTF-8")
}

arg <- grep("^--kod=", commandArgs(TRUE), value = TRUE)
secili <- if (length(arg)) strsplit(sub("^--kod=", "", arg[1]), ",")[[1]] else NULL
if (sys.nframe() == 0) {
  hata <- character()
  for (t in KIR_TABLOLAR) {
    if (!is.null(secili) && !t$kod %in% secili) next
    r <- tryCatch({
      s <- t$oku()
      if (!"TIME_PERIOD" %in% names(s$veri)) stop("TIME_PERIOD yok")
      if (!"REF_AREA" %in% names(s$veri)) s$veri$REF_AREA <- "TR"
      s <- kir_son(s, names(t$boyut_ad), ad = t$kod)
      id <- kir_yaz(s, t$ad, t$boyut_ad)
      kayit_yaz(t, id)
      sprintf("%s: %d gözlem, %d yer, dönem %s", t$kod, nrow(s$veri), length(unique(s$veri$REF_AREA)), paste(range(s$veri$TIME_PERIOD), collapse = ".."))
    }, error = function(e) { hata <<- c(hata, sprintf("%s: %s", t$kod, conditionMessage(e))); NULL })
    if (!is.null(r)) cat(r, "\n")
  }
  if (length(hata)) { cat("\nHATA:\n", paste0("  ", hata, collapse = "\n"), "\n"); quit(status = 1) }
}
