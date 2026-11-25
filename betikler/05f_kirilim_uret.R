.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))
source(file.path(dirname(normalizePath(.f, winslash = "/")), "kirilim_yardimci.R"))

# Kırılım aracı verisi -> <SITE>/veri/kirilim/<kod>.js  ve  veri/kirilim_dizin.js
#   window.KIRILIM_TABLO["<kod>"]  (tablo başına, seçilince yüklenir)
#   window.KIRILIM_DIZIN           (açılışta yüklenen küçük dizin)
#
# Tablolar ayar/kirilim_tablolar.csv içinde. Çalıştırma:
#   PORTAL_SITE=site-lab Rscript betikler/05f_kirilim_uret.R
# Doğrulaması ya da etiketi tutmayan tablo yazılmaz; nedeni sonda listelenir.
# Elle etiket düzeltmesi: ayar/kirilim_etiket_duzeltme.csv

suppressMessages(library(jsonlite))

il_re <- "^TR[0-9A-C][0-9]{2}$"
bolge_re <- "^TR[0-9A-C][0-9]$"   # İBBS-2 (26 bölge); yalnız bülten çapraz tablolarında (BULTEN_KIR_*)
HEDEF_BOYUT <- 1.5 * 1024 * 1024
BOYUT_DISI <- c("DATAFLOW", "FREQ", "TIME_PERIOD", "OBS_VALUE", "OBS_STATUS",
                "CONF_STATUS", "TIME_FORMAT", "DECIMALS", "DECIMAL", "UNIT_MULT",
                "YAYIM_DONEMI", "AY", "TRANSFORMATION")
KONULAR <- c("Nüfus ve demografi", "Ekonomi", "Eğitim", "Kalkınma",
             "Yaşam koşulları", "Göç")

csv_oku <- function(yol, ...) {
  read.csv(file.path(PORTAL, yol), fileEncoding = "UTF-8",
           stringsAsFactors = FALSE, ...)
}

ham_oku <- function(df_id) {
  for (klasor in c("ham", "ham_ekonomi", "ham_tum")) {
    for (uzanti in c(".csv", ".csv.gz")) {
      f <- file.path(PORTAL, "veri", klasor, paste0(df_id, uzanti))
      if (file.exists(f)) {
        return(read.csv(if (grepl("[.]gz$", f)) gzfile(f) else f,
                        colClasses = "character", check.names = FALSE,
                        encoding = "UTF-8"))
      }
    }
  }
  stop("Ham dosya bulunamadı: ", df_id)
}

# Bir tabloyu üretir. Döner: list(tablo = ...) ya da list(neden = "...").
uret <- function(t, son_yil) {
  d <- ham_oku(t$df_id)
  if (nzchar(t$filtre)) {
    for (p in strsplit(strsplit(t$filtre, ";")[[1]], "=")) d <- d[d[[p[1]]] == p[2], ]
  }
  cog <- t$cografya
  # cografya "-": tabloda coğrafya sütunu yok, tümü Türkiye
  if (cog == "-") { d$REF_AREA_ <- "TR"; cog <- "REF_AREA_" }
  kir <- strsplit(t$kirilimlar, ";")[[1]]
  # tek kırılımlı tablo (bölüm, meslek vb.): arayüz en az iki eksen ister,
  # tek değerli yapay bir sütun eksen olarak eklenir
  tek <- length(kir) == 1
  if (tek) { d$SUTUN_ <- "V"; kir <- c(kir, "SUTUN_") }
  d[[cog]][d[[cog]] == "_T"] <- "TR"
  # ülke karşılaştırma tablolarında tek, il olmayan bir REF_AREA kodu (ör. "W") Türkiye gibi okunur
  u <- unique(d[[cog]])
  if (length(u) == 1 && u != "TR" && !grepl(il_re, u)) d[[cog]] <- "TR"
  bolge_ok <- startsWith(t$df_id, "BULTEN_KIR_")
  d <- d[d[[cog]] == "TR" | grepl(il_re, d[[cog]]) | (bolge_ok & grepl(bolge_re, d[[cog]])), ]
  d$v <- suppressWarnings(as.numeric(d$OBS_VALUE))
  d <- d[!is.na(d$v), ]
  if (!nrow(d)) return(list(neden = "filtre sonrası satır kalmadı"))

  # kırılım ve filtre dışındaki her boyut tek değerli olmalı
  for (b in setdiff(names(d), c(BOYUT_DISI, "OBS_VALUE", "v", cog, kir))) {
    if (length(unique(d[[b]])) > 1) {
      return(list(neden = sprintf("'%s' boyutu birden çok değerli, filtre ister", b)))
    }
  }

  donemler <- tail(sort(unique(d$TIME_PERIOD)), son_yil)
  d <- d[d$TIME_PERIOD %in% donemler, ]
  y <- tryCatch(yogunlastir(d, kir, cog, ad = t$df_id), error = function(e) e)
  if (inherits(y, "error")) return(list(neden = conditionMessage(y)))

  if (t$turkiye == "topla") {
    for (dn in names(y$v)) {
      m <- do.call(rbind, y$v[[dn]])
      y$v[[dn]]$TR <- ifelse(colSums(!is.na(m)) == 0, NA, colSums(m, na.rm = TRUE))
      y$v[[dn]] <- y$v[[dn]][c("TR", setdiff(names(y$v[[dn]]), "TR"))]
    }
    y$yerler <- c("TR", setdiff(y$yerler, "TR"))
  }

  tn <- strsplit(t$toplanamaz, ";")[[1]]  # toplanamayan (kesişen) kırılımlar
  hatalar <- dogrula(y, t$olcu, t$turkiye, yuvarlak = t$birim == "bin kişi", atla = tn)
  if (length(hatalar)) return(list(neden = paste("doğrulama:", paste(hatalar, collapse = "; "))))

  kirilimlar <- vector("list", length(kir))
  for (i in seq_along(kir)) {
    kodlar <- y$kirilimlar[[i]]$kodlar
    if (kir[i] == "SUTUN_") {
      kirilimlar[[i]] <- list(kol = "SUTUN_", ad = "Değer", kodlar = I(kodlar),
                              etiketler = I(t$birim), toplam = FALSE,
                              ortusen = FALSE, toplamAnlamsiz = TRUE)
      next
    }
    e <- etiket_coz(t$df_id, kir[i], kodlar, et, duzeltme)
    if (length(e$eksik)) {
      return(list(neden = sprintf("etiketsiz kod (%s): %s", kir[i], paste(e$eksik, collapse = ","))))
    }
    ad <- boyut_ad_csv$boyut_ad[match(paste(t$df_id, kir[i]), paste(boyut_ad_csv$df_id, boyut_ad_csv$boyut))]
    if (is.na(ad)) ad <- et$boyut_ad[match(kir[i], et$boyut)]
    kirilimlar[[i]] <- list(kol = kir[i], ad = if (is.na(ad)) kir[i] else ad,
                            kodlar = I(kodlar), etiketler = I(e$etiket),
                            toplam = "_T" %in% kodlar, ortusen = ortusen_var(kodlar) || kir[i] %in% tn,
                            toplamAnlamsiz = kir[i] %in% tn)
  }

  list(tablo = list(
    kod = t$kod, baslik = t$baslik, df_id = t$df_id, konu = t$konu,
    birim = t$birim, olcu = t$olcu, aciklama = t$aciklama, turkiye = t$turkiye,
    donemler = I(y$donemler), donemEtiket = I(donem_etiketi(y$donemler)),
    yerler = I(y$yerler), kirilimlar = kirilimlar, v = y$v))
}

yaz_tablo <- function(tablo) {
  json <- toJSON(tablo, auto_unbox = TRUE, na = "null", digits = NA)
  yol <- file.path(SITE, "veri/kirilim", paste0(tablo$kod, ".js"))
  dir.create(dirname(yol), recursive = TRUE, showWarnings = FALSE)
  writeLines(sprintf('window.KIRILIM_TABLO=window.KIRILIM_TABLO||{};window.KIRILIM_TABLO["%s"]=%s;',
                     tablo$kod, json), yol, useBytes = TRUE)
  yol
}

# --- girdiler ---------------------------------------------------------------
tablolar <- csv_oku("ayar/kirilim_tablolar.csv", na.strings = character())
et <- csv_oku("veri/etiket_kod.csv", colClasses = "character")
duzeltme <- csv_oku("ayar/kirilim_etiket_duzeltme.csv", colClasses = "character")
boyut_ad_csv <- if (file.exists(file.path(PORTAL, "ayar/kirilim_boyut_ad.csv"))) csv_oku("ayar/kirilim_boyut_ad.csv", colClasses = "character") else data.frame(df_id = character(), boyut = character(), boyut_ad = character())
yer <- csv_oku("ayar/yer_adlari.csv")
yer <- yer[yer$kod == "TR" | grepl(il_re, yer$kod) | grepl(bolge_re, yer$kod), ]
yer <- yer[order(yer$kod != "TR", yer$kod), ]

# --- üretim -----------------------------------------------------------------
yazilan <- list()
atilan <- character()
for (i in seq_len(nrow(tablolar))) {
  t <- tablolar[i, ]
  cat(sprintf("[%d/%d] %s\n", i, nrow(tablolar), t$kod))
  son_yil <- t$son_yil_sayisi
  repeat {
    r <- tryCatch(uret(t, son_yil), error = function(e) list(neden = conditionMessage(e)))
    if (is.null(r$tablo)) break
    yol <- yaz_tablo(r$tablo)
    if (file.size(yol) <= HEDEF_BOYUT || son_yil <= 1) break
    son_yil <- max(1, floor(son_yil * 0.7))
    cat(sprintf("   %.1f MB > hedef; son_yil_sayisi %d'e düşürülüyor\n", file.size(yol) / 1024^2, son_yil))
  }
  if (is.null(r$tablo)) {
    atilan <- c(atilan, sprintf("%s: %s", t$kod, r$neden))
    next
  }
  cat(sprintf("   %d dönem, %d yer, %.0f KB\n", length(r$tablo$donemler), length(r$tablo$yerler), file.size(yol) / 1024))
  yazilan[[t$kod]] <- list(t = t, tablo = r$tablo)
}

# --- dizin ------------------------------------------------------------------
konu_listesi <- list()
for (k in KONULAR) {
  ks <- Filter(function(x) x$t$konu == k, yazilan)
  if (!length(ks)) next
  konu_listesi[[length(konu_listesi) + 1]] <- list(
    ad = k,
    tablolar = lapply(unname(ks), function(x) list(
      kod = x$t$kod, baslik = x$t$baslik, olcu = x$t$olcu,
      df = I(c(x$t$df_id, Filter(nzchar, strsplit(x$t$baglanti_df, ";")[[1]]))),
      cografya = if (length(x$tablo$yerler) > 1) { if (any(grepl(bolge_re, x$tablo$yerler))) "bolge" else "il" } else "tr",
      kirilimSayisi = length(x$tablo$kirilimlar),
      kirilimlar = lapply(x$tablo$kirilimlar, function(b) list(kol = b$kol, ad = b$ad)),
      dosya = paste0("veri/kirilim/", x$t$kod, ".js"))))
}
dizin <- list(uretim = format(Sys.time(), "%Y-%m-%d %H:%M"),
              yerler = yer, konular = konu_listesi)
yol <- file.path(SITE, "veri/kirilim_dizin.js")
writeLines(paste0("window.KIRILIM_DIZIN=", toJSON(dizin, auto_unbox = TRUE), ";"), yol, useBytes = TRUE)

cat(sprintf("\nyazılan: %d, atılan: %d\n", length(yazilan), length(atilan)))
for (a in atilan) cat("  ATILDI ", a, "\n")
