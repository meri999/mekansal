# Bülten (TÜİK haber bülteni Excel) okuma yardımcıları.
# Kullanım: source(file.path(dirname(...), "bulten_yardimci.R")) (yollar.R önce yüklenmiş olmalı: PORTAL).
#
# Akış: bulten_dosya() Excel'i bulur -> bulten_oku() hücreleri karakter olarak okur
# -> il_kodu()/ibbs2_kodu() adları koda çevirir -> uzun_yaz() veri/ham/BULTEN_*.csv yazar.

suppressMessages(library(readxl))

BULTEN_KOK <- file.path(PORTAL, "veri/manuel_indirilenler/bultenler")
UZUN_SUTUNLAR <- c("REF_AREA", "TIME_PERIOD", "GOSTERGE", "OBS_VALUE")

# <klasor> içinde adında <parca> geçen dosyanın yolu (çoksa en yeni, yoksa hata).
# Windows 260 karakter sınırını aşan adlar kısa bir geçici yola kopyalanır.
bulten_dosya <- function(klasor, parca) {
  dizin <- file.path(BULTEN_KOK, klasor)
  adlar <- list.files(dizin)
  uyan <- adlar[grepl(parca, adlar, fixed = TRUE)]
  if (!length(uyan)) stop("bülten dosyası yok: ", klasor, " / ", parca, call. = FALSE)
  yollar <- file.path(dizin, uyan)
  bilgi <- file.info(yollar)
  yol <- yollar[which.max(replace(bilgi$mtime, is.na(bilgi$mtime), as.POSIXct(0, origin = "1970-01-01")))]
  if (nchar(normalizePath(yol, winslash = "/", mustWork = FALSE)) < 240 && file.exists(yol)) return(yol)
  hedef <- tempfile(fileext = paste0(".", tools::file_ext(yol)))
  kopya <- file.copy(paste0("\\\\?\\", normalizePath(yol, winslash = "\\", mustWork = FALSE)), hedef)
  if (!isTRUE(kopya)) stop("uzun yollu bülten dosyası kopyalanamadı: ", uyan[1], call. = FALSE)
  hedef
}

# İlk sayfa, başlıksız, tüm sütunlar karakter.
bulten_oku <- function(yol) {
  # Uzantısı .xls olan bazı dosyalar aslında .xlsx (zip, "PK" imzası): geçici .xlsx kopyasından okunur.
  if (identical(readBin(yol, "raw", 2), as.raw(c(0x50, 0x4b)))) {
    kopya <- tempfile(fileext = ".xlsx")
    file.copy(yol, kopya)
    yol <- kopya
  }
  d <-as.data.frame(read_excel(yol, sheet = 1, col_names = FALSE, .name_repair = "minimal"),
                     stringsAsFactors = FALSE)
  d[] <- lapply(d, as.character)
  d
}

# Ad karşılaştırması için: boşluk/satır sonu temizlenir, çift dilli "Province" eki atılır,
# Türkçe harfler ASCII büyük harfe çevrilir.
ad_anahtari <- function(x) {
  x <- gsub("\\s+", " ", trimws(as.character(x)))
  x <- sub(" Province.*$", "", x)
  x <- chartr("çğıöşüÇĞİÖŞÜ", "CGIOSUCGIOSU", x)
  toupper(trimws(x))
}

IL_KOD_RE <- "^TR[0-9A-C][0-9]{2}$"

# Bülten yazımındaki il adı -> TR1xx biçiminde il kodu (eşleşmeyen NA).
# takma: bulten_ad, yer_kodu sütunlu data.frame (yazım farkları).
il_kodu <- function(ad, yer, takma = NULL) {
  il <- yer[grepl(IL_KOD_RE, yer$kod), ]
  tablo <- stats::setNames(il$kod, ad_anahtari(il$ad_tr))
  if (!is.null(takma) && nrow(takma)) {
    tablo <- c(tablo, stats::setNames(takma$yer_kodu, ad_anahtari(takma$bulten_ad)))
  }
  unname(tablo[ad_anahtari(ad)])
}

# "TR10 (İstanbul)" -> "TR10"; "TR Türkiye" -> "TR"; diğerleri NA.
ibbs2_kodu <- function(etiket) {
  m <- regmatches(trimws(etiket), regexec("^(TR[0-9A-C]?[0-9]?)(\\s|\\(|$)", trimws(etiket)))
  vapply(m, function(z) if (length(z) >= 2) z[2] else NA_character_, "")
}

# il kodu (TR100) -> İBBS-2 bölge kodu (TR10): ilk 4 karakter.
ibbs2_il_eslemesi <- function(yer) {
  il <- yer$kod[grepl(IL_KOD_RE, yer$kod)]
  data.frame(il = il, ibbs2 = substr(il, 1, 4), stringsAsFactors = FALSE)
}

# Uzun biçimde yazar; (REF_AREA, TIME_PERIOD, GOSTERGE) yinelenirse hata.
uzun_yaz <- function(df, yol) {
  eksik <- setdiff(UZUN_SUTUNLAR, names(df))
  if (length(eksik)) stop("uzun_yaz: sütun yok: ", paste(eksik, collapse = ","), call. = FALSE)
  anahtar <- paste(df$REF_AREA, df$TIME_PERIOD, df$GOSTERGE, sep = "|")
  if (anyDuplicated(anahtar)) {
    stop("uzun_yaz: yinelenen gözlem: ", paste(head(anahtar[duplicated(anahtar)], 3), collapse = "; "), call. = FALSE)
  }
  ek <- setdiff(names(df), UZUN_SUTUNLAR)
  utils::write.csv(df[, c(UZUN_SUTUNLAR, ek), drop = FALSE], yol, row.names = FALSE,
                   fileEncoding = "UTF-8", na = "")
  invisible(yol)
}

# Sütun başlığı beklenen metni içeriyor mu? Değilse dosya biçimi değişmiştir: dur.
baslik_dogrula <- function(d, satirlar, sutun, metin, ad) {
  hucre <- paste(d[satirlar, sutun], collapse = " ")
  if (!grepl(metin, hucre, fixed = TRUE)) stop(ad, ": ", sutun, ". sütunda '", metin, "' yok: ", hucre, call. = FALSE)
  invisible(TRUE)
}

# Okuyucu çıktısında (Türkiye hariç) tam `beklenen` il olmalı; eksik il sessizce geçmesin.
il_say_dogrula <- function(df, ad, beklenen = 81) {
  n <- length(setdiff(unique(df$REF_AREA), "TR"))
  if (n != beklenen) stop(ad, ": ", n, " il bulundu, ", beklenen, " bekleniyor", call. = FALSE)
  TRUE
}
