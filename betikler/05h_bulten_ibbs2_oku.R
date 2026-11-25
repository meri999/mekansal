.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))
source(file.path(dirname(normalizePath(.f, winslash = "/")), "bulten_yardimci.R"))

# İBBS-2 (26 bölge) düzeyli TÜİK bülten tabloları (gelir ve yaşam koşulları)
#   ->  veri/ham/BULTEN_IBBS2.csv     (REF_AREA: TR veya İBBS-2 kodu, örn. TR10)
#   ->  veri/ham/BULTEN_IBBS2_IL.csv  (her il, bağlı olduğu İBBS-2 bölgesinin değerini alır)
#
# Gösterge kartlarında il düzeyinde "bölge değeri" olarak görünür; ayrı İBBS-2 dosyası
# Kırılım Gezgini'nin Bölge düzeyi için kullanılır.
#
# Kullanım: Rscript betikler/05h_bulten_ibbs2_oku.R

YER <- read.csv(file.path(PORTAL, "ayar/yer_adlari.csv"), fileEncoding = "UTF-8", stringsAsFactors = FALSE)
HAM <- file.path(PORTAL, "veri/ham")
sayi <- function(x) suppressWarnings(as.numeric(x))
YIL_HUCRE <- "^(19|20)[0-9]{2}$"

# Geniş tablo: satır başında "TR10 (İstanbul)" gibi etiket, sütun başlıkları yıl.
oku_ibbs2_genis <- function(klasor, parca, gosterge) {
  d <- bulten_oku(bulten_dosya(klasor, parca))
  yil_satiri <- which(apply(d, 1, function(r) sum(grepl(YIL_HUCRE, trimws(r)), na.rm = TRUE) >= 3))[1]
  if (is.na(yil_satiri)) stop("ibbs2: yıl başlık satırı bulunamadı: ", parca, call. = FALSE)
  yil_sut <- which(grepl(YIL_HUCRE, trimws(unlist(d[yil_satiri, ]))))
  kod <- ibbs2_kodu(d[[1]])
  satir <- which(seq_len(nrow(d)) > yil_satiri & !is.na(kod))
  cikti <- do.call(rbind, lapply(yil_sut, function(j) {
    data.frame(REF_AREA = kod[satir], TIME_PERIOD = trimws(d[yil_satiri, j]), GOSTERGE = gosterge,
               OBS_VALUE = sayi(d[satir, j]), stringsAsFactors = FALSE)
  }))
  cikti <- cikti[!is.na(cikti$OBS_VALUE), ]
  n <- length(unique(cikti$REF_AREA))
  if (n != 27) stop("ibbs2: ", gosterge, ": ", n, " bölge bulundu, 26 bölge + Türkiye bekleniyor", call. = FALSE)
  cikti
}

# Göreli yoksulluk oranı (medyan gelirin %50'sine göre; 2024-2025): blok başlığı "Medyan gelirin %50'sine göre".
oku_ibbs2_goreli_yoksulluk <- function() {
  d <- bulten_oku(bulten_dosya("yoksulluk_yasam_kosullari_2025", "(Türkiye-İBBS 2. Düzey)"))
  yil_satiri <- which(apply(d, 1, function(r) sum(grepl(YIL_HUCRE, trimws(r)), na.rm = TRUE) >= 3))[1]
  if (is.na(yil_satiri)) stop("göreli yoksulluk: yıl satırı bulunamadı", call. = FALSE)
  baslik_dogrula(d, 1:yil_satiri, 8, "Yoksulluk oranı", "göreli yoksulluk")
  baslik_dogrula(d, 1:yil_satiri, 5, "Yoksul sayısı", "göreli yoksulluk")
  blok <- which(grepl("^Medyan gelirin %50", trimws(d[[1]])))[1]
  if (is.na(blok)) stop("göreli yoksulluk: %50 bloğu bulunamadı", call. = FALSE)
  son <- which(grepl("^Medyan gelirin", trimws(d[[1]])) & seq_len(nrow(d)) > blok)
  son <- if (length(son)) son[1] - 1 else nrow(d)
  kod <- ibbs2_kodu(d[[1]])
  satir <- which(seq_len(nrow(d)) > blok & seq_len(nrow(d)) <= son & !is.na(kod))
  cikti <- do.call(rbind, lapply(c(8, 9), function(j) {
    data.frame(REF_AREA = kod[satir], TIME_PERIOD = trimws(d[yil_satiri, j]), GOSTERGE = "GORELI_YOKSULLUK",
               OBS_VALUE = sayi(d[satir, j]), stringsAsFactors = FALSE)
  }))
  cikti <- cikti[!is.na(cikti$OBS_VALUE), ]
  n <- length(unique(cikti$REF_AREA))
  if (n != 27) stop("göreli yoksulluk: ", n, " bölge bulundu, 27 bekleniyor", call. = FALSE)
  cikti
}

oku_ibbs2 <- function() {
  rbind(
    oku_ibbs2_goreli_yoksulluk(),
    oku_ibbs2_genis("yoksulluk_yasam_kosullari_2025", "Bölge bazında yoksulluk veya sosyal dışlanma", "YOKSULLUK_DISLANMA_RISKI"),
    oku_ibbs2_genis("gelir_dagilimi_2025", "fert gelirine göre Gini katsayısı.xls", "GINI"),
    oku_ibbs2_genis("gelir_dagilimi_2025", "fert gelirine göre P80_P20 oranı.xls", "P80_P20")
  )
}

# Her ile bağlı olduğu İBBS-2 bölgesinin değerini verir; Türkiye satırı aynen kalır.
ibbs2_il_a_yay <- function(ib) {
  esle <- ibbs2_il_eslemesi(YER)
  bolge <- ib[ib$REF_AREA != "TR", ]
  yay <- merge(esle, bolge, by.x = "ibbs2", by.y = "REF_AREA")
  bolge_siz <- setdiff(unique(esle$ibbs2), unique(bolge$REF_AREA))
  if (length(bolge_siz)) stop("ibbs2: veride olmayan bölgeler: ", paste(bolge_siz, collapse = ","), call. = FALSE)
  il <- data.frame(REF_AREA = yay$il, TIME_PERIOD = yay$TIME_PERIOD, GOSTERGE = yay$GOSTERGE,
                   OBS_VALUE = yay$OBS_VALUE, stringsAsFactors = FALSE)
  rbind(ib[ib$REF_AREA == "TR", ], il)
}

if (sys.nframe() == 0) {
  ib <- oku_ibbs2()
  uzun_yaz(ib, file.path(HAM, "BULTEN_IBBS2.csv"))
  il <- ibbs2_il_a_yay(ib)
  il_say_dogrula(il, "ibbs2 il genişletmesi")
  uzun_yaz(il, file.path(HAM, "BULTEN_IBBS2_IL.csv"))
  cat(sprintf("BULTEN_IBBS2: %d satır; BULTEN_IBBS2_IL: %d satır, dönem %s..%s\n",
              nrow(ib), nrow(il), min(ib$TIME_PERIOD), max(ib$TIME_PERIOD)))
}
