.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))

# TÜİK Yıllık GSYH bülteni ek tabloları (xls)
#   ->  veri/ham/BULTEN_GSYH_ULUSAL.csv
#
# API'de yalnız bölgesel hesaplar (en geç 2024) var; ulusal yıllık seri
# bültenden okunur. Dosyalar (veriportali.tuik.gov.tr > Yıllık Gayrisafi
# Yurt İçi Hasıla bülteni > İstatistiksel tablolar), veri/manuel_indirilenler/ içine:
#
#   gsyh_yillik_<yil>_gsyh_kisibasi.xls
#     ("Gayri Safi Yurt İçi Hasıla ve Kişi Başına GSYH": yıl, yıl ortası
#      nüfus, kişi başı TL ve USD)
#   gsyh_yillik_<yil>_hacim.xls
#     ("GSYH, harcama yöntemiyle zincirlenmiş hacim": yıl, endeks, hacim,
#      değişim oranı)
#
# Her yeni bülten için (Mart/Ağustos) yeni yıl dosyaları eklenir; en yeni
# yıl adı geçerli olur.
#
# Kullanım: Rscript betikler/05b_gsyh_bulten_oku.R

suppressMessages(library(readxl))


# --- yardımcı fonksiyonlar ---------------------------------------------

oku <- function(f) {
  d <- as.data.frame(
    read_excel(f, sheet = 1, col_names = FALSE, .name_repair = "minimal"),
    stringsAsFactors = FALSE)
  d[] <- lapply(d, as.character)
  d
}

# ilk sütunu yılla başlayan satırlar
yilsatir <- function(d) which(grepl("^(19|20)[0-9]{2}", trimws(d[[1]])))

sayi <- function(x) suppressWarnings(as.numeric(x))

# adı desene uyan dosyalardan sıralamada sonuncusu (en yeni yıl)
son <- function(patern) {
  f <- list.files(file.path(PORTAL, "veri/manuel_indirilenler"), patern,
                  full.names = TRUE)
  if (!length(f)) stop("dosya yok: ", patern)
  sort(f)[length(f)]
}


# --- kişi başı GSYH tablosu --------------------------------------------

fk <- son("^gsyh_yillik_[0-9]{4}_gsyh_kisibasi.xls$")
fh <- son("^gsyh_yillik_[0-9]{4}_hacim.xls$")

a <- oku(fk)
k <- yilsatir(a)
yil <- substr(trimws(a[[1]][k]), 1, 4)
nufus <- sayi(a[[2]][k])
kbtl <- sayi(a[[4]][k])
kbusd <- sayi(a[[6]][k])

# sütun düzeni değişmişse sessizce yanlış veri üretmesin
if (!(tail(kbtl, 1) > 1e5 && tail(kbusd, 1) > 1e3 && tail(nufus, 1) > 5e4)) {
  stop("kisi basi tablosu sutun duzeni beklenenden farkli: ", basename(fk))
}


# --- hacim (büyüme) tablosu --------------------------------------------

b <- oku(fh)
k2 <- yilsatir(b)
yil2 <- substr(trimws(b[[1]][k2]), 1, 4)
buy <- sayi(b[[5]][k2])

ara <- "de.i.im"
baslik_var <- any(grepl(ara, b[1:6, 5], ignore.case = TRUE) |
                    grepl(ara, unlist(b[1:6, 5])))
if (!baslik_var && !any(grepl(ara, unlist(b[1:6, ]), ignore.case = TRUE))) {
  stop("hacim tablosu duzeni beklenenden farkli")
}


# --- çıktı -------------------------------------------------------------

mk <- function(g, y, v) {
  data.frame(REF_AREA = "TR", TIME_PERIOD = y, GOSTERGE = g, OBS_VALUE = v,
             KAYNAK_DOSYA = basename(fk), stringsAsFactors = FALSE)
}

cikti <- rbind(
  # TL x bin kişi = bin TL; /1e6 -> milyar TL
  mk("GSYH_CARI_MILYAR_TL", yil, round(kbtl * nufus / 1e6, 3)),
  mk("KISI_BASI_TL", yil, kbtl),
  mk("KISI_BASI_USD", yil, kbusd),
  transform(mk("BUYUME", yil2, buy), KAYNAK_DOSYA = basename(fh))
)
cikti <- cikti[!is.na(cikti$OBS_VALUE), ]

# seri değişim yılında çakışan satırlar: sonuncu geçerli
cikti <- cikti[!duplicated(cikti[, c("TIME_PERIOD", "GOSTERGE")],
                           fromLast = TRUE), ]

dir.create(file.path(PORTAL, "veri/ham"), showWarnings = FALSE)
write.csv(cikti, file.path(PORTAL, "veri/ham/BULTEN_GSYH_ULUSAL.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf("Yazildi: veri/ham/BULTEN_GSYH_ULUSAL.csv (%d satir; son yil %s)\n",
            nrow(cikti), max(cikti$TIME_PERIOD)))
