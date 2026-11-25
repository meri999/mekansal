.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))
source(file.path(dirname(normalizePath(.f, winslash = "/")), "bulten_yardimci.R"))

# İl düzeyli TÜİK bülten tabloları (xls)  ->  veri/ham/BULTEN_<AD>_IL.csv (uzun biçim)
#
# Dosyalar veri/manuel_indirilenler/bultenler/<bülten>/ altındadır (bkz. ENVANTER.csv).
# Doğrudan çalıştırılınca bütün okuyucular çalışır ve CSV yazar; source edilince
# yalnız işlevler tanımlanır (testler böyle kullanır).
#
# Kullanım: Rscript betikler/05g_bulten_il_oku.R

YER <- read.csv(file.path(PORTAL, "ayar/yer_adlari.csv"), fileEncoding = "UTF-8", stringsAsFactors = FALSE)
TAKMA <- read.csv(file.path(PORTAL, "ayar/il_ad_takma.csv"), fileEncoding = "UTF-8", stringsAsFactors = FALSE)
HAM <- file.path(PORTAL, "veri/ham")

sayi <- function(x) suppressWarnings(as.numeric(x))

# Başlık satırındaki son yıl (örn. "..., 2025" -> "2025").
baslik_yili <- function(d, satir = 1) {
  m <- regmatches(d[satir, 1], gregexpr("(19|20)[0-9]{2}", d[satir, 1]))[[1]]
  if (!length(m)) stop("başlıkta yıl yok: ", d[satir, 1], call. = FALSE)
  tail(m, 1)
}

# `ilk` sütunundaki ad hücrelerini il koduna çevirir. Sayısal değer taşıyıp da
# eşleşmeyen satır (il olmayan "Toplam"/"Türkiye" hariç) varsa durur.
il_satirlari <- function(d, ilk_satir, ad_sutun = 1, deger_sutunu = ad_sutun + 1) {
  adlar <- d[[ad_sutun]]
  kod <- il_kodu(adlar, YER, TAKMA)
  satir <- seq_len(nrow(d))
  sayisal <- !is.na(sayi(d[[deger_sutunu]]))
  toplam <- grepl("^(Toplam|Türkiye|Total)", trimws(adlar))
  eksik <- satir >= ilk_satir & sayisal & is.na(kod) & !toplam
  if (any(eksik)) {
    stop("il kodu bulunamayan satırlar: ", paste(trimws(adlar[eksik]), collapse = "; "),
         " (ayar/il_ad_takma.csv'ye ekleyin)", call. = FALSE)
  }
  list(satir = which(satir >= ilk_satir & !is.na(kod)), kod = kod)
}

# --- Görev 2: iç göç (net göç hızı dahil) ---------------------------------
# Sütun başlıkları metinle doğrulanır ve aldığı - verdiği = net tutarlılığı denetlenir.
ic_goc_dogrula <- function(d, baslik) {
  baslik_dogrula(d, baslik, 2, "nüfus", "iç göç")
  baslik_dogrula(d, baslik, 3, "Aldığı", "iç göç")
  baslik_dogrula(d, baslik, 4, "Verdiği", "iç göç")
  baslik_dogrula(d, baslik, 5, "Net göç", "iç göç")
  if (grepl("hızı", d[baslik, 5], fixed = TRUE)) stop("iç göç: 5. sütun net göç, hız değil bekleniyor: ", d[baslik, 5], call. = FALSE)
  baslik_dogrula(d, baslik, 6, "hızı", "iç göç")
  s <- (baslik + 1):nrow(d)
  fark <- abs(sayi(d[s, 3]) - sayi(d[s, 4]) - sayi(d[s, 5]))
  if (isTRUE(any(fark > 0.5, na.rm = TRUE))) stop("iç göç: aldığı - verdiği != net göç", call. = FALSE)
  TRUE
}

oku_ic_goc <- function() {
  d <- bulten_oku(bulten_dosya("ic_goc_2025", "net göç hızı"))
  yil <- baslik_yili(d)
  baslik <- which(grepl("^İl", trimws(d[[1]])) & grepl("Province", d[[1]]))[1]
  if (is.na(baslik)) stop("iç göç: 'İl Province' başlık satırı bulunamadı", call. = FALSE)
  ic_goc_dogrula(d, baslik)
  s <- il_satirlari(d, baslik + 1)
  gosterge <- c("NUFUS", "ALDIGI_GOC", "VERDIGI_GOC", "NET_GOC", "NET_GOC_HIZI")
  cikti <- do.call(rbind, lapply(seq_along(gosterge), function(j) {
    data.frame(REF_AREA = s$kod[s$satir], TIME_PERIOD = yil, GOSTERGE = gosterge[j],
               OBS_VALUE = sayi(d[s$satir, j + 1]), stringsAsFactors = FALSE)
  }))
  il_say_dogrula(cikti, "iç göç")
  cikti
}

# --- Görev 3: yapı izin (ruhsat ve kullanma izni, daire sayısı, yıllık) ----
oku_yapi_izin <- function() {
  d <- bulten_oku(bulten_dosya("yapi_izin_2026_q3", "İllere göre yapı belgesi verilen daire sayısı"))
  baslik <- which(grepl("^Yıl", d[[2]]) & grepl("^Çeyrek", d[[3]]))[1]
  if (is.na(baslik)) stop("yapı izin: 'Yıl / Çeyrek' başlık satırı bulunamadı", call. = FALSE)
  il_sut <- 5:ncol(d)
  kod <- il_kodu(unlist(d[baslik, il_sut]), YER, TAKMA)
  if (anyNA(kod)) stop("yapı izin: eşleşmeyen il başlıkları: ",
                       paste(unlist(d[baslik, il_sut])[is.na(kod)], collapse = "; "), call. = FALSE)
  gosterge <- NA_character_
  yil <- NA_character_
  cikti <- list()
  for (r in (baslik + 1):nrow(d)) {
    belge <- trimws(d[r, 1])
    if (grepl("^Yapı ruhsatı", belge)) gosterge <- "RUHSAT_DAIRE"
    if (grepl("^Yapı kullanma", belge)) gosterge <- "KULLANMA_DAIRE"
    if (!is.na(d[r, 2]) && nzchar(trimws(d[r, 2])) && !is.na(sayi(d[r, 2]))) yil <- as.character(round(sayi(d[r, 2])))
    ceyrek <- d[r, 3]
    yillik <- is.na(ceyrek) || !nzchar(trimws(ceyrek))
    deger <- sayi(unlist(d[r, il_sut]))
    if (is.na(gosterge) || is.na(sayi(d[r, 4])) || !yillik || all(is.na(deger))) next
    cikti[[length(cikti) + 1]] <- data.frame(REF_AREA = kod, TIME_PERIOD = yil, GOSTERGE = gosterge,
                                             OBS_VALUE = deger, stringsAsFactors = FALSE)
  }
  cikti <- do.call(rbind, cikti)
  il_say_dogrula(cikti, "yapı izin")
  cikti
}

# --- Görev 4: sinema (il, 2025) ---------------------------------------------
oku_sinema <- function() {
  d <- bulten_oku(bulten_dosya("sinema_gosteri_2025", "İllere göre sinemalarda gösterilen film ve seyirci sayısı"))
  yil <- baslik_yili(d)
  baslik <- which(grepl("^İl(\\s|$)", trimws(d[[1]])))[1]   # "İllere göre..." başlığını almaz
  if (is.na(baslik)) stop("sinema: 'İl' başlık satırı bulunamadı", call. = FALSE)
  ust <- d[baslik - 1, ]
  # sütun konumları başlıktan doğrulanır: salon, koltuk, film (toplam), seyirci (toplam)
  sut <- c(SALON = 2, KOLTUK = 3, FILM = 5, SEYIRCI = 9)
  beklenen <- c(SALON = "Sinema salonu", KOLTUK = "Koltuk", FILM = "Gösterilen film", SEYIRCI = "Seyirci")
  for (g in names(sut)) {
    if (!grepl(beklenen[[g]], ust[[sut[[g]]]], fixed = TRUE)) {
      stop("sinema: ", sut[[g]], ". sütun ", g, " değil: ", ust[[sut[[g]]]], call. = FALSE)
    }
  }
  s <- il_satirlari(d, baslik + 1)
  satir <- c(which(trimws(d[[1]]) == "Türkiye"), s$satir)
  kod <- c("TR", s$kod[s$satir])
  cikti <- lapply(names(sut), function(g) {
    data.frame(REF_AREA = kod, TIME_PERIOD = yil, GOSTERGE = g, OBS_VALUE = sayi(d[satir, sut[[g]]]),
               stringsAsFactors = FALSE)
  })
  cikti <- do.call(rbind, cikti)
  cikti[!is.na(cikti$OBS_VALUE), ]
}

# --- Görev 5: beşeri kalkınma endeksi (il, 2018-2022) -----------------------
oku_beseri_kalkinma <- function() {
  d <- bulten_oku(bulten_dosya("beseri_kalkinma_2022", "İllere göre beşeri kalkınma endeksi"))
  baslik <- which(grepl("^Yıl", trimws(d[[1]])))[1]
  if (is.na(baslik)) stop("beşeri kalkınma: 'Yıl' başlık satırı bulunamadı", call. = FALSE)
  sut <- c(ENDEKS = 7, YASAM_ENDEKSI = 10, EGITIM_ENDEKSI = 13, GELIR_ENDEKSI = 16)
  beklenen <- c(ENDEKS = "Beşeri kalkınma", YASAM_ENDEKSI = "Yaşam", EGITIM_ENDEKSI = "Eğitim", GELIR_ENDEKSI = "Gelir")
  for (g in names(sut)) {
    if (!grepl(beklenen[[g]], d[baslik - 1, sut[[g]]], fixed = TRUE)) {
      stop("beşeri kalkınma: ", sut[[g]], ". sütun ", g, " değil: ", d[baslik - 1, sut[[g]]], call. = FALSE)
    }
  }
  yil <- NA_character_
  yilli <- rep(NA_character_, nrow(d))
  for (r in (baslik + 1):nrow(d)) {
    if (!is.na(sayi(d[r, 1]))) yil <- as.character(round(sayi(d[r, 1])))
    yilli[r] <- yil
  }
  ad <- trimws(d[[5]])
  kod <- ifelse(ad == "Türkiye", "TR", il_kodu(ad, YER, TAKMA))
  satir <- which(seq_len(nrow(d)) > baslik & !is.na(kod) & !is.na(yilli))
  eksik <- seq_len(nrow(d)) > baslik & is.na(kod) & !is.na(sayi(d[[sut[["ENDEKS"]]]]))
  if (any(eksik)) stop("beşeri kalkınma: eşleşmeyen il adları: ", paste(ad[eksik], collapse = "; "), call. = FALSE)
  cikti <- do.call(rbind, lapply(names(sut), function(g) {
    data.frame(REF_AREA = kod[satir], TIME_PERIOD = yilli[satir], GOSTERGE = g,
               OBS_VALUE = sayi(d[satir, sut[[g]]]), stringsAsFactors = FALSE)
  }))
  il_say_dogrula(cikti, "beşeri kalkınma")
  cikti
}

# --- Görev 6: uluslararası göç il tabloları (2025) --------------------------
# Ortak: yıl 1. sütunda yalnız `Toplam-Total` satırında, il adı 2. sütunda.
ugoc_tablo <- function(parca) {
  d <- bulten_oku(bulten_dosya("ugoc_2025", parca))
  baslik <- which(grepl("^Yıl", trimws(d[[1]])))[1]
  if (is.na(baslik)) stop("ugoc: 'Yıl' başlık satırı bulunamadı: ", parca, call. = FALSE)
  ilk <- baslik + 2
  ad <- trimws(d[[2]])
  kod <- ifelse(grepl("^Toplam", ad), "TR", il_kodu(ad, YER, TAKMA))
  satir <- which(seq_len(nrow(d)) >= ilk & !is.na(kod) & !is.na(sayi(d[[3]])))
  eksik <- seq_len(nrow(d)) >= ilk & is.na(kod) & !is.na(sayi(d[[3]]))
  if (any(eksik)) stop("ugoc: eşleşmeyen il adları: ", paste(ad[eksik], collapse = "; "), call. = FALSE)
  # yıl her bloğun Toplam satırında yazılı: aşağı doldur
  yil <- rep(NA_character_, nrow(d))
  son <- NA_character_
  for (r in ilk:nrow(d)) {
    if (!is.na(sayi(d[r, 1])) && grepl("^(19|20)[0-9]{2}$", as.character(round(sayi(d[r, 1]))))) {
      son <- as.character(round(sayi(d[r, 1])))
    }
    yil[r] <- son
  }
  list(d = d, satir = satir, kod = kod, yil = yil)
}

# Vatandaşlık tablosu: sütun başlıkları metinle doğrulanır; gelen ve giden toplamları Türk + yabancıya eşit olmalı.
ugoc_vatandaslik_dogrula <- function(d) {
  baslik <- which(grepl("^Yıl", trimws(d[[1]])))[1]
  if (is.na(baslik)) stop("ugoc vatandaşlık: 'Yıl' başlık satırı bulunamadı", call. = FALSE)
  baslik_dogrula(d, baslik, 7, "Türkiye'ye", "ugoc vatandaşlık")
  baslik_dogrula(d, baslik, 11, "Türkiye'den", "ugoc vatandaşlık")
  baslik_dogrula(d, baslik, 14, "Net", "ugoc vatandaşlık")
  for (j in c(8, 12)) baslik_dogrula(d, baslik + 1, j, "Türk", "ugoc vatandaşlık")
  for (j in c(9, 13)) baslik_dogrula(d, baslik + 1, j, "Yabancı", "ugoc vatandaşlık")
  s <- (baslik + 2):nrow(d)
  for (g in list(c(7, 8, 9), c(11, 12, 13))) {
    fark <- abs(sayi(d[s, g[1]]) - sayi(d[s, g[2]]) - sayi(d[s, g[3]]))
    if (isTRUE(any(fark > 0, na.rm = TRUE))) stop("ugoc vatandaşlık: ", g[1], ". sütun != ", g[2], " + ", g[3], call. = FALSE)
  }
  TRUE
}

oku_ugoc_il <- function() {
  c1 <- ugoc_tablo("İllere ve cinsiyete göre")
  v <- function(t, j) sayi(t$d[t$satir, j])
  if (isTRUE(any(abs(v(c1, 4) - v(c1, 8) - v(c1, 11)) > 0, na.rm = TRUE))) stop("ugoc: gelen - giden != net (cinsiyet tablosu)", call. = FALSE)
  c2 <- ugoc_tablo("İllere ve vatandaşlığa göre")
  ugoc_vatandaslik_dogrula(c2$d)
  if (!identical(sort(unique(c1$yil[c1$satir])), sort(unique(c2$yil[c2$satir])))) {
    stop("ugoc: iki tablonun yılları farklı", call. = FALSE)
  }
  parca <- function(t, gosterge, j) {
    data.frame(REF_AREA = t$kod[t$satir], TIME_PERIOD = t$yil[t$satir], GOSTERGE = gosterge, OBS_VALUE = v(t, j),
               stringsAsFactors = FALSE)
  }
  cikti <- rbind(parca(c1, "GELEN", 4), parca(c1, "GIDEN", 8), parca(c1, "NET", 11),
                 parca(c2, "GELEN_YABANCI", 9), parca(c2, "GIDEN_YABANCI", 13))
  il_say_dogrula(cikti, "uluslararası göç")
  cikti
}

# --- tiyatro il tablosu (2024/25 sezonu; yıl = sezonun bitiş yılı) -----------
oku_tiyatro_il <- function() {
  d <- bulten_oku(bulten_dosya("sinema_gosteri_2025", "İllere göre tiyatrolarda oynanan eser, gösteri ve seyirci"))
  baslik <- which(grepl("^İl(\\s|$)", trimws(d[[1]])))[1]
  if (is.na(baslik)) stop("tiyatro il: 'İl' başlık satırı bulunamadı", call. = FALSE)
  baslik_dogrula(d, baslik, 2, "Tiyatro salonu", "tiyatro il")
  baslik_dogrula(d, baslik, 3, "Koltuk", "tiyatro il")
  baslik_dogrula(d, baslik - 1, 5, "Telif", "tiyatro il")
  baslik_dogrula(d, baslik, 5, "Oynanan eser", "tiyatro il")
  baslik_dogrula(d, baslik, 9, "Gösteri", "tiyatro il")
  baslik_dogrula(d, baslik + 1, 5, "Toplam", "tiyatro il")
  baslik_dogrula(d, baslik + 1, 9, "Toplam", "tiyatro il")
  s <- il_satirlari(d, baslik + 2)
  satir <- s$satir; kod <- s$kod[satir]
  # seyirci: beş tiyatro türünün "Toplam" sütunları (5, 9, 13, 17, 21) toplanır
  d2 <- bulten_oku(bulten_dosya("sinema_gosteri_2025", "yetişkin çocuk tiyatro seyirci"))
  b2 <- which(grepl("^İl(\\s|$)", trimws(d2[[1]])))[1]
  tur <- c(5, 9, 13, 17, 21)
  for (j in tur) baslik_dogrula(d2, b2 + 1, j, "Toplam", "tiyatro seyirci")
  if (!identical(trimws(d2[satir, 1]), trimws(d[satir, 1]))) stop("tiyatro: iki dosyada il sırası farklı", call. = FALSE)
  m <- sapply(tur, function(j) sayi(d2[satir, j]))
  seyirci <- ifelse(rowSums(!is.na(m)) > 0, rowSums(m, na.rm = TRUE), NA_real_)
  tr <- which(trimws(d2[[1]]) == "Türkiye")
  tr_toplam <- sum(sapply(tur, function(j) sayi(d2[tr, j])), na.rm = TRUE)
  if (abs(sum(seyirci, na.rm = TRUE) - tr_toplam) > 0) stop("tiyatro: il seyirci toplamı Türkiye'ye eşit değil", call. = FALSE)
  g <- function(ad, v) data.frame(REF_AREA = kod, TIME_PERIOD = "2025", GOSTERGE = ad, OBS_VALUE = v, stringsAsFactors = FALSE)
  cikti <- rbind(g("SALON", sayi(d[satir, 2])), g("KOLTUK", sayi(d[satir, 3])),
                 g("ESER", sayi(d[satir, 5])), g("GOSTERI", sayi(d[satir, 9])), g("SEYIRCI", seyirci))
  # Türkiye satırı (eser sayısı dipnotlu "10 216(1)" olduğundan okunmaz)
  trs <- which(trimws(d[[1]]) == "Türkiye")[1]
  tr_satir <- data.frame(REF_AREA = "TR", TIME_PERIOD = "2025", GOSTERGE = c("SALON", "KOLTUK", "GOSTERI", "SEYIRCI"),
                         OBS_VALUE = c(sayi(d[trs, 2]), sayi(d[trs, 3]), sayi(d[trs, 9]), tr_toplam), stringsAsFactors = FALSE)
  cikti <- rbind(cikti, tr_satir)
  cikti <- cikti[!is.na(cikti$OBS_VALUE), ]
  il_say_dogrula(cikti, "tiyatro il")
  cikti
}

# --- iç göç nedeni: aldığı ve verdiği göçte neden payları (%) ----------------
# Toplam sütununa oranlanır; neden sütunları toplamı = Toplam doğrulanır.
oku_goc_nedeni <- function() {
  cikti <- list()
  yonler <- c(ALDIGI = "aldığı", VERDIGI = "verdiği")
  for (yk in names(yonler)) {
    yon <- yonler[[yk]]
    d <- bulten_oku(bulten_dosya("ic_goc_2025", paste0("Göç etme nedenine göre illerin ", yon, " göç")))
    yil <- baslik_yili(d, 1)
    baslik <- 4
    baslik_dogrula(d, baslik, 2, "Toplam", "göç nedeni")
    baslik_dogrula(d, baslik, 3, "Tayin", "göç nedeni")
    baslik_dogrula(d, baslik, 4, "iş bulmak", "göç nedeni")
    baslik_dogrula(d, baslik, 5, "Eğitim", "göç nedeni")
    baslik_dogrula(d, baslik, 6, "Medeni durum", "göç nedeni")
    baslik_dogrula(d, baslik, 8, "bağımlı", "göç nedeni")
    baslik_dogrula(d, baslik, 14, "Bilinmeyen", "göç nedeni")
    s <- il_satirlari(d, baslik + 1)
    v <- function(j) sayi(d[s$satir, j])
    topl <- v(2)
    neden_toplami <- rowSums(sapply(3:14, v), na.rm = TRUE)
    if (any(abs(neden_toplami - topl) > 0)) stop("göç nedeni (", yon, "): neden toplamı != toplam", call. = FALSE)
    kod <- s$kod[s$satir]
    pay <- function(ad, j) data.frame(REF_AREA = kod, TIME_PERIOD = yil, GOSTERGE = paste0(yk, "_", ad),
                                      OBS_VALUE = 100 * rowSums(sapply(j, v), na.rm = TRUE) / topl, stringsAsFactors = FALSE)
    cikti <- c(cikti, list(pay("IS_PAYI", c(3, 4)), pay("EGITIM_PAYI", 5), pay("AILE_PAYI", c(6, 8))))
  }
  cikti <- do.call(rbind, cikti)
  il_say_dogrula(cikti, "göç nedeni")
  cikti
}

# --- çalıştır --------------------------------------------------------------
if (sys.nframe() == 0) {
  uretilen <- list(BULTEN_IC_GOC_IL = oku_ic_goc, BULTEN_YAPI_IZIN_IL = oku_yapi_izin,
                   BULTEN_SINEMA_IL = oku_sinema, BULTEN_BESERI_KALKINMA_IL = oku_beseri_kalkinma,
                   BULTEN_UGOC_IL = oku_ugoc_il, BULTEN_TIYATRO_IL = oku_tiyatro_il,
                   BULTEN_GOC_NEDENI_IL = oku_goc_nedeni)
  for (ad in names(uretilen)) {
    df <- uretilen[[ad]]()
    uzun_yaz(df, file.path(HAM, paste0(ad, ".csv")))
    cat(sprintf("%s: %d satır, %d il, dönem %s\n", ad, nrow(df), length(unique(df$REF_AREA)),
                paste(sort(unique(df$TIME_PERIOD)), collapse = ",")))
  }
}
