.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))
source(file.path(dirname(normalizePath(.f, winslash = "/")), "bulten_yardimci.R"))

# Türkiye düzeyli TÜİK bülten tabloları (xls)  ->  veri/ham/BULTEN_TR_<AD>.csv (uzun biçim)
# Dış ticaret (yıllık + aylık, milyar ABD $), hayat tablosu (doğuşta beklenen yaşam süresi),
# kümes hayvancılığı (aylık). Doğrudan çalıştırılınca CSV yazar; source edilince yalnız işlevler.
#
# Kullanım: Rscript betikler/05i_bulten_tr_oku.R

HAM <- file.path(PORTAL, "veri/ham")
AYLAR <- c(Ocak = 1, Şubat = 2, Mart = 3, Nisan = 4, Mayıs = 5, Haziran = 6, Temmuz = 7,
           Ağustos = 8, Eylül = 9, Ekim = 10, Kasım = 11, Aralık = 12)
sayi_tr <- function(x) suppressWarnings(as.numeric(x))
bos_mu <- function(x) is.na(x) || !nzchar(trimws(x))

# --- dış ticaret (genel ticaret sistemi): yıllık ve aylık ------------------
oku_dis_ticaret <- function() {
  dis_ticaret_ayristir(bulten_oku(bulten_dosya("dis_ticaret_2026_09", "yıllara ve aylara göre dış ticaret")))
}

# Ayrıştırma ayrı işlev: veri bloğu "Months" başlık satırından sonra başlar (sabit satır numarası yok),
# tanınmayan bir satır etiketi sessizce atlanmaz, durdurur.
dis_ticaret_ayristir <- function(d) {
  hb <- which(trimws(d[[1]]) == "Months")[1]
  if (is.na(hb)) stop("dış ticaret: 'Months' başlık satırı bulunamadı", call. = FALSE)
  ust <- seq_len(hb)
  if (!grepl("Bin ABD", paste(unlist(d[ust, ]), collapse = " "), fixed = TRUE)) stop("dış ticaret: birim 'Bin ABD $' değil", call. = FALSE)
  baslik_dogrula(d, ust, 3, "İhracat", "dış ticaret")
  baslik_dogrula(d, ust, 6, "İthalat", "dış ticaret")
  baslik_dogrula(d, ust, 9, "denge", "dış ticaret")
  baslik_dogrula(d, ust, 12, "hacmi", "dış ticaret")
  baslik_dogrula(d, ust, 15, "Karşılama", "dış ticaret")
  sut <- c(IHRACAT = 3, ITHALAT = 6, DENGE = 9, HACIM = 12, KARSILAMA = 15)
  carpan <- c(IHRACAT = 1e-6, ITHALAT = 1e-6, DENGE = 1e-6, HACIM = 1e-6, KARSILAMA = 1)  # bin $ -> milyar $

  ay_re <- paste0("^(", paste(names(AYLAR), collapse = "|"), ")\\s*-\\s*[A-Za-z]+\\s*$")
  bolum <- "yillik"        # yillik -> (ay etiketi geldikçe) aylik / kumulatif
  ay <- NA_integer_
  cikti <- list()
  kum_re <- paste0("^(", paste(names(AYLAR), collapse = "|"), ")\\s*-\\s*(", paste(names(AYLAR), collapse = "|"), ")\\s*/")
  for (r in (hb + 1):nrow(d)) {
    et <- trimws(d[r, 1])
    if (!bos_mu(d[r, 1])) {
      if (grepl(ay_re, et)) { bolum <- "aylik"; ay <- unname(AYLAR[sub("\\s*-.*$", "", et)]) }
      else if (grepl("^TÜİK", et)) break
      else if (grepl(kum_re, et)) { bolum <- "kumulatif"; ay <- NA_integer_ }   # "Ocak- Eylül/January- September"
      else stop("dış ticaret: tanınmayan satır etiketi: ", et, call. = FALSE)
    }
    yil <- sub("^\\s*((19|20)[0-9]{2}).*$", "\\1", d[r, 2])
    if (!grepl("^(19|20)[0-9]{2}$", yil) || bolum == "kumulatif") next
    donem <- if (bolum == "yillik") yil else sprintf("%s-%02d", yil, ay)
    sonek <- if (bolum == "yillik") "_YILLIK" else "_AYLIK"
    for (g in names(sut)) {
      v <- sayi_tr(d[r, sut[[g]]]) * carpan[[g]]
      if (is.na(v)) next
      cikti[[length(cikti) + 1]] <- data.frame(REF_AREA = "TR", TIME_PERIOD = donem,
                                               GOSTERGE = paste0(g, sonek), OBS_VALUE = v, stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, cikti)
}

# --- hayat tablosu: doğuşta beklenen yaşam süresi (yaş 0, e(x)) -------------
oku_hayat_tablosu <- function() {
  d <- bulten_oku(bulten_dosya("hayat_tablolari_2025", "tek yaş hayat tablosu"))
  baslik <- which(grepl("^Yıl", trimws(d[[1]])))[1]
  if (is.na(baslik)) stop("hayat tablosu: 'Yıl' başlık satırı bulunamadı", call. = FALSE)
  ust <- baslik + 0:1
  sut <- c(BEKLENEN_YASAM_TOPLAM = 6, BEKLENEN_YASAM_ERKEK = 11, BEKLENEN_YASAM_KADIN = 16)
  baslik_dogrula(d, baslik, 3, "Toplam", "hayat tablosu")
  baslik_dogrula(d, baslik, 8, "Erkek", "hayat tablosu")
  baslik_dogrula(d, baslik, 13, "Kadın", "hayat tablosu")
  for (j in sut) baslik_dogrula(d, baslik + 1, j, "e(x)", "hayat tablosu")
  donem <- NA_character_
  cikti <- list()
  for (r in (baslik + 2):nrow(d)) {
    if (!bos_mu(d[r, 1]) && grepl("^(19|20)[0-9]{2}-(19|20)[0-9]{2}$", trimws(d[r, 1]))) {
      donem <- sub("^.*-", "", trimws(d[r, 1]))   # kayan üç yıllık dönemin son yılı
    }
    if (!is.na(donem) && !is.na(sayi_tr(d[r, 2])) && sayi_tr(d[r, 2]) == 0) {
      for (g in names(sut)) {
        cikti[[length(cikti) + 1]] <- data.frame(REF_AREA = "TR", TIME_PERIOD = donem, GOSTERGE = g,
                                                 OBS_VALUE = sayi_tr(d[r, sut[[g]]]), stringsAsFactors = FALSE)
      }
    }
  }
  do.call(rbind, cikti)
}

# --- kümes hayvancılığı (aylık) -------------------------------------------
oku_kumes <- function() {
  d <- bulten_oku(bulten_dosya("kumes_2026_09", "Kümes hayvancılığı ürünleri"))
  baslik <- which(grepl("^Yıl", trimws(d[[2]])))[1]
  if (is.na(baslik)) stop("kümes: 'Yıl' başlık satırı bulunamadı", call. = FALSE)
  ay_sut <- which(vapply(seq_len(ncol(d)), function(j) {
    ilk <- sub("\\s.*$", "", trimws(d[baslik, j])); !is.na(ilk) && ilk %in% names(AYLAR)
  }, TRUE))
  ay_no <- unname(AYLAR[sub("\\s.*$", "", trimws(d[baslik, ay_sut]))])
  urun_re <- c(TAVUK_YUMURTASI = "^Tavuk yumurtası", KESILEN_TAVUK = "^Kesilen tavuk", TAVUK_ETI = "^Tavuk eti",
               KESILEN_HINDI = "^Kesilen hindi", HINDI_ETI = "^Hindi eti")
  urun <- NA_character_
  cikti <- list()
  for (r in (baslik + 1):nrow(d)) {
    if (!bos_mu(d[r, 1])) {
      es <- vapply(urun_re, function(re) grepl(re, trimws(d[r, 1])), TRUE)
      urun <- if (any(es)) names(urun_re)[es][1] else NA_character_
    }
    yil <- sub("\\.0$", "", trimws(d[r, 2]))
    if (is.na(urun) || !grepl("^(19|20)[0-9]{2}$", yil)) next
    v <- sayi_tr(unlist(d[r, ay_sut]))
    tam <- !is.na(v)
    if (!any(tam)) next
    cikti[[length(cikti) + 1]] <- data.frame(REF_AREA = "TR", TIME_PERIOD = sprintf("%s-%02d", yil, ay_no[tam]),
                                             GOSTERGE = urun, OBS_VALUE = v[tam], stringsAsFactors = FALSE)
  }
  do.call(rbind, cikti)
}

# --- sinema (Türkiye, 2000-2025) -------------------------------------------
oku_sinema_tr <- function() {
  d <- bulten_oku(bulten_dosya("sinema_gosteri_2025", "Sinema, Gösterilen Film ve Seyirci"))
  baslik <- which(grepl("^Yıl", trimws(d[[1]])))[1]
  if (is.na(baslik)) stop("sinema TR: 'Yıl' başlık satırı bulunamadı", call. = FALSE)
  baslik_dogrula(d, baslik - 1, 2, "Sinema salonu", "sinema TR")
  baslik_dogrula(d, baslik, 3, "Koltuk", "sinema TR")
  baslik_dogrula(d, baslik - 1, 5, "Gösterilen film", "sinema TR")
  baslik_dogrula(d, baslik - 1, 9, "Seyirci", "sinema TR")
  for (j in c(5, 9)) baslik_dogrula(d, baslik, j, "Toplam", "sinema TR")
  for (j in c(6, 10)) baslik_dogrula(d, baslik, j, "Yerli", "sinema TR")
  for (j in c(7, 11)) baslik_dogrula(d, baslik, j, "Yabancı", "sinema TR")
  sut <- c(SALON = 2, KOLTUK = 3, FILM = 5, FILM_YERLI = 6, FILM_YABANCI = 7,
           SEYIRCI = 9, SEYIRCI_YERLI = 10, SEYIRCI_YABANCI = 11)
  satir <- which(seq_len(nrow(d)) > baslik & grepl("^(19|20)[0-9]{2}$", as.character(round(sayi_tr(d[[1]])))))
  yil <- as.character(round(sayi_tr(d[satir, 1])))
  do.call(rbind, lapply(names(sut), function(g) {
    data.frame(REF_AREA = "TR", TIME_PERIOD = yil, GOSTERGE = g, OBS_VALUE = sayi_tr(d[satir, sut[[g]]]),
               stringsAsFactors = FALSE)
  }))
}

# --- tiyatro (Türkiye, sezon yılı; yıl = sezonun bitiş yılı) ---------------
oku_tiyatro_tr <- function() {
  d <- bulten_oku(bulten_dosya("sinema_gosteri_2025", "Sezon Yılına Göre Tiyatro"))
  baslik <- which(grepl("^Sezon yılı(\\s|$)", trimws(d[[1]])))[1]   # "Sezon yılına göre..." başlığını almaz
  if (is.na(baslik)) stop("tiyatro TR: 'Sezon yılı' başlık satırı bulunamadı", call. = FALSE)
  baslik_dogrula(d, baslik - 1, 2, "Tiyatro salonu", "tiyatro TR")
  baslik_dogrula(d, baslik, 3, "Koltuk", "tiyatro TR")
  baslik_dogrula(d, baslik, 5, "Oynanan eser", "tiyatro TR")
  baslik_dogrula(d, baslik, 9, "Gösteri", "tiyatro TR")
  sut <- c(SALON = 2, KOLTUK = 3, ESER = 5, GOSTERI = 9)
  satir <- which(seq_len(nrow(d)) > baslik & grepl("^[0-9]{4}/'[0-9]{2}$", trimws(d[[1]])))
  yil <- as.character(as.integer(substr(trimws(d[satir, 1]), 1, 4)) + 1L)
  do.call(rbind, lapply(names(sut), function(g) {
    data.frame(REF_AREA = "TR", TIME_PERIOD = yil, GOSTERGE = g, OBS_VALUE = sayi_tr(d[satir, sut[[g]]]),
               stringsAsFactors = FALSE)
  }))
}

# --- Gelir ve yaşam koşulları: sürekli yoksulluk, maddi yoksunluk (Türkiye, 2006-) ---
oku_yoksulluk_tr <- function() {
  d <- bulten_oku(bulten_dosya("yoksulluk_yasam_kosullari_2025", "Sürekli yoksulluk, maddi yoksunluk"))
  yil_satiri <- which(apply(d, 1, function(r) sum(grepl("^(19|20)[0-9]{2}$", trimws(r)), na.rm = TRUE) >= 5))[1]
  if (is.na(yil_satiri)) stop("yoksulluk TR: yıl satırı bulunamadı", call. = FALSE)
  yil_sut <- which(grepl("^(19|20)[0-9]{2}$", trimws(unlist(d[yil_satiri, ]))))
  etiket <- c(SUREKLI_YOKSULLUK = "^Sürekli yoksulluk oranı", MADDI_YOKSUNLUK = "^Maddi yoksunluk oranı",
              MADDI_SOSYAL_YOKSUNLUK = "^Maddi ve sosyal yoksunluk oranı")
  cikti <- list()
  for (g in names(etiket)) {
    r <- which(seq_len(nrow(d)) > yil_satiri & grepl(etiket[[g]], trimws(d[[1]])))[1]
    if (is.na(r)) stop("yoksulluk TR: satır bulunamadı: ", g, call. = FALSE)
    v <- sayi_tr(unlist(d[r, yil_sut]))      # "-(1)" dipnotlu hücreler NA olur
    ok <- !is.na(v)
    cikti[[length(cikti) + 1]] <- data.frame(REF_AREA = "TR", TIME_PERIOD = trimws(unlist(d[yil_satiri, yil_sut]))[ok],
                                             GOSTERGE = g, OBS_VALUE = v[ok], stringsAsFactors = FALSE)
  }
  do.call(rbind, cikti)
}

# --- Hayvan sayıları (Aralık ve Haziran dönemleri) ------------------------
oku_hayvan_sayilari <- function() {
  d <- bulten_oku(bulten_dosya("hayvancilik_2026_06", "hayvan sayıları"))
  ay_en <- c(Ocak = 1, Şubat = 2, Mart = 3, Nisan = 4, Mayıs = 5, Haziran = 6, Temmuz = 7,
             Ağustos = 8, Eylül = 9, Ekim = 10, Kasım = 11, Aralık = 12)
  ust <- which(grepl("(19|20)[0-9]{2}", d[[3]]) & !is.na(d[[3]]))[1]
  if (is.na(ust)) stop("hayvan: dönem satırı bulunamadı", call. = FALSE)
  donem <- function(j) {
    m <- regmatches(d[ust, j], regexec("^(\\S+)\\s+((19|20)[0-9]{2})", trimws(d[ust, j])))[[1]]
    if (length(m) < 3 || is.na(ay_en[m[2]])) stop("hayvan: dönem çözülemedi: ", d[ust, j], call. = FALSE)
    sprintf("%s-%02d", m[3], ay_en[[m[2]]])
  }
  sut <- c(3, 7)
  for (j in sut) baslik_dogrula(d, ust + 1, j, "Sayı", "hayvan sayıları")
  etiket <- c(TOPLAM = "^Toplam", BUYUKBAS = "^Büyükbaş", SIGIR = "^Sığır", MANDA = "^Manda",
              KUCUKBAS = "^Küçükbaş", KOYUN = "^Koyun", KECI = "^Keçi")
  cikti <- list()
  for (g in names(etiket)) {
    r <- which(seq_len(nrow(d)) > ust & grepl(etiket[[g]], trimws(d[[1]])))[1]
    if (is.na(r)) stop("hayvan: satır bulunamadı: ", g, call. = FALSE)
    for (j in sut) cikti[[length(cikti) + 1]] <- data.frame(REF_AREA = "TR", TIME_PERIOD = donem(j), GOSTERGE = g,
                                                           OBS_VALUE = sayi_tr(d[r, j]), stringsAsFactors = FALSE)
  }
  do.call(rbind, cikti)
}

# --- Bitkisel üretim, ikinci tahmin (ton): seçilmiş ürünler ----------------
oku_bitkisel <- function() {
  secim <- list(
    list("Tahıllar", c(BUGDAY = "^Buğday -", ARPA = "^Arpa -", MISIR = "^Mısır [(]dane[)]", AYCICEGI = "^Ayçiçeği -",
                       SEKER_PANCARI = "^Şeker pancarı -", PATATES = "^Patates -", PAMUK = "^Pamuk [(]kütlü[)]")),
    list("Sebze", c(DOMATES = "^Domates -")),
    list("Meyve", c(UZUM = "^Üzüm -", ZEYTIN = "^Zeytin -", FINDIK = "^Fındık -", ELMA = "^Elma -")))
  cikti <- list()
  for (s in secim) {
    d <- bulten_oku(bulten_dosya("bitkisel_uretim_2026", s[[1]]))
    ust <- which(grepl("^(19|20)[0-9]{2}$", trimws(d[[2]])) & grepl("^(19|20)[0-9]{2}$", trimws(d[[5]])))[1]
    if (is.na(ust)) stop("bitkisel: yıl satırı bulunamadı: ", s[[1]], call. = FALSE)
    if (!grepl("Ton", paste(unlist(d[1:ust, ]), collapse = " "), fixed = TRUE)) stop("bitkisel: birim ton değil: ", s[[1]], call. = FALSE)
    yillar <- trimws(c(d[ust, 2], d[ust, 5]))
    for (g in names(s[[2]])) {
      r <- which(seq_len(nrow(d)) > ust & grepl(s[[2]][[g]], trimws(d[[1]])))
      if (length(r) != 1) stop("bitkisel: ", g, " için ", length(r), " satır", call. = FALSE)
      cikti[[length(cikti) + 1]] <- data.frame(REF_AREA = "TR", TIME_PERIOD = yillar, GOSTERGE = g,
                                               OBS_VALUE = sayi_tr(c(d[r, 2], d[r, 5])), stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, cikti)
}

# --- Yapı izin (Türkiye, çeyreklik): ruhsat ve kullanma izni; bina, daire, yüzölçüm ---
oku_yapi_izin_tr <- function() {
  d <- bulten_oku(bulten_dosya("yapi_izin_2026_q3", "Yapı izin istatistikleri ve değişim oranları"))
  baslik <- which(grepl("^Yıl", trimws(d[[1]])))[1]
  if (is.na(baslik)) stop("yapı izin TR: 'Yıl' başlık satırı bulunamadı", call. = FALSE)
  ust <- baslik - 1
  baslik_dogrula(d, ust, 3, "Yapı ruhsatı", "yapı izin TR")
  baslik_dogrula(d, baslik, 3, "Bina sayısı", "yapı izin TR")
  baslik_dogrula(d, baslik, 6, "Daire sayısı", "yapı izin TR")
  baslik_dogrula(d, baslik, 9, "Yüzölçüm", "yapı izin TR")
  kul <- which(grepl("kullanma izin", unlist(d[ust, ]), fixed = TRUE))[1]
  if (is.na(kul)) stop("yapı izin TR: kullanma izin bloğu bulunamadı", call. = FALSE)
  baslik_dogrula(d, baslik, kul, "Bina sayısı", "yapı izin TR")
  baslik_dogrula(d, baslik, kul + 3, "Daire sayısı", "yapı izin TR")
  baslik_dogrula(d, baslik, kul + 6, "Yüzölçüm", "yapı izin TR")
  sut <- c(RUHSAT_BINA = 3, RUHSAT_DAIRE = 6, RUHSAT_YUZOLCUM = 9,
           KULLANMA_BINA = kul, KULLANMA_DAIRE = kul + 3, KULLANMA_YUZOLCUM = kul + 6)
  ceyrek <- c(I = 1, II = 2, III = 3, IV = 4)
  yil <- NA_character_; cikti <- list()
  for (r in (baslik + 1):nrow(d)) {
    if (!bos_mu(d[r, 1]) && grepl("^(19|20)[0-9]{2}$", trimws(d[r, 1]))) yil <- trimws(d[r, 1])
    q <- trimws(d[r, 2])
    if (is.na(yil) || is.na(q) || is.na(ceyrek[q])) next
    for (g in names(sut)) {
      v <- sayi_tr(d[r, sut[[g]]])
      if (!is.na(v)) cikti[[length(cikti) + 1]] <- data.frame(REF_AREA = "TR", TIME_PERIOD = sprintf("%s-Q%d", yil, ceyrek[[q]]),
                                                             GOSTERGE = g, OBS_VALUE = v, stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, cikti)
}

# --- çalıştır --------------------------------------------------------------
if (sys.nframe() == 0) {
  uretilen <- list(BULTEN_TR_DIS_TICARET = oku_dis_ticaret, BULTEN_TR_HAYAT = oku_hayat_tablosu,
                   BULTEN_TR_KUMES = oku_kumes, BULTEN_TR_SINEMA = oku_sinema_tr,
                   BULTEN_TR_TIYATRO = oku_tiyatro_tr,
                   BULTEN_TR_YOKSULLUK = oku_yoksulluk_tr, BULTEN_TR_HAYVAN = oku_hayvan_sayilari,
                   BULTEN_TR_BITKISEL = oku_bitkisel, BULTEN_TR_YAPI_IZIN = oku_yapi_izin_tr)
  for (ad in names(uretilen)) {
    df <- uretilen[[ad]]()
    uzun_yaz(df, file.path(HAM, paste0(ad, ".csv")))
    cat(sprintf("%s: %d satır, %d gösterge, dönem %s..%s\n", ad, nrow(df), length(unique(df$GOSTERGE)),
                min(df$TIME_PERIOD), max(df$TIME_PERIOD)))
  }
}
