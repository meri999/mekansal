# Kültür çapraz tabloları (sinema_gosteri_2025): il düzeyli tiyatro, opera/bale/orkestra, Türkiye serileri
# Sezon dönemi bitiş yılına bağlanır: 2024/'25 -> "2025".

kultur_sezon_sz <- function(ilk = 1999, son = 2024) {
  y <- ilk:son
  x <- as.list(rep("", length(y)))
  for (i in seq_along(y)) x[[i]] <- sprintf("^%d/'%02d$", y[i], (y[i] + 1) %% 100)
  names(x) <- as.character(y + 1)
  do.call(sz, x)
}
KULTUR_SEZON <- kultur_sezon_sz()
KULTUR_TIYATRO_SZ <- sz(DEVLET = c("^Devlet", "Devlet tiyatroları"), OZEL = c("^Özel", "Özel tiyatrolar"),
                        BELEDIYE = c("^Belediye", "Belediye tiyatroları"), UNIVERSITE = c("^Üniversite", "Üniversite tiyatroları"),
                        SIVIL_TOPLUM = c("^Sivil", "Sivil toplum tiyatroları"))
KULTUR_YASGRUBU_SZ <- sz(`_T` = "^Toplam", YETISKIN = c("^Yetişkin", "Yetişkin"), COCUK = c("^Çocuk", "Çocuk"))
KULTUR_TELIF_SZ <- sz(`_T` = "^Toplam", TELIF = c("^Telif", "Telif"), CEVIRI = c("^Çeviri", "Çeviri"))
KULTUR_YERLI_SZ <- sz(`_T` = "^Toplam", YERLI = c("^Yerli", "Yerli"), YABANCI = c("^Yabancı", "Yabancı"))
KULTUR_IL_TIYATRO_SUTUN <- c(5:7, 9:11, 13:15, 17:19, 21:23)

# İl x tiyatro grubu türü x (yetişkin/çocuk ya da telif/çeviri), 2024/25 sezonu
# Eser sayısında Türkiye satırı tekil eser sayısıdır (dipnot 1: Devlet Tiyatroları eserleri birden çok ilde oynar);
# il toplamı Türkiye'ye eşit olmaz. Devlet'in Türkiye hücreleri (eser tablolarında) çıkarılır.
kultur_tr_devlet_na <- function(s, yalniz_devlet = TRUE) {
  v <- s$veri
  i <- v$REF_AREA == "TR"
  if (yalniz_devlet) i <- i & v$TIYATRO_TURU == "DEVLET"
  v$OBS_VALUE[i] <- NA_real_
  s$veri <- v
  s
}

kultur_il_tiyatro <- function(parca, eksen, eksen_ad, sz2, ad, eser = FALSE) {
  function() {
    d <- kir_oku("sinema_gosteri_2025", parca)
    sut <- list(TIYATRO_TURU = list(tur = "sozluk", sozluk = KULTUR_TIYATRO_SZ, duzey = 1))
    sut[[eksen]] <- list(tur = "sozluk", sozluk = sz2, duzey = 2)
    s <- capraz_oku(d, bas = c(4, 5), veri = c(6, NA), ad = ad, sutun = sut,
                    satir = list(REF_AREA = list(sutun = 1, tur = "il")),
                    veri_sutun = KULTUR_IL_TIYATRO_SUTUN, sabit = list(TIME_PERIOD = "2025"))
    if (eser) s <- kultur_tr_devlet_na(s)
    s
  }
}

kultur_il_tablo <- function(kod, baslik, parca, eksen, eksen_ad, sz2, birim, aciklama, turkiye = "var", bagl = "BULTEN_TIYATRO_IL", eser = FALSE) {
  list(ad = toupper(sub("^bulten_", "", kod)), kod = kod, konu = "Yaşam koşulları", baslik = baslik,
       oku = kultur_il_tiyatro(parca, eksen, eksen_ad, sz2, kod, eser),
       boyut_ad = setNames(c("Tiyatro grubu türü", eksen_ad), c("TIYATRO_TURU", eksen)),
       turkiye = turkiye, olcu = "sayi", birim = birim, son_yil_sayisi = 1, aciklama = aciklama, baglanti_df = bagl)
}

# Opera ve bale salonu bulunan iller: il blokları x sezon satırları; ölçü başına bir tablo
kultur_opera_il <- function(kod, baslik, birim, sutun, ad_kisa) {
  list(ad = toupper(sub("^bulten_", "", kod)), kod = kod, konu = "Yaşam koşulları", baslik = baslik,
       oku = function() {
         d <- kir_oku("sinema_gosteri_2025", "Opera ve bale salonu bulunan")
         capraz_oku(d, bas = 4, veri = c(5, NA), ad = kod,
                    sutun = list(YERLILIK = list(tur = "sozluk", sozluk = KULTUR_YERLI_SZ, duzey = 1)),
                    satir = list(TIME_PERIOD = list(sutun = 1, tur = "sozluk", sozluk = KULTUR_SEZON, tam = TRUE)),
                    blok = list(REF_AREA = list(sutun = 1, tur = "il", desen = ".")),
                    veri_sutun = sutun)
       },
       boyut_ad = c(YERLILIK = "Eser kaynağı"), turkiye = "var", olcu = "sayi", birim = birim, son_yil_sayisi = 2,
       aciklama = paste0("Opera ve bale salonu bulunan illerde ", ad_kisa, "; yerli/yabancı eser (2023/24 ve 2024/25 sezonları, Kültür ve Turizm Bakanlığı kaynaklı)"),
       baglanti_df = "")
}

# Türkiye sezon/yıl serileri
kultur_tr_seri <- function(kod, baslik, parca, bas, veri0, sutun, sz2, zaman, birim, aciklama, bagl, adet = 11, tur_ad = "Eser kaynağı", eksen = "YERLILIK") {
  list(ad = toupper(sub("^bulten_", "", kod)), kod = kod, konu = "Yaşam koşulları", baslik = baslik,
       oku = function() {
         d <- kir_oku("sinema_gosteri_2025", parca)
         sut <- list(); sut[[eksen]] <- list(tur = "sozluk", sozluk = sz2, duzey = length(bas))
         capraz_oku(d, bas = bas, veri = c(veri0, NA), ad = kod, sutun = sut, satir = list(TIME_PERIOD = zaman),
                    veri_sutun = sutun, sabit = list(REF_AREA = "TR"))
       },
       boyut_ad = setNames(tur_ad, eksen), turkiye = "var", olcu = "sayi", birim = birim, son_yil_sayisi = adet,
       aciklama = aciklama, baglanti_df = bagl)
}

# İller x grup türü x sezon (orkestra/koro/topluluk); sütun = ölçü
kultur_orkestra_il <- function(kod, baslik, birim, sutun, ad_kisa) {
  list(ad = toupper(sub("^bulten_", "", kod)), kod = kod, konu = "Yaşam koşulları", baslik = baslik,
       oku = function() {
         d <- kir_oku("sinema_gosteri_2025", "İllere göre orkestra")
         # Genel "Toplam" grup satırı yalnız Türkiye düzeyinde verilir (il kırılımı yok); il toplamı doğrulaması
         # için atılır, grup toplamı grup türlerinin toplamıdır.
         s <- capraz_oku(d, bas = 3, veri = c(4, NA), ad = kod,
                    satir = list(REF_AREA = list(sutun = 2, tur = "il", doldur = TRUE, bos = "Toplam"),
                                 TIME_PERIOD = list(sutun = 3, tur = "sozluk", sozluk = KULTUR_SEZON, tam = TRUE)),
                    blok = list(GRUP_TURU = list(sutun = 1, tur = "oto", ilk = "Toplam-Total", desen = "^(Orkestra|Koro|Topluluk|Opera)")),
                    veri_sutun = sutun)
         s$veri <- s$veri[s$veri$GRUP_TURU != "_T", ]
         s$etiket <- s$etiket[!(s$etiket$boyut == "GRUP_TURU" & s$etiket$kod == "_T"), ]
         s
       },
       boyut_ad = c(GRUP_TURU = "Grup türü"), turkiye = "var", olcu = "sayi", birim = birim, son_yil_sayisi = 2,
       aciklama = paste0("Orkestra, koro ve topluluklarda ", ad_kisa, "; grup türü ve ile göre (2023/24 ve 2024/25 sezonları)"),
       baglanti_df = "")
}

KIR_TABLOLAR <- c(KIR_TABLOLAR, list(
  # --- il düzeyli tiyatro: yetişkin/çocuk ---
  kultur_il_tablo("bulten_tiyatro_il_seyirci_yas", "İllerin tiyatro seyircisi: tiyatro grubu türü ve yetişkin/çocuk",
                  "yetişkin çocuk tiyatro seyirci", "YASGRUBU_TURU", "Yetişkin/çocuk", KULTUR_YASGRUBU_SZ, "kişi",
                  "İl ve tiyatro grubu türüne göre yetişkin ve çocuk tiyatro seyirci sayısı (2024/25 sezonu, TÜİK Sinema ve Gösteri Sanatları İstatistikleri)"),
  kultur_il_tablo("bulten_tiyatro_il_eser_yas", "İllerde oynanan tiyatro eseri: tiyatro grubu türü ve yetişkin/çocuk",
                  "yetişkin ve çocuk eser", "YASGRUBU_TURU", "Yetişkin/çocuk", KULTUR_YASGRUBU_SZ, "eser",
                  "İl ve tiyatro grubu türüne göre oynanan yetişkin ve çocuk eser sayısı (2024/25 sezonu; Devlet Tiyatroları için Türkiye değeri eşsiz eser sayısı olduğundan gösterilmez)", eser = TRUE),
  kultur_il_tablo("bulten_tiyatro_il_gosteri_yas", "İllerde oynanan tiyatro gösterisi: tiyatro grubu türü ve yetişkin/çocuk",
                  "yetişkin ve çocuk gösteri", "YASGRUBU_TURU", "Yetişkin/çocuk", KULTUR_YASGRUBU_SZ, "gösteri",
                  "İl ve tiyatro grubu türüne göre oynanan yetişkin ve çocuk gösteri sayısı (2024/25 sezonu)"),
  # --- il düzeyli tiyatro: telif/çeviri ---
  kultur_il_tablo("bulten_tiyatro_il_seyirci_telif", "İllerin tiyatro seyircisi: tiyatro grubu türü ve telif/çeviri",
                  "telif çeviri tiyatro seyirci", "TELIF_CEVIRI", "Telif/çeviri", KULTUR_TELIF_SZ, "kişi",
                  "İl ve tiyatro grubu türüne göre telif ve çeviri eserlerin seyirci sayısı (2024/25 sezonu)"),
  kultur_il_tablo("bulten_tiyatro_il_eser_telif", "İllerde oynanan tiyatro eseri: tiyatro grubu türü ve telif/çeviri",
                  "telif çeviri eser", "TELIF_CEVIRI", "Telif/çeviri", KULTUR_TELIF_SZ, "eser",
                  "İl ve tiyatro grubu türüne göre oynanan telif ve çeviri eser sayısı (2024/25 sezonu; Devlet Tiyatroları için Türkiye değeri eşsiz eser sayısı olduğundan gösterilmez)", eser = TRUE),
  kultur_il_tablo("bulten_tiyatro_il_gosteri_telif", "İllerde oynanan tiyatro gösterisi: tiyatro grubu türü ve telif/çeviri",
                  "telif çeviri gösteri", "TELIF_CEVIRI", "Telif/çeviri", KULTUR_TELIF_SZ, "gösteri",
                  "İl ve tiyatro grubu türüne göre oynanan telif ve çeviri gösteri sayısı (2024/25 sezonu)"),

  # --- il x telif/çeviri (tüm gruplar) ---
  list(ad = "TIYATRO_IL_GOSTERI_TELIF_TUM", kod = "bulten_tiyatro_il_gosteri_telif_tum", konu = "Yaşam koşulları",
       baslik = "İllerde oynanan tiyatro gösterisi: telif/çeviri",
       oku = function() {
         d <- kir_oku("sinema_gosteri_2025", "İllere göre tiyatrolarda oynanan eser")
         capraz_oku(d, bas = c(4, 5), veri = c(6, NA), ad = "tiyatro il gösteri", sutun = list(TELIF_CEVIRI = list(tur = "sozluk", sozluk = KULTUR_TELIF_SZ, duzey = 2)),
                    satir = list(REF_AREA = list(sutun = 1, tur = "il")), veri_sutun = 9:11, sabit = list(TIME_PERIOD = "2025"))
       },
       boyut_ad = c(TELIF_CEVIRI = "Telif/çeviri"), turkiye = "var", olcu = "sayi", birim = "gösteri", son_yil_sayisi = 1,
       aciklama = "İllerde oynanan toplam tiyatro gösterisi sayısı; telif/çeviri (2024/25 sezonu, tüm tiyatro grupları)", baglanti_df = "BULTEN_TIYATRO_IL"),
  list(ad = "TIYATRO_IL_SEYIRCI_TELIF_TUM", kod = "bulten_tiyatro_il_seyirci_telif_tum", konu = "Yaşam koşulları",
       baslik = "İllerin tiyatro seyircisi: telif/çeviri",
       oku = function() {
         d <- kir_oku("sinema_gosteri_2025", "İllere göre tiyatrolarda oynanan eser")
         capraz_oku(d, bas = c(4, 5), veri = c(6, NA), ad = "tiyatro il seyirci", sutun = list(TELIF_CEVIRI = list(tur = "sozluk", sozluk = KULTUR_TELIF_SZ, duzey = 2)),
                    satir = list(REF_AREA = list(sutun = 1, tur = "il")), veri_sutun = 13:15, sabit = list(TIME_PERIOD = "2025"))
       },
       boyut_ad = c(TELIF_CEVIRI = "Telif/çeviri"), turkiye = "var", olcu = "sayi", birim = "kişi", son_yil_sayisi = 1,
       aciklama = "İllerde tiyatro seyirci sayısı; telif/çeviri (2024/25 sezonu, tüm tiyatro grupları)", baglanti_df = "BULTEN_TIYATRO_IL"),
  list(ad = "TIYATRO_IL_ESER_TELIF_TUM", kod = "bulten_tiyatro_il_eser_telif_tum", konu = "Yaşam koşulları",
       baslik = "İllerde oynanan tiyatro eseri: telif/çeviri",
       oku = function() {
         d <- kir_oku("sinema_gosteri_2025", "İllere göre tiyatrolarda oynanan eser")
         capraz_oku(d, bas = c(4, 5), veri = c(6, NA), ad = "tiyatro il eser", sutun = list(TELIF_CEVIRI = list(tur = "sozluk", sozluk = KULTUR_TELIF_SZ, duzey = 2)),
                    satir = list(REF_AREA = list(sutun = 1, tur = "il")), veri_sutun = 5:7, sabit = list(TIME_PERIOD = "2025")) |>
           kultur_tr_devlet_na(yalniz_devlet = FALSE)
       },
       boyut_ad = c(TELIF_CEVIRI = "Telif/çeviri"), turkiye = "var", olcu = "sayi", birim = "eser", son_yil_sayisi = 1,
       aciklama = "İllerde oynanan tiyatro eseri sayısı; telif/çeviri (2024/25 sezonu, tüm tiyatro grupları; Türkiye değeri eşsiz eser sayısı olduğundan gösterilmez)", baglanti_df = "BULTEN_TIYATRO_IL"),

  # --- opera ve bale salonu bulunan iller ---
  kultur_opera_il("bulten_opera_bale_il_eser", "Opera ve bale salonu bulunan illerde oynanan eser: yerli/yabancı", "eser", 4:6, "oynanan eser sayısı"),
  kultur_opera_il("bulten_opera_bale_il_gosteri", "Opera ve bale salonu bulunan illerde gösteri: yerli/yabancı", "gösteri", 8:10, "gösteri sayısı"),
  kultur_opera_il("bulten_opera_bale_il_seyirci", "Opera ve bale salonu bulunan illerde seyirci: yerli/yabancı", "kişi", 12:14, "seyirci sayısı"),

  # --- Türkiye serileri ---
  kultur_tr_seri("bulten_opera_bale_tr_eser", "Opera ve bale: oynanan eser (Türkiye, sezon serisi)", "Opera ve Bale Salonu, Koltuk",
                 bas = c(3, 4), veri0 = 5, sutun = 5:7, sz2 = KULTUR_YERLI_SZ, birim = "eser",
                 zaman = list(sutun = 1, tur = "sozluk", sozluk = KULTUR_SEZON, tam = TRUE),
                 aciklama = "Türkiye'de opera ve bale salonlarında oynanan eser sayısı; yerli/yabancı, 1999/00-2024/25 sezonları (son 11 sezon)", bagl = ""),
  kultur_tr_seri("bulten_opera_bale_tr_seyirci", "Opera ve bale: seyirci (Türkiye, sezon serisi)", "Opera ve Bale Salonu, Koltuk",
                 bas = c(3, 4), veri0 = 5, sutun = 9:11, sz2 = KULTUR_YERLI_SZ, birim = "kişi",
                 zaman = list(sutun = 1, tur = "sozluk", sozluk = KULTUR_SEZON, tam = TRUE),
                 aciklama = "Türkiye'de opera ve bale seyirci sayısı; yerli/yabancı eser, 1999/00-2024/25 sezonları (son 11 sezon)", bagl = ""),
  kultur_tr_seri("bulten_sinema_tr_film", "Sinema: gösterilen film (Türkiye, yıllık)", "Sinema, Gösterilen Film",
                 bas = c(3, 4), veri0 = 5, sutun = 5:7,
                 sz2 = sz(`_T` = "^Toplam", YERLI = c("^Yerli", "Yerli film"), YABANCI = c("^Yabancı", "Yabancı film")),
                 zaman = list(sutun = 1, tur = "yil"), birim = "film", tur_ad = "Film kaynağı",
                 aciklama = "Türkiye'de gösterilen film sayısı; yerli/yabancı, 2000-2022 (2023 sonrası bültende film sayısı yok)", bagl = "BULTEN_TR_SINEMA"),
  kultur_tr_seri("bulten_sinema_tr_seyirci", "Sinema: seyirci (Türkiye, yıllık)", "Sinema, Gösterilen Film",
                 bas = c(3, 4), veri0 = 5, sutun = 9:11,
                 sz2 = sz(`_T` = "^Toplam", YERLI = c("^Yerli", "Yerli film"), YABANCI = c("^Yabancı", "Yabancı film")),
                 zaman = list(sutun = 1, tur = "yil"), birim = "kişi", tur_ad = "Film kaynağı",
                 aciklama = "Türkiye'de sinema seyirci sayısı; yerli/yabancı film, 2000-2025", bagl = "BULTEN_TR_SINEMA"),
  kultur_tr_seri("bulten_tiyatro_tr_eser", "Tiyatro: oynanan eser (Türkiye, sezon serisi)", "Sezon Yılına Göre Tiyatro",
                 bas = c(4, 5), veri0 = 6, sutun = 5:7, sz2 = KULTUR_TELIF_SZ, birim = "eser", tur_ad = "Telif/çeviri", eksen = "TELIF_CEVIRI",
                 zaman = list(sutun = 1, tur = "sozluk", sozluk = KULTUR_SEZON, tam = TRUE),
                 aciklama = "Türkiye'de tiyatrolarda oynanan eser sayısı; telif/çeviri, 1999/00-2024/25 sezonları (son 11 sezon)", bagl = "BULTEN_TR_TIYATRO"),
  kultur_tr_seri("bulten_tiyatro_tr_gosteri", "Tiyatro: gösteri (Türkiye, sezon serisi)", "Sezon Yılına Göre Tiyatro",
                 bas = c(4, 5), veri0 = 6, sutun = 9:11, sz2 = KULTUR_TELIF_SZ, birim = "gösteri", tur_ad = "Telif/çeviri", eksen = "TELIF_CEVIRI",
                 zaman = list(sutun = 1, tur = "sozluk", sozluk = KULTUR_SEZON, tam = TRUE),
                 aciklama = "Türkiye'de tiyatrolarda oynanan gösteri sayısı; telif/çeviri, 1999/00-2024/25 sezonları (son 11 sezon)", bagl = "BULTEN_TR_TIYATRO"),
  kultur_tr_seri("bulten_tiyatro_tr_seyirci", "Tiyatro: seyirci (Türkiye, sezon serisi)", "Sezon Yılına Göre Tiyatro",
                 bas = c(4, 5), veri0 = 6, sutun = 13:15, sz2 = KULTUR_TELIF_SZ, birim = "kişi", tur_ad = "Telif/çeviri", eksen = "TELIF_CEVIRI",
                 zaman = list(sutun = 1, tur = "sozluk", sozluk = KULTUR_SEZON, tam = TRUE),
                 aciklama = "Türkiye'de tiyatro seyirci sayısı; telif/çeviri eser, 1999/00-2024/25 sezonları (son 11 sezon)", bagl = "BULTEN_TR_TIYATRO"),

  # --- opera, bale, orkestra, koro, topluluk: sanatçı sayısı ---
  list(ad = "SANATCI_KADRO", kod = "bulten_sanatci_kadro", konu = "Yaşam koşulları",
       baslik = "Opera, bale, orkestra, koro ve topluluklarda sanatçı sayısı: kadro durumu ve cinsiyet",
       oku = function() {
         d <- kir_oku("sinema_gosteri_2025", "kadro durumuna göre sanatçı")
         capraz_oku(d, bas = c(3, 4), veri = c(5, NA), ad = "sanatçı kadro",
                    sutun = list(TIME_PERIOD = list(tur = "sozluk", sozluk = KULTUR_SEZON, duzey = 1, tam = TRUE),
                                 CINSIYET = list(tur = "sozluk", sozluk = CINSIYET_SZ, duzey = 2)),
                    satir = list(KADRO = list(sutun = 1, tur = "oto")),
                    blok = list(GRUP = list(sutun = 1, tur = "oto", desen = "^(Opera ve bale|Orkestra|Koro|Topluluk)")),
                    veri_sutun = c(2:4, 6:8), sabit = list(REF_AREA = "TR"))
       },
       boyut_ad = c(GRUP = "Kurum türü", KADRO = "Kadro durumu", CINSIYET = "Cinsiyet"), turkiye = "var", olcu = "sayi", birim = "kişi",
       son_yil_sayisi = 2, aciklama = "Opera ve bale, orkestra, koro ve topluluklarda sanatçı sayısı; kadro durumu ve cinsiyete göre (2023/24 ve 2024/25 sezonları)",
       baglanti_df = ""),

  # --- orkestra, koro ve topluluklar: il x sezon, ölçü başına tablo ---
  kultur_orkestra_il("bulten_orkestra_il_grup", "Orkestra, koro ve topluluk sayısı: grup türü ve il", "grup", 4, "grup sayısı"),
  kultur_orkestra_il("bulten_orkestra_il_koltuk", "Orkestra, koro ve topluluk salonlarının koltuk sayısı: grup türü ve il", "koltuk", 5, "koltuk sayısı"),
  kultur_orkestra_il("bulten_orkestra_il_gosteri", "Orkestra, koro ve topluluk gösterisi: grup türü ve il", "gösteri", 6, "gösteri sayısı"),
  kultur_orkestra_il("bulten_orkestra_il_seyirci", "Orkestra, koro ve topluluk seyircisi: grup türü ve il", "kişi", 7, "seyirci sayısı")
))
