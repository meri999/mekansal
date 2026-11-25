# Hayat tabloları çapraz tabloları (hayat_tablolari_2025)
# Yardımcı işlevler hy_ önekli. Ülke karşılaştırmalarında REF_AREA = "TR" sabit, ülke eksen (ULKE).

HY <- "hayat_tablolari_2025"
HY_CINSIYET <- sz(`_T` = c("^Toplam", "Toplam"), E = c("^Erkek", "Erkek"), K = c("^Kadın", "Kadın"))

# Tek yaş hayat tablosunda gösterilen yaşlar (tüm tek yaşlar çok geniş olurdu)
HY_YASLAR <- c(0, 1, seq(5, 95, by = 5))

KIR_TABLOLAR <- c(KIR_TABLOLAR, list(

  list(ad = "HAYAT_AVRUPA_BEKLENEN", kod = "bulten_hayat_avrupa_beklenen", konu = "Nüfus ve demografi",
       baslik = "Avrupa ülkelerinde doğuşta beklenen yaşam süresi: ülke ve cinsiyet",
       oku = function() {
         capraz_oku(kir_oku(HY, "ülkelerinde cinsiyete göre doğuşta beklenen yaşam"), bas = 4, veri = c(5, NA), ad = "Avrupa beklenen yaşam",
                    sutun = list(CINSIYET = list(tur = "sozluk", duzey = 1, sozluk = HY_CINSIYET)),
                    satir = list(ULKE = list(sutun = 1, tur = "oto")),
                    sabit = list(REF_AREA = "TR", TIME_PERIOD = "2024"))
       },
       boyut_ad = c(ULKE = "Ülke", CINSIYET = "Cinsiyet"), turkiye = "var", olcu = "oran", birim = "yıl", son_yil_sayisi = 1,
       aciklama = "Avrupa ülkeleri ve Türkiye için doğuşta beklenen yaşam süresi (2024; Türkiye: Hayat Tabloları 2023-2025). Ülke karşılaştırması: Türkiye bölgesi altında ülke kırılımı",
       baglanti_df = "BULTEN_TR_HAYAT"),

  list(ad = "HAYAT_AVRUPA_SAGLIKLI", kod = "bulten_hayat_avrupa_saglikli", konu = "Nüfus ve demografi",
       baslik = "Avrupa ülkelerinde doğuşta sağlıklı yaşam süresi: ülke ve cinsiyet",
       oku = function() {
         capraz_oku(kir_oku(HY, "ülkelerinde cinsiyete göre doğuşta sağlıklı"), bas = 4:5, veri = c(6, NA), ad = "Avrupa sağlıklı yaşam",
                    sutun = list(TIME_PERIOD = list(tur = "yil", duzey = 1),
                                 CINSIYET = list(tur = "sozluk", duzey = 2, sozluk = HY_CINSIYET)),
                    satir = list(ULKE = list(sutun = 1, tur = "oto")),
                    sabit = list(REF_AREA = "TR"))
       },
       boyut_ad = c(ULKE = "Ülke", CINSIYET = "Cinsiyet"), turkiye = "var", olcu = "oran", birim = "yıl", son_yil_sayisi = 10,
       aciklama = "Avrupa ülkeleri ve Türkiye için doğuşta sağlıklı yaşam süresi, 2015-2024 (Türkiye değeri dönem hayat tablosundan, dönem ortası yıl)",
       baglanti_df = "BULTEN_TR_HAYAT"),

  list(ad = "HAYAT_EGITIM_BEKLENEN", kod = "bulten_hayat_egitim_beklenen", konu = "Nüfus ve demografi",
       baslik = "Beklenen yaşam süresi: yaş, eğitim durumu ve cinsiyet",
       oku = function() {
         capraz_oku(kir_oku(HY, "eğitim durumuna göre beklenen"), bas = 4:5, veri = c(6, NA), ad = "eğitime göre beklenen yaşam",
                    sutun = list(EGITIM = list(tur = "sozluk", duzey = 1, sozluk = sz(
                      OOALTI = c("^Ortaöğretim altı", "Ortaöğretim altı"), ORTAOGRETIM = c("^Ortaöğretim", "Ortaöğretim"),
                      YUKSEKOGRETIM = c("^Yükseköğretim", "Yükseköğretim"))),
                      CINSIYET = list(tur = "sozluk", duzey = 2, sozluk = HY_CINSIYET)),
                    satir = list(YAS = list(sutun = 1, tur = "yas")),
                    sabit = list(REF_AREA = "TR", TIME_PERIOD = "2025"))
       },
       boyut_ad = c(YAS = "Yaş", EGITIM = "Eğitim durumu", CINSIYET = "Cinsiyet"), turkiye = "var", olcu = "oran", birim = "yıl",
       son_yil_sayisi = 1,
       aciklama = "Yaş, eğitim durumu ve cinsiyete göre beklenen yaşam süresi, Türkiye, Hayat Tabloları 2023-2025 (dönem sonu yılı: 2025)",
       baglanti_df = "BULTEN_TR_HAYAT"),

  list(ad = "HAYAT_TEK_YAS_EX", kod = "bulten_hayat_tek_yas_ex", konu = "Nüfus ve demografi",
       baslik = "Beklenen yaşam süresi e(x): yaş ve cinsiyet (hayat tablosu)",
       oku = function() {
         d <- kir_oku(HY, "tek yaş hayat tablosu")
         # dönem etiketi yalnız her bloğun ilk satırında; aşağı doldur
         per <- NA_character_
         for (r in 5:nrow(d)) {
           if (!is.na(d[r, 1]) && grepl("^(19|20)[0-9]{2}-(19|20)[0-9]{2}$", trimws(d[r, 1]))) per <- trimws(d[r, 1])
           if (!is.na(d[r, 2])) d[r, 1] <- per
         }
         # yalnız seçili yaşlar (tüm tek yaşlar çok geniş)
         yas <- trimws(d[[2]])
         tut <- seq_len(nrow(d)) < 5 | is.na(yas) | yas %in% c(HY_YASLAR, "100+")
         d <- d[tut, ]
         capraz_oku(d, bas = 3:4, veri = c(5, NA), ad = "tek yaş e(x)",
                    sutun = list(CINSIYET = list(tur = "sozluk", duzey = 1, sozluk = HY_CINSIYET)),
                    sutun_atla = c("m\\(x", "q\\(x", "l\\(x"),
                    satir = list(TIME_PERIOD = list(sutun = 1, tur = "sozluk", sozluk = sz(
                                   `2025` = c("^2023-2025$", "2023-2025"), `2024` = c("^2022-2024$", "2022-2024"))),
                                 YAS = list(sutun = 2, tur = "yas")),
                    sabit = list(REF_AREA = "TR"))
       },
       boyut_ad = c(YAS = "Yaş", CINSIYET = "Cinsiyet"), turkiye = "var", olcu = "oran", birim = "yıl", son_yil_sayisi = 2,
       aciklama = "Türkiye hayat tablosu, x yaşında beklenen kalan yaşam süresi e(x); seçili yaşlar (0, 1, her 5 yaş, 100+). Dönem 2023-2025 için 2025, 2022-2024 için 2024 yazılmıştır",
       baglanti_df = "BULTEN_TR_HAYAT")
))
