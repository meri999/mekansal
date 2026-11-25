# İç göç ve uluslararası göç çapraz tabloları (ic_goc_2025, ugoc_2025)
KIR_TABLOLAR <- c(KIR_TABLOLAR, list(
  list(ad = "IC_GOC_NEDEN_ALDIGI", kod = "bulten_ic_goc_neden_aldigi", konu = "Göç",
       baslik = "İllerin aldığı iç göç: göç etme nedeni",
       oku = function() {
         d <- kir_oku("ic_goc_2025", "Göç etme nedenine göre illerin aldığı göç")
         capraz_oku(d, bas = 4, veri = c(5, NA), ad = "göç nedeni (aldığı)",
                    sutun = list(NEDEN = list(tur = "oto", duzey = 1)),
                    satir = list(REF_AREA = list(sutun = 1, tur = "il")),
                    veri_sutun = 2:14, sabit = list(TIME_PERIOD = "2025"))
       },
       boyut_ad = c(NEDEN = "Göç etme nedeni"), turkiye = "var", olcu = "sayi", birim = "kişi", son_yil_sayisi = 1,
       aciklama = "İllere göç eden nüfus; göç etme nedenine göre (2025)", baglanti_df = "BULTEN_GOC_NEDENI_IL"),

  list(ad = "IC_GOC_NEDEN_VERDIGI", kod = "bulten_ic_goc_neden_verdigi", konu = "Göç",
       baslik = "İllerin verdiği iç göç: göç etme nedeni",
       oku = function() {
         d <- kir_oku("ic_goc_2025", "Göç etme nedenine göre illerin verdiği göç")
         capraz_oku(d, bas = 4, veri = c(5, NA), ad = "göç nedeni (verdiği)",
                    sutun = list(NEDEN = list(tur = "oto", duzey = 1)),
                    satir = list(REF_AREA = list(sutun = 1, tur = "il")),
                    veri_sutun = 2:14, sabit = list(TIME_PERIOD = "2025"))
       },
       boyut_ad = c(NEDEN = "Göç etme nedeni"), turkiye = "var", olcu = "sayi", birim = "kişi", son_yil_sayisi = 1,
       aciklama = "İllerden göç eden nüfus; göç etme nedenine göre (2025)", baglanti_df = "BULTEN_GOC_NEDENI_IL"),

  list(ad = "IC_GOC_CINSIYET", kod = "bulten_ic_goc_cinsiyet", konu = "Göç",
       baslik = "İller arası göç eden nüfus: cinsiyet",
       oku = function() {
         d <- kir_oku("ic_goc_2025", "Cinsiyete göre iller arası göç eden nüfus ve oranı")
         capraz_oku(d, bas = 4, veri = c(5, NA), ad = "iç göç cinsiyet",
                    sutun = list(CINSIYET = list(tur = "sozluk", sozluk = CINSIYET_SZ, duzey = 1)),
                    satir = list(TIME_PERIOD = list(sutun = 1, tur = "yil")),
                    veri_sutun = 2:4, sabit = list(REF_AREA = "TR"))
       },
       boyut_ad = c(CINSIYET = "Cinsiyet"), turkiye = "var", olcu = "sayi", birim = "kişi", son_yil_sayisi = 11,
       aciklama = "İller arası göç eden nüfus; cinsiyete göre (2008-2025; 2008-2017 yabancı nüfus dahil, 2018 sonrası hariç)",
       baglanti_df = "BULTEN_IC_GOC_IL"),

  list(ad = "IC_GOC_NEDEN_YIL", kod = "bulten_ic_goc_neden_yil", konu = "Göç",
       baslik = "İller arası göç eden nüfus: göç etme nedeni",
       oku = function() {
         d <- kir_oku("ic_goc_2025", "Göç etme nedenine göre iller arası göç eden nüfus ve oranı")
         capraz_oku(d, bas = 4, veri = c(5, NA), ad = "iç göç neden yıl",
                    sutun = list(TIME_PERIOD = list(tur = "yil", duzey = 1)),
                    satir = list(NEDEN = list(sutun = 1, tur = "oto")),
                    veri_sutun = 2:9, sabit = list(REF_AREA = "TR"))
       },
       boyut_ad = c(NEDEN = "Göç etme nedeni"), turkiye = "var", olcu = "sayi", birim = "kişi", son_yil_sayisi = 8,
       aciklama = "İller arası göç eden nüfus; göç etme nedenine göre (2018-2025)", baglanti_df = "BULTEN_IC_GOC_IL"),

  list(ad = "IC_GOC_NEDEN_CINSIYET", kod = "bulten_ic_goc_neden_cinsiyet", konu = "Göç",
       baslik = "İller arası göç eden nüfus: göç etme nedeni ve cinsiyet",
       oku = function() {
         d <- kir_oku("ic_goc_2025", "Göç etme nedeni ve cinsiyete göre iller arası göç eden nüfus ve oranı")
         capraz_oku(d, bas = 3, veri = c(5, NA), ad = "iç göç neden cinsiyet",
                    sutun = list(CINSIYET = list(tur = "sozluk", sozluk = CINSIYET_SZ, duzey = 1)),
                    satir = list(NEDEN = list(sutun = 1, tur = "oto")),
                    veri_sutun = 2:4, sabit = list(REF_AREA = "TR", TIME_PERIOD = "2025"))
       },
       boyut_ad = c(NEDEN = "Göç etme nedeni", CINSIYET = "Cinsiyet"), turkiye = "var", olcu = "sayi", birim = "kişi", son_yil_sayisi = 1,
       aciklama = "İller arası göç eden nüfus; göç etme nedeni ve cinsiyete göre (2025)", baglanti_df = "BULTEN_GOC_NEDENI_IL"),

  list(ad = "IC_GOC_NEDEN_EGITIM", kod = "bulten_ic_goc_neden_egitim", konu = "Göç",
       baslik = "İller arası göç eden nüfus: göç etme nedeni ve eğitim durumu",
       oku = function() {
         d <- kir_oku("ic_goc_2025", "Göç etme nedeni ve bitirilen eğitim durumuna göre")
         capraz_oku(d, bas = 4, veri = c(5, NA), ad = "iç göç neden eğitim",
                    sutun = list(EGITIM = list(tur = "sozluk", duzey = 1, sozluk = sz(
                      `_T` = "^Toplam", OKUR_YAZAR_DEGIL = c("^Okuma yazma bilmeyen", "Okuma yazma bilmeyen"),
                      OKUR_YAZAR_OKULSUZ = c("^Okuma yazma bilen", "Okuma yazma bilen fakat bir okul bitirmeyen"),
                      ILKOKUL = c("^İlkokul", "İlkokul"), ILKOGRETIM_ORTAOKUL = c("^İlköğretim", "İlköğretim, ortaokul veya dengi okul"),
                      LISE = c("^Lise", "Lise veya dengi okul"), YUKSEKOGRETIM = c("^Yükseköğretim", "Yükseköğretim"),
                      BILINMEYEN = c("^Bilinmeyen", "Bilinmeyen")))),
                    satir = list(NEDEN = list(sutun = 1, tur = "oto")),
                    veri_sutun = 2:9, sabit = list(REF_AREA = "TR", TIME_PERIOD = "2025"))
       },
       boyut_ad = c(NEDEN = "Göç etme nedeni", EGITIM = "Bitirilen eğitim"), turkiye = "var", olcu = "sayi", birim = "kişi", son_yil_sayisi = 1,
       aciklama = "İller arası göç eden 6 yaş ve üzeri nüfus; göç etme nedeni ve bitirilen eğitim durumuna göre (2025)",
       baglanti_df = "BULTEN_GOC_NEDENI_IL"),

  list(ad = "IC_GOC_YAS_CINSIYET", kod = "bulten_ic_goc_yas_cinsiyet", konu = "Göç",
       baslik = "İller arası göç eden nüfus: yaş grubu ve cinsiyet",
       oku = function() {
         d <- kir_oku("ic_goc_2025", "Yaş grubu ve cinsiyete göre iller arası göç eden nüfus")
         capraz_oku(d, bas = 3, veri = c(4, NA), ad = "iç göç yaş cinsiyet",
                    sutun = list(CINSIYET = list(tur = "sozluk", sozluk = CINSIYET_SZ, duzey = 1)),
                    satir = list(YAS = list(sutun = 1, tur = "yas")),
                    veri_sutun = 2:4, sabit = list(REF_AREA = "TR", TIME_PERIOD = "2025"))
       },
       boyut_ad = c(YAS = "Yaş grubu", CINSIYET = "Cinsiyet"), turkiye = "var", olcu = "sayi", birim = "kişi", son_yil_sayisi = 1,
       aciklama = "İller arası göç eden nüfus; yaş grubu ve cinsiyete göre (2025)", baglanti_df = "BULTEN_IC_GOC_IL"),

  list(ad = "IC_GOC_YAS_CINSIYET_NEDEN", kod = "bulten_ic_goc_yas_cinsiyet_neden", konu = "Göç",
       baslik = "İller arası göç eden nüfus: yaş grubu, cinsiyet ve göç etme nedeni",
       oku = function() {
         d <- kir_oku("ic_goc_2025", "Yaş grubu, cinsiyet ve göç etme nedenine göre")
         capraz_oku(d, bas = 3, veri = c(4, NA), ad = "iç göç yaş cinsiyet neden",
                    sutun = list(NEDEN = list(tur = "oto", duzey = 1)),
                    satir = list(YAS = list(sutun = 1, tur = "yas", doldur = TRUE),
                                 CINSIYET = list(sutun = 2, tur = "sozluk", sozluk = CINSIYET_SZ)),
                    veri_sutun = 3:15, sabit = list(REF_AREA = "TR", TIME_PERIOD = "2025"))
       },
       boyut_ad = c(YAS = "Yaş grubu", CINSIYET = "Cinsiyet", NEDEN = "Göç etme nedeni"), turkiye = "var", olcu = "sayi", birim = "kişi", son_yil_sayisi = 1,
       aciklama = "İller arası göç eden nüfus; yaş grubu, cinsiyet ve göç etme nedenine göre (2025)", baglanti_df = "BULTEN_GOC_NEDENI_IL"),

  # --- uluslararası göç (ugoc_2025) ---
  list(ad = "UGOC_YABANCI_ULKE_YIL", kod = "bulten_ugoc_yabanci_ulke_yil", konu = "Göç",
       baslik = "Yabancı uyruklu nüfus: vatandaşlık ülkesi, ikamet başlangıç yılı ve cinsiyet",
       oku = function() {
         d <- kir_oku("ugoc_2025", "ikamet etmeye başladığı yıla, cinsiyete ve vatandaşlık ülkesine")
         yil_sz <- sz(`_T` = c("^Toplam yabancı", "Toplam"), Y2025 = c("[|] 2025 [|]", "2025"), Y2024 = c("[|] 2024 [|]", "2024"),
                      Y2023 = c("[|] 2023 [|]", "2023"), Y2022 = c("[|] 2022 [|]", "2022"), Y2021 = c("[|] 2021 [|]", "2021"),
                      Y_LE2020 = c("[|] 2020", "2020 ve öncesi"))
         capraz_oku(d, bas = 4:6, veri = c(7, NA), ad = "yabancı nüfus ülke yıl",
                    sutun = list(IKAMET_YILI = list(tur = "sozluk", sozluk = yil_sz, tam = TRUE),
                                 CINSIYET = list(tur = "sozluk", sozluk = CINSIYET_SZ, duzey = 3)),
                    satir = list(ULKE = list(sutun = 1, tur = "oto")),
                    veri_sutun = c(2:4, 6:8, 10:12, 14:16, 18:20, 22:24, 26:28),
                    sabit = list(REF_AREA = "TR", TIME_PERIOD = "2025"))
       },
       boyut_ad = c(ULKE = "Vatandaşlık ülkesi", IKAMET_YILI = "İkamet etmeye başladığı yıl", CINSIYET = "Cinsiyet"),
       turkiye = "var", olcu = "sayi", birim = "kişi", son_yil_sayisi = 1,
       aciklama = "Türkiye'de ikamet eden yabancı uyruklu nüfus (31 Aralık 2025); vatandaşlık ülkesi, ikamet etmeye başladığı yıl ve cinsiyete göre",
       baglanti_df = "BULTEN_UGOC_IL"),

  list(ad = "UGOC_YABANCI_YAS_YIL", kod = "bulten_ugoc_yabanci_yas_yil", konu = "Göç",
       baslik = "Yabancı uyruklu nüfus: yaş grubu, ikamet başlangıç yılı ve cinsiyet",
       oku = function() {
         d <- kir_oku("ugoc_2025", "ikamet etmeye başladığı yıla, cinsiyete ve yaş grubuna")
         yil_sz <- sz(`_T` = c("^(Toplam|Total foreign)", "Toplam"), Y2025 = c("^2025", "2025"), Y2024 = c("^2024", "2024"),
                      Y2023 = c("^2023", "2023"), Y2022 = c("^2022", "2022"), Y2021 = c("^2021", "2021"),
                      Y_LE2020 = c("^2020", "2020 ve öncesi"))
         satir <- list(IKAMET_YILI = list(sutun = 2, tur = "sozluk", sozluk = yil_sz, doldur = TRUE),
                       CINSIYET = list(sutun = 3, tur = "sozluk", sozluk = CINSIYET_SZ))
         a <- capraz_oku(d, bas = 4, veri = c(6, NA), ad = "yabancı nüfus yaş yıl (toplam)",
                         sutun = list(YAS = list(tur = "sozluk", sozluk = sz(`_T` = "^Toplam"), duzey = 1)),
                         satir = satir, veri_sutun = 4, sabit = list(REF_AREA = "TR", TIME_PERIOD = "2025"))
         b <- capraz_oku(d, bas = 5, veri = c(6, NA), ad = "yabancı nüfus yaş yıl",
                         sutun = list(YAS = list(tur = "yas", duzey = 1)),
                         satir = satir, veri_sutun = 5:20, sabit = list(REF_AREA = "TR", TIME_PERIOD = "2025"))
         kir_birlestir(a, b)
       },
       boyut_ad = c(YAS = "Yaş grubu", IKAMET_YILI = "İkamet etmeye başladığı yıl", CINSIYET = "Cinsiyet"),
       turkiye = "var", olcu = "sayi", birim = "kişi", son_yil_sayisi = 1,
       aciklama = "Türkiye'de ikamet eden yabancı uyruklu nüfus (31 Aralık 2025); yaş grubu, ikamet etmeye başladığı yıl ve cinsiyete göre",
       baglanti_df = "BULTEN_UGOC_IL"),

  list(ad = "UGOC_ULKE", kod = "bulten_ugoc_ulke", konu = "Göç",
       baslik = "Uluslararası göç: vatandaşlık ülkesi, göç yönü ve cinsiyet",
       oku = function() {
         d <- kir_oku("ugoc_2025", "Vatandaşlık ülkesine göre Türkiye'ye gelen")
         capraz_oku(d, bas = 4:5, veri = c(6, NA), ad = "uluslararası göç ülke",
                    sutun = list(YON = list(tur = "sozluk", duzey = 1, sozluk = sz(GELEN = "^Türkiye.ye gelen", GIDEN = "^Türkiye.den giden")),
                                 CINSIYET = list(tur = "sozluk", sozluk = CINSIYET_SZ, duzey = 2)),
                    satir = list(TIME_PERIOD = list(sutun = 1, tur = "yil", doldur = TRUE),
                                 ULKE = list(sutun = 2, tur = "oto")),
                    veri_sutun = c(3:5, 7:9), sabit = list(REF_AREA = "TR"))
       },
       boyut_ad = c(YON = "Göç yönü", ULKE = "Vatandaşlık ülkesi", CINSIYET = "Cinsiyet"),
       turkiye = "var", olcu = "sayi", birim = "kişi", son_yil_sayisi = 2, toplanamaz = "ULKE",
       aciklama = "Türkiye'ye gelen ve Türkiye'den giden göç; vatandaşlık ülkesi ve cinsiyete göre (2024-2025). Toplam, Türk vatandaşları ve yabancı uyruklular (ara toplam) ülke listesiyle birlikte yer alır",
       baglanti_df = "BULTEN_UGOC_IL"),

  list(ad = "UGOC_YAS_CINSIYET", kod = "bulten_ugoc_yas_cinsiyet", konu = "Göç",
       baslik = "Uluslararası göç: yaş grubu, göç yönü ve cinsiyet",
       oku = function() {
         d <- kir_oku("ugoc_2025", "Yaş grubu ve cinsiyete göre Türkiye'ye gelen")
         capraz_oku(d, bas = 4:5, veri = c(6, NA), ad = "uluslararası göç yaş",
                    sutun = list(YON = list(tur = "sozluk", duzey = 1, sozluk = sz(GELEN = "^Türkiye.ye gelen", GIDEN = "^Türkiye.den giden")),
                                 CINSIYET = list(tur = "sozluk", sozluk = CINSIYET_SZ, duzey = 2)),
                    satir = list(TIME_PERIOD = list(sutun = 1, tur = "yil", doldur = TRUE),
                                 YAS = list(sutun = 2, tur = "yas")),
                    veri_sutun = c(4:6, 8:10), sabit = list(REF_AREA = "TR"))
       },
       boyut_ad = c(YON = "Göç yönü", YAS = "Yaş grubu", CINSIYET = "Cinsiyet"),
       turkiye = "var", olcu = "sayi", birim = "kişi", son_yil_sayisi = 2,
       aciklama = "Türkiye'ye gelen ve Türkiye'den giden göç; yaş grubu ve cinsiyete göre (2024-2025)",
       baglanti_df = "BULTEN_UGOC_IL"),

  list(ad = "UGOC_IL_CINSIYET", kod = "bulten_ugoc_il_cinsiyet", konu = "Göç",
       baslik = "Uluslararası göç (il): göç yönü ve cinsiyet",
       oku = function() {
         d <- kir_oku("ugoc_2025", "İllere ve cinsiyete göre")
         capraz_oku(d, bas = 4:5, veri = c(6, NA), ad = "uluslararası göç il cinsiyet",
                    sutun = list(YON = list(tur = "sozluk", duzey = 1, sozluk = sz(GELEN = "^Türkiye.ye gelen", GIDEN = "^Türkiye.den giden")),
                                 CINSIYET = list(tur = "sozluk", sozluk = CINSIYET_SZ, duzey = 2)),
                    satir = list(TIME_PERIOD = list(sutun = 1, tur = "yil", doldur = TRUE),
                                 REF_AREA = list(sutun = 2, tur = "il")),
                    veri_sutun = c(4:6, 8:10))
       },
       boyut_ad = c(YON = "Göç yönü", CINSIYET = "Cinsiyet"),
       turkiye = "var", olcu = "sayi", birim = "kişi", son_yil_sayisi = 2,
       aciklama = "Türkiye'ye gelen ve Türkiye'den giden göç; illere ve cinsiyete göre (2024-2025)",
       baglanti_df = "BULTEN_UGOC_IL"),

  list(ad = "UGOC_IL_VATANDASLIK", kod = "bulten_ugoc_il_vatandaslik", konu = "Göç",
       baslik = "Uluslararası göç (il): göç yönü ve vatandaşlık",
       oku = function() {
         d <- kir_oku("ugoc_2025", "İllere ve vatandaşlığa göre")
         capraz_oku(d, bas = 4:5, veri = c(6, NA), ad = "uluslararası göç il vatandaşlık",
                    sutun = list(YON = list(tur = "sozluk", duzey = 1, sozluk = sz(GELEN = "^Türkiye.ye gelen", GIDEN = "^Türkiye.den giden")),
                                 VATANDASLIK = list(tur = "sozluk", duzey = 2,
                                                    sozluk = sz(`_T` = "^Toplam", TURK = c("^Türk", "Türk vatandaşları"),
                                                                YABANCI = c("^Yabancı", "Yabancı uyruklular")))),
                    satir = list(TIME_PERIOD = list(sutun = 1, tur = "yil", doldur = TRUE),
                                 REF_AREA = list(sutun = 2, tur = "il")),
                    veri_sutun = c(7:9, 11:13))
       },
       boyut_ad = c(YON = "Göç yönü", VATANDASLIK = "Vatandaşlık"),
       turkiye = "var", olcu = "sayi", birim = "kişi", son_yil_sayisi = 2,
       aciklama = "Türkiye'ye gelen ve Türkiye'den giden göç; illere ve vatandaşlığa göre (2024-2025)",
       baglanti_df = "BULTEN_UGOC_IL")
))
