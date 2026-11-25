# Ekonomi çapraz tabloları: yapı izin (yapi_izin_2026_q3), dış ticaret (dis_ticaret_2026_09),
# bitkisel üretim (bitkisel_uretim_2026), hayvan sayıları (hayvancilik_2026_06),
# canlı hayvan ve hayvansal ürün fiyatları / değerleri (canli_hayvan_degeri_2021).
# Not: dönem biçimleri: yıl "2025", çeyrek "2025-Q1", ay "2025-09", yıl-sonu/ara sayım "2025-12".

# --- yardımcılar (ekonomi_ önekli) -------------------------------------------
# Çift dilli etiketten Türkçe kısmı: "Koyun sütü -Sheep's milk" -> "Koyun sütü"
ekonomi_tr_sutun <- function(d, kol = 1) {
  x <- as.character(d[[kol]])
  ok <- !is.na(x) & !grepl("^(TÜİK|TurkStat|Kaynak|Source|Tablodaki|Figures|Not:|Note:)", x)
  x[ok] <- sub("\\s*-\\s*[A-Z].*$", "", x[ok])
  d[[kol]] <- x
  d
}

# Kayan nokta gürültüsünü giderir (hane basamağa yuvarlar). Dış ticarette bin ABD $ değerleri 3 ondalıklı
# (tam dolar) olduğundan carpan = 1000 ile ABD $ tam sayısına çevrilir; toplamlar böylece tam tutar.
ekonomi_yuvarla <- function(s, carpan = 1, hane = 0) {
  s$veri$OBS_VALUE <- round(s$veri$OBS_VALUE * carpan, hane)
  s
}

# Belirli bir satırdaki etiketi (beklenen desenle doğrulayarak) değiştirir.
ekonomi_etiket_yaz <- function(d, satir, beklenen, yeni, kol = 1) {
  if (!grepl(beklenen, d[[kol]][satir])) stop("satır ", satir, " beklenen etiket değil: ", d[[kol]][satir], call. = FALSE)
  d[[kol]][satir] <- yeni
  d
}

# Kullanılmayan (değeri hiç olmayan) kodların etiketlerini düşürür.
ekonomi_temizle <- function(s) {
  for (e in unique(s$etiket$boyut)) {
    if (!e %in% names(s$veri)) next
    kul <- unique(s$veri[[e]][!is.na(s$veri$OBS_VALUE)])
    s$etiket <- s$etiket[!(s$etiket$boyut == e & !s$etiket$kod %in% kul), ]
  }
  s
}

# Yapı izin: yıllık blok bitiş satırı (ikinci sütun dolmaya başlayana kadar)
ekonomi_yillik_son <- function(d, ilk = 5) {
  i <- ilk
  while (i <= nrow(d) && bosmu(d[[2]][i]) && !bosmu(d[[1]][i]) && grepl("^(19|20)[0-9]{2}$", trimws(d[[1]][i]))) i <- i + 1
  i - 1
}

# YIL + CEYREK satır eksenlerinden TIME_PERIOD "2025-Q1" üretir.
ekonomi_ceyrek <- function(s) {
  s$veri$TIME_PERIOD <- paste0(s$veri$YIL, "-Q", s$veri$CEYREK)
  s$veri$YIL <- NULL; s$veri$CEYREK <- NULL
  s$etiket <- s$etiket[!s$etiket$boyut %in% c("YIL", "CEYREK"), ]
  s
}

CEYREK_SZ <- sz(`1` = "^I$", `2` = "^II$", `3` = "^III$", `4` = "^IV$")
SATIR_YIL <- list(tur = "yil", sutun = 1, doldur = TRUE)
SATIR_CEYREK <- list(tur = "sozluk", sutun = 2, sozluk = CEYREK_SZ)
BELGE_SZ <- sz(RUHSAT = c("^Yapı ruhsatı", "Yapı ruhsatı"), KULLANMA_IZNI = c("^Yapı kullanma", "Yapı kullanma izin belgesi"))
OLCU_SZ <- sz(BINA = c("Bina sayısı", "Bina sayısı"), DAIRE = c("Daire sayısı", "Daire sayısı"),
              YUZOLCUM = c("Yüzölçüm", "Yüzölçüm (m²)"))
# Arındırılmış tablolarda yalnız adet ölçüleri (bina, daire) kalır; yüzölçüm (m²) çıkarılır.
ekonomi_adet <- function(s) {
  s$veri <- s$veri[s$veri$OLCU != "YUZOLCUM", ]
  s$etiket <- s$etiket[!(s$etiket$boyut == "OLCU" & s$etiket$kod == "YUZOLCUM"), ]
  s
}
OLCU_YUZ_SZ <- sz(BINA = c("Bina sayısı", "Bina sayısı"), DAIRE = c("Daire sayısı", "Daire sayısı"),
                  YUZOLCUM_KONUT_DISI = c("Yüzölçüm.*Konut dışı", "Yüzölçüm: konut dışı kullanım alanı"),
                  YUZOLCUM_KONUT = c("Yüzölçüm.*Konut", "Yüzölçüm: konut kullanım alanı"),
                  YUZOLCUM_ORTAK = c("Yüzölçüm.*Ortak", "Yüzölçüm: ortak kullanım alanı"),
                  YUZOLCUM_T = c("Yüzölçüm.*Toplam", "Yüzölçüm: toplam"))
INSAAT_TIPI_SZ <- sz(`_T` = c("^1$", "Binalar (tümü)"),
                     T111 = c("^111$", "Bir daireli ikamet amaçlı binalar"),
                     T112 = c("^112$", "İki ve daha fazla daireli ikamet amaçlı binalar"),
                     T113 = c("^113$", "Halka açık ikamet yerleri"),
                     T121 = c("^121$", "Otel vb. binalar"),
                     T122 = c("^122$", "Ofis (işyeri) binaları"),
                     T123 = c("^123$", "Toptan ve perakende ticaret binaları"),
                     T124 = c("^124$", "Trafik ve iletişim binaları"),
                     T125 = c("^125$", "Sanayi binaları ve depolar"),
                     T126 = c("^126$", "Kamu eğlence, eğitim, hastane veya bakım kuruluşları binaları"),
                     T127 = c("^127$", "İkamet amaçlı binalar dışındaki diğer binalar"))
SATIR_YIL2 <- list(tur = "yil", sutun = 2, doldur = TRUE)
SATIR_CEYREK3 <- list(tur = "sozluk", sutun = 3, sozluk = CEYREK_SZ)
SATIR_TIP <- list(tur = "sozluk", sutun = 1, doldur = TRUE, yoksay = "[^0-9]", sozluk = INSAAT_TIPI_SZ)

# Yapı izin yıllık tablo (belge veren idare / yapı sahipliği): BELGE x OLCU x taraf
ekonomi_yapi_yillik <- function(parca, ad, eksen, ek = list()) {
  d <- kir_oku("yapi_izin_2026_q3", parca)
  son <- ekonomi_yillik_son(d)
  s <- capraz_oku(d, bas = 2:4, veri = c(5, son), ad = ad,
                  sutun = list(BELGE = list(tur = "sozluk", sozluk = BELGE_SZ, duzey = 1),
                               OLCU = list(tur = "sozluk", sozluk = OLCU_SZ, duzey = 2),
                               TARAF = list(tur = "oto", duzey = 3)),
                  satir = list(TIME_PERIOD = list(tur = "yil", sutun = 1)),
                  sabit = list(REF_AREA = "TR"))
  names(s$veri)[names(s$veri) == "TARAF"] <- eksen
  s$etiket$boyut[s$etiket$boyut == "TARAF"] <- eksen
  ekonomi_yuvarla(s, 1, 2)
}

# Yapı izin çeyreklik (belge veren / yapı sahipliği): yıllık blok sonrası çeyrek satırları
ekonomi_yapi_ceyrek <- function(parca, ad, eksen) {
  d <- kir_oku("yapi_izin_2026_q3", parca)
  son <- ekonomi_yillik_son(d)
  s <- capraz_oku(d, bas = 2:4, veri = c(son + 1, NA), ad = ad,
                  sutun = list(BELGE = list(tur = "sozluk", sozluk = BELGE_SZ, duzey = 1),
                               OLCU = list(tur = "sozluk", sozluk = OLCU_SZ, duzey = 2),
                               TARAF = list(tur = "oto", duzey = 3)),
                  satir = list(YIL = SATIR_YIL, CEYREK = SATIR_CEYREK),
                  sabit = list(REF_AREA = "TR"))
  names(s$veri)[names(s$veri) == "TARAF"] <- eksen
  s$etiket$boyut[s$etiket$boyut == "TARAF"] <- eksen
  ekonomi_yuvarla(ekonomi_ceyrek(s), 1, 2)
}

# Dış ticaret: eksen (FAALIYET/GRUP/ULKE_GRUBU) x AKIM; donem = "Eylül" (ay) ya da "Ocak" (Ocak-Eylül kümülatif)
ekonomi_dis_ticaret <- function(parca, ad, eksen, donem, bas, atla_donem, ek_atla = character(), satir_atla = character()) {
  d <- kir_oku("dis_ticaret_2026_09", parca)
  s <- capraz_oku(d, bas = bas, veri = c(9, NA), ad = ad,
                  sutun = list(TIME_PERIOD = list(tur = "yil", duzey = 2)),
                  sutun_atla = c("Pay", "Değişim", "Change", atla_donem, ek_atla),
                  satir = setNames(list(list(sutun = 1, tur = "oto")), eksen),
                  blok = list(AKIM = list(sutun = 1, tur = "sozluk", desen = "^(İhracat|İthalat)",
                                          sozluk = sz(IHRACAT = c("^İhracat", "İhracat"), ITHALAT = c("^İthalat", "İthalat")))),
                  satir_atla = satir_atla, sabit = list(REF_AREA = "TR"))
  if (donem == "Eylül") s$veri$TIME_PERIOD <- paste0(s$veri$TIME_PERIOD, "-09")
  ekonomi_yuvarla(s, 1000)
}

# Hayvan/hayvansal ürün fiyat tabloları (satır = ürün, sütun = yıl)
ekonomi_fiyat <- function(parca, ad) {
  d <- ekonomi_tr_sutun(kir_oku("canli_hayvan_degeri_2021", parca))
  s <- capraz_oku(d, bas = 4, veri = c(5, NA), ad = ad,
                  sutun = list(TIME_PERIOD = list(tur = "yil", duzey = 1)),
                  satir = list(URUN = list(sutun = 1, tur = "oto")),
                  sabit = list(REF_AREA = "TR"))
  ekonomi_temizle(s)
}

# Bitkisel üretim: satır = ürün (grup + alt ürün, örtüşür), sütun = yıl (Pay ve Değişim hariç)
ekonomi_bitkisel <- function(parca, ad) {
  d <- ekonomi_tr_sutun(kir_oku("bitkisel_uretim_2026", parca))
  yil_sut <- which(grepl("^(19|20)[0-9]{2}$", trimws(as.character(unlist(d[7, ])))))
  s <- capraz_oku(d, bas = 7, veri = c(8, NA), ad = ad, veri_sutun = yil_sut,
                  sutun = list(TIME_PERIOD = list(tur = "yil", duzey = 1)),
                  satir = list(URUN = list(sutun = 1, tur = "oto")),
                  sabit = list(REF_AREA = "TR"))
  ekonomi_temizle(s)
}

KIR_TABLOLAR <- c(KIR_TABLOLAR, list(
  # ---- Yapı izin: yıllık ----------------------------------------------------
  list(ad = "YAPI_IZIN_BELGE_VEREN", kod = "bulten_yapi_izin_belge_veren", konu = "Ekonomi",
       baslik = "Yapı izni: belge veren yetkili idare (yıllık)",
       oku = function() ekonomi_yapi_yillik("Belge veren yetkili", "yapı izin belge veren (yıllık)", "IDARE"),
       boyut_ad = c(BELGE = "Belge türü", OLCU = "Ölçü", IDARE = "Belge veren idare"),
       turkiye = "var", olcu = "sayi", birim = "adet (bina, daire); m² (yüzölçüm)", son_yil_sayisi = 11,
       toplanamaz = "OLCU;BELGE;IDARE",
       aciklama = "Yapı ruhsatı ve yapı kullanma izin belgesi; bina, daire ve yüzölçüm; belediye ve diğer idareler (TÜİK yapı izin bülteni, 2025 yıllık)",
       baglanti_df = "BULTEN_TR_YAPI_IZIN"),
  list(ad = "YAPI_IZIN_YAPI_SAHIPLIGI", kod = "bulten_yapi_izin_yapi_sahipligi", konu = "Ekonomi",
       baslik = "Yapı izni: yapı sahipliği (yıllık)",
       oku = function() ekonomi_yapi_yillik("Yapı sahipliğine", "yapı izin yapı sahipliği (yıllık)", "SAHIPLIK"),
       boyut_ad = c(BELGE = "Belge türü", OLCU = "Ölçü", SAHIPLIK = "Yapı sahipliği"),
       turkiye = "var", olcu = "sayi", birim = "adet (bina, daire); m² (yüzölçüm)", son_yil_sayisi = 11,
       toplanamaz = "OLCU;BELGE;SAHIPLIK",
       aciklama = "Yapı ruhsatı ve yapı kullanma izin belgesi; devlet sektörü, özel sektör ve yapı kooperatifi (TÜİK yapı izin bülteni, 2025 yıllık)",
       baglanti_df = "BULTEN_TR_YAPI_IZIN"),
  # ---- Yapı izin: çeyreklik -------------------------------------------------
  list(ad = "YAPI_IZIN_BELGE_VEREN_CEYREK", kod = "bulten_yapi_izin_belge_veren_ceyrek", konu = "Ekonomi",
       baslik = "Yapı izni: belge veren yetkili idare (çeyreklik)",
       oku = function() ekonomi_yapi_ceyrek("Belge veren yetkili", "yapı izin belge veren (çeyrek)", "IDARE"),
       boyut_ad = c(BELGE = "Belge türü", OLCU = "Ölçü", IDARE = "Belge veren idare"),
       turkiye = "var", olcu = "sayi", birim = "adet (bina, daire); m² (yüzölçüm)", son_yil_sayisi = 11,
       toplanamaz = "OLCU;BELGE;IDARE",
       aciklama = "Yapı ruhsatı ve yapı kullanma izin belgesi; belediye ve diğer idareler (TÜİK yapı izin bülteni, 2010 I - 2026 III çeyrekleri)",
       baglanti_df = "BULTEN_TR_YAPI_IZIN"),
  list(ad = "YAPI_IZIN_YAPI_SAHIPLIGI_CEYREK", kod = "bulten_yapi_izin_yapi_sahipligi_ceyrek", konu = "Ekonomi",
       baslik = "Yapı izni: yapı sahipliği (çeyreklik)",
       oku = function() ekonomi_yapi_ceyrek("Yapı sahipliğine", "yapı izin yapı sahipliği (çeyrek)", "SAHIPLIK"),
       boyut_ad = c(BELGE = "Belge türü", OLCU = "Ölçü", SAHIPLIK = "Yapı sahipliği"),
       turkiye = "var", olcu = "sayi", birim = "adet (bina, daire); m² (yüzölçüm)", son_yil_sayisi = 11,
       toplanamaz = "OLCU;BELGE;SAHIPLIK",
       aciklama = "Yapı ruhsatı ve yapı kullanma izin belgesi; devlet sektörü, özel sektör ve yapı kooperatifi (TÜİK yapı izin bülteni, 2010 I - 2026 III çeyrekleri)",
       baglanti_df = "BULTEN_TR_YAPI_IZIN"),
  list(ad = "YAPI_IZIN_KULLANMA_AMACI_CEYREK", kod = "bulten_yapi_izin_kullanma_amaci_ceyrek", konu = "Ekonomi",
       baslik = "Yapı izni: kullanma amacı (çeyreklik)",
       oku = function() {
         d <- kir_oku("yapi_izin_2026_q3", "Kullanma amacına göre yapı izin")
         s <- capraz_oku(d, bas = 2:4, veri = c(5, NA), ad = "yapı izin kullanma amacı",
                         sutun = list(BELGE = list(tur = "sozluk", sozluk = BELGE_SZ, duzey = 1),
                                      OLCU = list(tur = "sozluk", sozluk = OLCU_YUZ_SZ)),
                         satir = list(INSAAT_TIPI = SATIR_TIP, YIL = SATIR_YIL2, CEYREK = SATIR_CEYREK3),
                         sabit = list(REF_AREA = "TR"))
         ekonomi_yuvarla(ekonomi_ceyrek(s), 1, 2)
       },
       boyut_ad = c(INSAAT_TIPI = "Kullanma amacı (inşaat tipi)", BELGE = "Belge türü", OLCU = "Ölçü"),
       turkiye = "var", olcu = "sayi", birim = "adet (bina, daire); m² (yüzölçüm)", son_yil_sayisi = 11,
       toplanamaz = "OLCU;BELGE;INSAAT_TIPI",
       aciklama = "Yapı ruhsatı ve yapı kullanma izin belgesi; inşaat tiplerine göre bina, daire ve yüzölçüm (TÜİK yapı izin bülteni, 2010 I - 2026 III çeyrekleri)",
       baglanti_df = "BULTEN_TR_YAPI_IZIN"),
  list(ad = "YAPI_IZIN_KULLANMA_AMACI_ARINDIRILMIS", kod = "bulten_yapi_izin_kullanma_amaci_arindirilmis", konu = "Ekonomi",
       baslik = "Yapı izni: kullanma amacı, mevsim ve takvim etkilerinden arındırılmış (çeyreklik)",
       oku = function() {
         d <- kir_oku("yapi_izin_2026_q3", "Kullanma amacına göre mevsim")
         s <- capraz_oku(d, bas = 2:4, veri = c(5, NA), ad = "yapı izin kullanma amacı arındırılmış",
                         sutun = list(BELGE = list(tur = "sozluk", sozluk = BELGE_SZ, duzey = 1),
                                      OLCU = list(tur = "sozluk", sozluk = OLCU_SZ, duzey = 2)),
                         sutun_atla = "\\| Takvim",
                         satir = list(INSAAT_TIPI = SATIR_TIP, YIL = SATIR_YIL2, CEYREK = SATIR_CEYREK3),
                         sabit = list(REF_AREA = "TR"))
         ekonomi_adet(ekonomi_ceyrek(s))
       },
       boyut_ad = c(INSAAT_TIPI = "Kullanma amacı (inşaat tipi)", BELGE = "Belge türü", OLCU = "Ölçü"),
       turkiye = "var", olcu = "sayi", birim = "adet", son_yil_sayisi = 11,
       toplanamaz = "OLCU;BELGE;INSAAT_TIPI",
       aciklama = "Mevsim ve takvim etkilerinden arındırılmış yapı ruhsatı ve yapı kullanma izni; yalnız mevsim ve takvim arındırmalı seri; bina ve daire sayısı (TÜİK yapı izin bülteni, çeyreklik)",
       baglanti_df = "BULTEN_TR_YAPI_IZIN"),
  list(ad = "YAPI_IZIN_ARINDIRILMIS", kod = "bulten_yapi_izin_arindirilmis", konu = "Ekonomi",
       baslik = "Yapı izni: mevsim ve takvim etkilerinden arındırılmış (çeyreklik)",
       oku = function() {
         d <- kir_oku("yapi_izin_2026_q3", "Mevsim ve takvim")
         s <- capraz_oku(d, bas = 2:5, veri = c(6, NA), ad = "yapı izin arındırılmış",
                         sutun = list(BELGE = list(tur = "sozluk", sozluk = BELGE_SZ, duzey = 1),
                                      OLCU = list(tur = "sozluk", sozluk = OLCU_SZ, duzey = 2),
                                      ARINDIRMA = list(tur = "sozluk", duzey = 3,
                                                       sozluk = sz(TAKVIM = c("^Takvim", "Takvim etkilerinden arındırılmış"),
                                                                   MEVSIM_TAKVIM = c("^Mevsim", "Mevsim ve takvim etkilerinden arındırılmış")))),
                         sutun_atla = c("değişim", "Annual", "Quarterly"),
                         satir = list(YIL = SATIR_YIL, CEYREK = SATIR_CEYREK),
                         sabit = list(REF_AREA = "TR"))
         ekonomi_adet(ekonomi_ceyrek(s))
       },
       boyut_ad = c(BELGE = "Belge türü", OLCU = "Ölçü", ARINDIRMA = "Arındırma türü"),
       turkiye = "var", olcu = "sayi", birim = "adet", son_yil_sayisi = 11,
       toplanamaz = "OLCU;BELGE;ARINDIRMA",
       aciklama = "Yapı ruhsatı ve yapı kullanma izni; takvim ve mevsim-takvim etkilerinden arındırılmış bina ve daire sayısı serileri (TÜİK yapı izin bülteni, çeyreklik)",
       baglanti_df = "BULTEN_TR_YAPI_IZIN"),
  # ---- Dış ticaret ----------------------------------------------------------
  list(ad = "DIS_TICARET_FAALIYET_EYLUL", kod = "bulten_dis_ticaret_faaliyet_eylul", konu = "Ekonomi",
       baslik = "Dış ticaret: ekonomik faaliyet (Eylül)",
       oku = function() ekonomi_dis_ticaret("ekonomik faaliyetlere", "dış ticaret faaliyet Eylül", "FAALIYET", "Eylül", c(4, 6, 7), "^Ocak"),
       boyut_ad = c(FAALIYET = "Ekonomik faaliyet (ISIC Rev.4)", AKIM = "İhracat / ithalat"),
       turkiye = "var", olcu = "sayi", birim = "ABD $", son_yil_sayisi = 2,
       aciklama = "Genel ticaret sistemine göre ihracat ve ithalat, yalnız Eylül ayı (TÜİK dış ticaret bülteni, Eylül 2025 ve Eylül 2026)",
       baglanti_df = "BULTEN_TR_DIS_TICARET"),
  list(ad = "DIS_TICARET_FAALIYET_KUMULATIF", kod = "bulten_dis_ticaret_faaliyet_kumulatif", konu = "Ekonomi",
       baslik = "Dış ticaret: ekonomik faaliyet (Ocak-Eylül toplamı)",
       oku = function() ekonomi_dis_ticaret("ekonomik faaliyetlere", "dış ticaret faaliyet Ocak-Eylül", "FAALIYET", "Ocak", c(4, 6, 7), "^Eylül"),
       boyut_ad = c(FAALIYET = "Ekonomik faaliyet (ISIC Rev.4)", AKIM = "İhracat / ithalat"),
       turkiye = "var", olcu = "sayi", birim = "ABD $", son_yil_sayisi = 2,
       aciklama = "Genel ticaret sistemine göre ihracat ve ithalat; her yılın Ocak-Eylül kümülatif toplamı (TÜİK dış ticaret bülteni, 2025 ve 2026)",
       baglanti_df = "BULTEN_TR_DIS_TICARET"),
  list(ad = "DIS_TICARET_GRUP_EYLUL", kod = "bulten_dis_ticaret_grup_eylul", konu = "Ekonomi",
       baslik = "Dış ticaret: geniş ekonomik gruplar (Eylül)",
       oku = function() ekonomi_dis_ticaret("geniş ekonomik", "dış ticaret geniş gruplar Eylül", "GRUP", "Eylül", c(4, 6, 7), "^Ocak"),
       boyut_ad = c(GRUP = "Geniş ekonomik grup (BEC)", AKIM = "İhracat / ithalat"),
       turkiye = "var", olcu = "sayi", birim = "ABD $", son_yil_sayisi = 2,
       aciklama = "Genel ticaret sistemine göre ihracat ve ithalat, yalnız Eylül ayı (TÜİK dış ticaret bülteni, Eylül 2025 ve Eylül 2026)",
       baglanti_df = "BULTEN_TR_DIS_TICARET"),
  list(ad = "DIS_TICARET_GRUP_KUMULATIF", kod = "bulten_dis_ticaret_grup_kumulatif", konu = "Ekonomi",
       baslik = "Dış ticaret: geniş ekonomik gruplar (Ocak-Eylül toplamı)",
       oku = function() ekonomi_dis_ticaret("geniş ekonomik", "dış ticaret geniş gruplar Ocak-Eylül", "GRUP", "Ocak", c(4, 6, 7), "^Eylül"),
       boyut_ad = c(GRUP = "Geniş ekonomik grup (BEC)", AKIM = "İhracat / ithalat"),
       turkiye = "var", olcu = "sayi", birim = "ABD $", son_yil_sayisi = 2,
       aciklama = "Genel ticaret sistemine göre ihracat ve ithalat; her yılın Ocak-Eylül kümülatif toplamı (TÜİK dış ticaret bülteni, 2025 ve 2026)",
       baglanti_df = "BULTEN_TR_DIS_TICARET"),
  list(ad = "DIS_TICARET_ULKE_GRUBU_EYLUL", kod = "bulten_dis_ticaret_ulke_grubu_eylul", konu = "Ekonomi",
       baslik = "Dış ticaret: ülke grupları (Eylül)",
       oku = function() ekonomi_dis_ticaret("ülke gruplarına", "dış ticaret ülke grupları Eylül", "ULKE_GRUBU", "Eylül", c(4, 6, 7), "^Ocak",
                                            satir_atla = "^Seçilmiş ülke grupları"),
       boyut_ad = c(ULKE_GRUBU = "Ülke grubu", AKIM = "İhracat / ithalat"),
       turkiye = "var", olcu = "sayi", birim = "ABD $", son_yil_sayisi = 2, toplanamaz = "ULKE_GRUBU",
       aciklama = "Genel ticaret sistemine göre ihracat ve ithalat, ülke grupları (örtüşen gruplar), yalnız Eylül ayı (TÜİK dış ticaret bülteni, Eylül 2025 ve 2026)",
       baglanti_df = "BULTEN_TR_DIS_TICARET"),
  list(ad = "DIS_TICARET_ULKE_GRUBU_KUMULATIF", kod = "bulten_dis_ticaret_ulke_grubu_kumulatif", konu = "Ekonomi",
       baslik = "Dış ticaret: ülke grupları (Ocak-Eylül toplamı)",
       oku = function() ekonomi_dis_ticaret("ülke gruplarına", "dış ticaret ülke grupları Ocak-Eylül", "ULKE_GRUBU", "Ocak", c(4, 6, 7), "^Eylül",
                                            satir_atla = "^Seçilmiş ülke grupları"),
       boyut_ad = c(ULKE_GRUBU = "Ülke grubu", AKIM = "İhracat / ithalat"),
       turkiye = "var", olcu = "sayi", birim = "ABD $", son_yil_sayisi = 2, toplanamaz = "ULKE_GRUBU",
       aciklama = "Genel ticaret sistemine göre ihracat ve ithalat, ülke grupları (örtüşen gruplar); her yılın Ocak-Eylül kümülatif toplamı (TÜİK dış ticaret bülteni, 2025 ve 2026)",
       baglanti_df = "BULTEN_TR_DIS_TICARET"),
  # ---- Bitkisel üretim ------------------------------------------------------
  list(ad = "BITKISEL_TAHIL", kod = "bulten_bitkisel_tahil", konu = "Ekonomi",
       baslik = "Bitkisel üretim: tahıllar ve diğer bitkisel ürünler",
       oku = function() ekonomi_bitkisel("Tahıllar", "bitkisel tahıl"),
       boyut_ad = c(URUN = "Ürün"), turkiye = "var", olcu = "sayi", birim = "ton", son_yil_sayisi = 2, toplanamaz = "URUN",
       aciklama = "Tahıllar ve diğer bitkisel ürünlerin üretim miktarı; ürün grupları ve alt ürünler birlikte (TÜİK bitkisel üretim bülteni 2026, 2025 ve 2026 tahmini)",
       baglanti_df = "BULTEN_TR_BITKISEL"),
  list(ad = "BITKISEL_SEBZE", kod = "bulten_bitkisel_sebze", konu = "Ekonomi",
       baslik = "Bitkisel üretim: sebze ürünleri",
       oku = function() ekonomi_bitkisel("Sebze", "bitkisel sebze"),
       boyut_ad = c(URUN = "Ürün"), turkiye = "var", olcu = "sayi", birim = "ton", son_yil_sayisi = 2, toplanamaz = "URUN",
       aciklama = "Sebze ürünlerinin üretim miktarı; ürün grupları ve alt ürünler birlikte (TÜİK bitkisel üretim bülteni 2026, 2025 ve 2026 tahmini)",
       baglanti_df = "BULTEN_TR_BITKISEL"),
  list(ad = "BITKISEL_MEYVE", kod = "bulten_bitkisel_meyve", konu = "Ekonomi",
       baslik = "Bitkisel üretim: meyve, içecek ve baharat bitkileri",
       oku = function() ekonomi_bitkisel("Meyve", "bitkisel meyve"),
       boyut_ad = c(URUN = "Ürün"), turkiye = "var", olcu = "sayi", birim = "ton", son_yil_sayisi = 2, toplanamaz = "URUN",
       aciklama = "Meyve, içecek ve baharat bitkilerinin üretim miktarı; ürün grupları ve alt ürünler birlikte (TÜİK bitkisel üretim bülteni 2026, 2025 ve 2026 tahmini)",
       baglanti_df = "BULTEN_TR_BITKISEL"),
  # ---- Hayvancılık ----------------------------------------------------------
  list(ad = "HAYVAN_SAYISI_TUR", kod = "bulten_hayvan_sayisi_tur", konu = "Ekonomi",
       baslik = "Hayvan sayıları: tür ve ırk",
       oku = function() {
         d <- kir_oku("hayvancilik_2026_06", "Tür ve")
         d <- ekonomi_etiket_yaz(d, 10, "^Yerli", "Sığır yerli")
         d <- ekonomi_etiket_yaz(d, 15, "^Yerli", "Koyun yerli")
         d <- ekonomi_etiket_yaz(d, 14, "^Merinos", "Koyun merinos")
         d <- ekonomi_etiket_yaz(d, 8, "^Kültür -", "Sığır kültür")
         d <- ekonomi_etiket_yaz(d, 9, "^Kültür melezi", "Sığır kültür melezi")
         d <- ekonomi_etiket_yaz(d, 19, "^Diğer", "Diğer hayvanlar")
         s <- capraz_oku(d, bas = 3:4, veri = c(5, NA), veri_sutun = c(3, 7), ad = "hayvan sayısı",
                         sutun = list(TIME_PERIOD = list(tur = "sozluk", duzey = 1,
                                                         sozluk = sz(`2025-12` = "^Aralık 2025", `2026-06` = "^Haziran 2026"))),
                         satir = list(TUR = list(sutun = 1, tur = "oto")),
                         sabit = list(REF_AREA = "TR"))
         s
       },
       boyut_ad = c(TUR = "Hayvan türü ve ırkı"), turkiye = "var", olcu = "sayi", birim = "baş", son_yil_sayisi = 2, toplanamaz = "TUR",
       aciklama = "Tür ve ırklarına göre hayvan sayısı; üst tür ve alt ırklar birlikte (TÜİK hayvancılık bülteni, Aralık 2025 ve Haziran 2026)",
       baglanti_df = "BULTEN_TR_HAYVAN"),
  # ---- Canlı hayvan ve hayvansal ürün fiyatları / değerleri -----------------
  list(ad = "CANLI_HAYVAN_DEGER_ANA_GRUP", kod = "bulten_canli_hayvan_deger_ana_grup", konu = "Ekonomi",
       baslik = "Canlı hayvan değeri: ana gruplar",
       oku = function() {
         d <- kir_oku("canli_hayvan_degeri_2021", "Ana gruplara")
         ekonomi_yuvarla(capraz_oku(d, bas = 4, veri = c(7, NA), ad = "canlı hayvan değeri ana grup",
                    sutun = list(TIME_PERIOD = list(tur = "yil", duzey = 1)),
                    sutun_atla = c("Pay", "Share", "Değişim", "Changes"),
                    satir = list(ANA_GRUP = list(sutun = 1, tur = "oto")),
                    satir_atla = "^[A-Za-z ]+$", sabit = list(REF_AREA = "TR")), 1, 3)
       },
       boyut_ad = c(ANA_GRUP = "Hayvan ana grubu"), turkiye = "var", olcu = "sayi", birim = "milyon TL", son_yil_sayisi = 2, toplanamaz = "ANA_GRUP",
       aciklama = "Canlı hayvan değeri; küçükbaş, büyükbaş, kümes hayvanları ve diğer (TÜİK canlı hayvan fiyat ve değer bülteni, 2020 ve 2021)",
       baglanti_df = ""),
  list(ad = "FIYAT_BUYUKBAS", kod = "bulten_fiyat_buyukbas", konu = "Ekonomi",
       baslik = "Büyükbaş hayvan fiyatları",
       oku = function() {
         s <- ekonomi_fiyat("Büyükbaş Hayvan", "büyükbaş fiyat")
         # tablo birimi TL/baş; bu kalem TL/canlı kg olduğundan etiketine yazılır
         i <- s$etiket$boyut == "URUN" & grepl("^Diğer sığır, manda", s$etiket$kod_ad)
         if (sum(i) != 1) stop("büyükbaş fiyat: 'Diğer sığır, manda...' kalemi bulunamadı", call. = FALSE)
         s$etiket$kod_ad[i] <- paste(sub(", canlı.*$", "", s$etiket$kod_ad[i]), "(TL/canlı kg)")
         s
       },
       boyut_ad = c(URUN = "Hayvan"), turkiye = "var", olcu = "oran", birim = "TL/baş", son_yil_sayisi = 11,
       aciklama = "Büyükbaş hayvan fiyatları (TÜİK canlı hayvan fiyat bülteni, 2008-2025); sığır ve dana kalemleri 2022 sonrası, süt sığırı ve yavru kalemleri 2023 sonrası verilir; Diğer sığır, manda ve bizonlar kalemi TL/canlı kg", baglanti_df = ""),
  list(ad = "FIYAT_KUCUKBAS", kod = "bulten_fiyat_kucukbas", konu = "Ekonomi",
       baslik = "Küçükbaş hayvan fiyatları",
       oku = function() ekonomi_fiyat("Küçükbaş Hayvan", "küçükbaş fiyat"),
       boyut_ad = c(URUN = "Hayvan"), turkiye = "var", olcu = "oran", birim = "TL/baş", son_yil_sayisi = 11,
       aciklama = "Küçükbaş hayvan fiyatları (TÜİK canlı hayvan fiyat bülteni; 2008-2025 serisi)", baglanti_df = ""),
  list(ad = "FIYAT_KUMES", kod = "bulten_fiyat_kumes", konu = "Ekonomi",
       baslik = "Kümes hayvanı fiyatları",
       oku = function() ekonomi_fiyat("Kümes Hayvanları", "kümes fiyat"),
       boyut_ad = c(URUN = "Hayvan"), turkiye = "var", olcu = "oran", birim = "TL/baş", son_yil_sayisi = 11,
       aciklama = "Kümes hayvanı fiyatları (TÜİK canlı hayvan fiyat bülteni; 2008-2025 serisi)", baglanti_df = ""),
  list(ad = "FIYAT_HAYVANSAL_URUN", kod = "bulten_fiyat_hayvansal_urun", konu = "Ekonomi",
       baslik = "Hayvansal ürün fiyatları",
       oku = function() ekonomi_fiyat("Hayvansal Ürün Fiyatları.xls", "hayvansal ürün fiyat"),
       boyut_ad = c(URUN = "Hayvansal ürün"), turkiye = "var", olcu = "oran", birim = "TL/kg (deri ürünleri TL/adet)", son_yil_sayisi = 11,
       aciklama = "Süt, et, deri, yapağı, yumurta, bal gibi hayvansal ürün fiyatları (TÜİK hayvansal ürün fiyat bülteni; 2008-2025 serisi)", baglanti_df = "")
))
