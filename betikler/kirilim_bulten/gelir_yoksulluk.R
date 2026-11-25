# Gelir dağılımı ve yoksulluk çapraz tabloları (gelir_dagilimi_2025, yoksulluk_yasam_kosullari_2025)
# Yardımcı işlevler gy_ önekli (başka tanım dosyalarıyla çakışmasın diye).

GY_GELIR <- "gelir_dagilimi_2025"
GY_YOKSUL <- "yoksulluk_yasam_kosullari_2025"
GY_YIL_BLOK <- "^(19|20)[0-9]{2}$"

# %10'luk ve %20'lik gelir grubu sözlükleri
GY_G10 <- sz(`_T` = "^Toplam",
             D01 = c("^İlk", "İlk %10"), D02 = c("^İkinci", "İkinci %10"), D03 = c("^Üçüncü", "Üçüncü %10"),
             D04 = c("^Dördüncü", "Dördüncü %10"), D05 = c("^Beşinci", "Beşinci %10"), D06 = c("^Altıncı", "Altıncı %10"),
             D07 = c("^Yedinci", "Yedinci %10"), D08 = c("^Sekizinci", "Sekizinci %10"), D09 = c("^Dokuzuncu", "Dokuzuncu %10"),
             D10 = c("^Son", "Son %10"))
GY_G20 <- sz(`_T` = "^Toplam",
             Q1 = c("^İlk", "İlk %20"), Q2 = c("^İkinci", "İkinci %20"), Q3 = c("^Üçüncü", "Üçüncü %20"),
             Q4 = c("^Dördüncü", "Dördüncü %20"), Q5 = c("^Son", "Son %20"))
# Yoksulluk eşiği (medyan gelirin yüzdesi)
GY_ESIK <- sz(E40 = c("%40", "Medyanın %40'ı"), E50 = c("%50", "Medyanın %50'si"),
              E60 = c("%60", "Medyanın %60'ı"), E70 = c("%70", "Medyanın %70'i"))

# Etikete elle eksen kodu ekler (sabit eksenler için)
gy_etiket_ekle <- function(s, boyut, kod, ad) {
  s$etiket <- rbind(s$etiket, data.frame(boyut = boyut, kod = kod, kod_ad = ad, stringsAsFactors = FALSE))
  s
}

# Yıllık bölge serisi (Gini, P80/P20): eşdeğer ve hanehalkı gelir tanımı tek tabloda
gy_seri <- function(es, hh) {
  oku1 <- function(parca, kod) {
    capraz_oku(kir_oku(GY_GELIR, parca), bas = 4:5, veri = c(6, NA), ad = parca,
               sutun = list(TIME_PERIOD = list(tur = "yil", duzey = 2)),
               satir = list(REF_AREA = list(sutun = 1, tur = "ibbs2")), sabit = list(GELIR_TANIMI = kod))
  }
  s <- kir_birlestir(oku1(es, "ESDEGER"), oku1(hh, "HANEHALKI"))
  gy_etiket_ekle(s, "GELIR_TANIMI", c("ESDEGER", "HANEHALKI"),
                 c("Eşdeğer hanehalkı kullanılabilir fert geliri", "Hanehalkı kullanılabilir geliri"))
}

# Yıl bloğu > bölge bloğu > (Yüzde / Ortalama / Medyan) satırları; sütun = gelir grubu
gy_dagilim <- function(parca, grup, satir_desen) {
  d <- kir_oku(GY_GELIR, parca)
  e <- trimws(d[[1]])
  d <- d[!(grepl("^(Yüzde|Ortalama|Medyan)", e) & !grepl(satir_desen, e)), ]
  capraz_oku(d, bas = 4:5, veri = c(6, NA), ad = parca,
             sutun = list(GRUP = list(tur = "sozluk", duzey = 2, sozluk = grup)),
             blok = list(TIME_PERIOD = list(sutun = 1, tur = "yil", desen = GY_YIL_BLOK),
                         REF_AREA = list(sutun = 1, tur = "ibbs2", desen = "^TR")))
}

# Bölge bazlı yoksulluk: yıl bloğu > bölge satırı; sütun = eşik x ölçü ("atla" ile ölçü seçilir)
gy_ayri <- function(atla) {
  capraz_oku(kir_oku(GY_YOKSUL, "Bölgeler için ayrı"), bas = 4:5, veri = c(6, NA), ad = "bölgeler için ayrı",
             sutun = list(ESIK = list(tur = "sozluk", duzey = 1, sozluk = GY_ESIK)), sutun_atla = atla,
             satir = list(REF_AREA = list(sutun = 1, tur = "ibbs2")),
             blok = list(TIME_PERIOD = list(sutun = 1, tur = "yil", desen = GY_YIL_BLOK)))
}

# Türkiye-İBBS 2. düzey 2024-2025: eşik bloğu > bölge satırı; sütun = ölçü x yıl
gy_esik_bolge <- function(atla) {
  capraz_oku(kir_oku(GY_YOKSUL, "yoksul sayıları ve yoksulluk oranları (Türkiye-İBBS"), bas = c(4, 6), veri = c(7, NA),
             ad = "eşdeğer gelir İBBS-2", sutun = list(TIME_PERIOD = list(tur = "yil", duzey = 2)), sutun_atla = atla,
             satir = list(REF_AREA = list(sutun = 1, tur = "ibbs2")),
             blok = list(ESIK = list(sutun = 1, tur = "sozluk", sozluk = GY_ESIK, desen = "^Medyan gelirin")))
}

# Türkiye için hesaplanan yoksulluk sınırı: yıl bloğu > bölge; sütun = eşik x ölçü x SGP
gy_tr_sinir <- function(atla) {
  capraz_oku(kir_oku(GY_YOKSUL, "Türkiye için hesaplanan"), bas = 4:6, veri = c(7, NA), ad = "Türkiye sınırı",
             sutun = list(ESIK = list(tur = "sozluk", duzey = 1, sozluk = GY_ESIK),
                          SGP = list(tur = "sozluk", duzey = 3,
                                     sozluk = sz(SGPLI = c("kullanılarak", "SGP ile"), SGPSIZ = c("kullanılmadan", "SGP olmadan")))),
             sutun_atla = atla, satir = list(REF_AREA = list(sutun = 1, tur = "ibbs2")),
             blok = list(TIME_PERIOD = list(sutun = 1, tur = "yil", desen = GY_YIL_BLOK)))
}

# Türkiye, eşik %40-70: satır = eşik; sütun = ölçü x yıl
gy_tr_esik <- function(atla) {
  capraz_oku(kir_oku(GY_YOKSUL, "yoksulluk açığı (Türkiye)"), bas = c(3, 5), veri = c(6, NA), ad = "Türkiye eşik",
             sutun = list(TIME_PERIOD = list(tur = "yil", duzey = 2)), sutun_atla = atla,
             satir = list(ESIK = list(sutun = 1, tur = "sozluk", sozluk = GY_ESIK)), sabit = list(REF_AREA = "TR"))
}

# Hanehalkı tipi: yıl bloğu > tip satırı; sütun = eşik x ölçü
GY_HHTIP <- sz(`_T` = "^Toplam", TEK_KISI = c("^Tek kişilik", "Tek kişilik hanehalkı"),
               TEK_CEKIRDEK = c("^Tek çekirdek", "Tek çekirdek aileden oluşan hanehalkı"),
               ESLER = c("^Sadece eşlerden", "Sadece eşlerden oluşan çekirdek aile"),
               ESLER_COCUK = c("^Eşler ve çocuklardan", "Eşler ve çocuklardan oluşan çekirdek aile"),
               TEK_EBEVEYN = c("^Tek ebeveyn", "Tek ebeveyn ve çocuklardan oluşan çekirdek aile"),
               GENIS_AILE = c("^En az bir çekirdek", "Çekirdek aile ve diğer kişilerden oluşan hanehalkı"),
               AILESIZ = c("^Çekirdek aile bulunmayan", "Çekirdek aile bulunmayan, birden fazla kişilik hanehalkı"))
gy_hhtip <- function(atla) {
  capraz_oku(kir_oku(GY_YOKSUL, "Hanehalkı tipine göre"), bas = 3:4, veri = c(5, NA), ad = "hanehalkı tipi",
             sutun = list(ESIK = list(tur = "sozluk", duzey = 1, sozluk = GY_ESIK)), sutun_atla = atla,
             satir = list(HHTIP = list(sutun = 1, tur = "sozluk", sozluk = GY_HHTIP)),
             blok = list(TIME_PERIOD = list(sutun = 1, tur = "yil", desen = GY_YIL_BLOK)), sabit = list(REF_AREA = "TR"))
}

GY_BOLGE_AD <- "Gelir ve Yaşam Koşulları Araştırması"

KIR_TABLOLAR <- c(KIR_TABLOLAR, list(

  # ---- gelir dağılımı ------------------------------------------------------
  list(ad = "GELIR_GINI", kod = "bulten_gelir_gini", konu = "Kalkınma",
       baslik = "Gini katsayısı: gelir tanımına göre (bölge)",
       oku = function() gy_seri("fert gelirine göre Gini katsayısı.xls", "gelire göre Gini katsayısı.xls"),
       boyut_ad = c(GELIR_TANIMI = "Gelir tanımı"), turkiye = "var", olcu = "oran", birim = "katsayı", son_yil_sayisi = 11,
       aciklama = "Eşdeğer fert geliri ve hanehalkı kullanılabilir gelire göre Gini katsayısı, İBBS 2. düzey (Gelir Dağılımı İstatistikleri 2025)",
       baglanti_df = "BULTEN_IBBS2_IL"),

  list(ad = "GELIR_P80_P20", kod = "bulten_gelir_p80_p20", konu = "Kalkınma",
       baslik = "P80/P20 oranı: gelir tanımına göre (bölge)",
       oku = function() gy_seri("fert gelirine göre P80_P20 oranı.xls", "gelire göre P80_P20 oranı.xls"),
       boyut_ad = c(GELIR_TANIMI = "Gelir tanımı"), turkiye = "var", olcu = "oran", birim = "oran", son_yil_sayisi = 11,
       aciklama = "Eşdeğer fert geliri ve hanehalkı kullanılabilir gelire göre P80/P20 oranı, İBBS 2. düzey (Gelir Dağılımı İstatistikleri 2025)",
       baglanti_df = "BULTEN_IBBS2_IL"),

  list(ad = "GELIR_PAY_10LUK", kod = "bulten_gelir_pay_10luk", konu = "Kalkınma",
       baslik = "Gelirin %10'luk gruplara dağılımı: grup payı (bölge)",
       oku = function() gy_dagilim("yüzde 10'luk gruplar itibarıyla yıllık fert gelirinin", GY_G10, "^Yüzde"),
       boyut_ad = c(GRUP = "Gelir grubu (%10'luk)"), turkiye = "var", olcu = "oran", birim = "%", son_yil_sayisi = 11,
       aciklama = "Eşdeğer hanehalkı kullanılabilir fert gelirine göre sıralı %10'luk gruplarda yıllık gelirin payı, İBBS 2. düzey",
       baglanti_df = "BULTEN_IBBS2_IL"),

  list(ad = "GELIR_ORT_10LUK", kod = "bulten_gelir_ort_10luk", konu = "Kalkınma",
       baslik = "Yıllık ortalama gelir: %10'luk gelir grubu (bölge)",
       oku = function() gy_dagilim("yüzde 10'luk gruplar itibarıyla yıllık fert gelirinin", GY_G10, "^Ortalama"),
       boyut_ad = c(GRUP = "Gelir grubu (%10'luk)"), turkiye = "var", olcu = "oran", birim = "TL", son_yil_sayisi = 11,
       aciklama = "Eşdeğer hanehalkı kullanılabilir fert gelirine göre sıralı %10'luk gruplarda yıllık ortalama gelir (TL), İBBS 2. düzey",
       baglanti_df = "BULTEN_IBBS2_IL"),

  list(ad = "GELIR_PAY_20LUK_ESDEGER", kod = "bulten_gelir_pay_20luk_esdeger", konu = "Kalkınma",
       baslik = "Eşdeğer gelirin %20'lik gruplara dağılımı: grup payı (bölge)",
       oku = function() gy_dagilim("Eşdeğer hanehalkı kullanılabilir fert gelirine göre sıralı yüzde 20'lik gruplar itibarıyla yıllık fert gelirinin", GY_G20, "^Yüzde"),
       boyut_ad = c(GRUP = "Gelir grubu (%20'lik)"), turkiye = "var", olcu = "oran", birim = "%", son_yil_sayisi = 11,
       aciklama = "Eşdeğer hanehalkı kullanılabilir fert gelirine göre sıralı %20'lik gruplarda yıllık gelirin payı, İBBS 2. düzey",
       baglanti_df = "BULTEN_IBBS2_IL"),

  list(ad = "GELIR_ORT_20LUK_ESDEGER", kod = "bulten_gelir_ort_20luk_esdeger", konu = "Kalkınma",
       baslik = "Eşdeğer yıllık ortalama gelir: %20'lik gelir grubu (bölge)",
       oku = function() gy_dagilim("Eşdeğer hanehalkı kullanılabilir fert gelirine göre sıralı yüzde 20'lik gruplar itibarıyla yıllık fert gelirinin", GY_G20, "^Ortalama"),
       boyut_ad = c(GRUP = "Gelir grubu (%20'lik)"), turkiye = "var", olcu = "oran", birim = "TL", son_yil_sayisi = 11,
       aciklama = "Eşdeğer hanehalkı kullanılabilir fert gelirine göre sıralı %20'lik gruplarda yıllık ortalama gelir (TL), İBBS 2. düzey",
       baglanti_df = "BULTEN_IBBS2_IL"),

  list(ad = "GELIR_PAY_20LUK_HANEHALKI", kod = "bulten_gelir_pay_20luk_hanehalki", konu = "Kalkınma",
       baslik = "Hanehalkı gelirinin %20'lik gruplara dağılımı: grup payı (bölge)",
       oku = function() gy_dagilim("Hanehalkı kullanılabilir gelire göre sıralı yüzde 20'lik", GY_G20, "^Yüzde"),
       boyut_ad = c(GRUP = "Gelir grubu (%20'lik)"), turkiye = "var", olcu = "oran", birim = "%", son_yil_sayisi = 11,
       aciklama = "Hanehalkı kullanılabilir gelirine göre sıralı %20'lik gruplarda yıllık gelirin payı, İBBS 2. düzey",
       baglanti_df = "BULTEN_IBBS2_IL"),

  list(ad = "GELIR_ORT_20LUK_HANEHALKI", kod = "bulten_gelir_ort_20luk_hanehalki", konu = "Kalkınma",
       baslik = "Hanehalkı yıllık ortalama gelir: %20'lik gelir grubu (bölge)",
       oku = function() gy_dagilim("Hanehalkı kullanılabilir gelire göre sıralı yüzde 20'lik", GY_G20, "^Ortalama"),
       boyut_ad = c(GRUP = "Gelir grubu (%20'lik)"), turkiye = "var", olcu = "oran", birim = "TL", son_yil_sayisi = 11,
       aciklama = "Hanehalkı kullanılabilir gelirine göre sıralı %20'lik gruplarda yıllık ortalama gelir (TL), İBBS 2. düzey",
       baglanti_df = "BULTEN_IBBS2_IL"),

  list(ad = "GELIR_GRUP_GECIS", kod = "bulten_gelir_grup_gecis", konu = "Kalkınma",
       baslik = "%10'luk gelir grubu geçişleri: başlangıç grubu ve 2025'teki değişim",
       oku = function() {
         capraz_oku(kir_oku(GY_GELIR, "geçiş oranları"), bas = 4:5, veri = c(6, NA), ad = "grup geçişi",
                    sutun = list(DEGISIM = list(tur = "sozluk", duzey = 2, sozluk = sz(
                      `_T` = "^Toplam", AZ2 = c("fazla azalan", "Bir gruptan fazla azalan"), AZ1 = c("bir grup azalan", "Bir grup azalan"),
                      SABIT = c("değişmeyen", "Değişmeyen"), ART1 = c("bir grup artan", "Bir grup artan"),
                      ART2 = c("fazla artan", "Bir gruptan fazla artan")))),
                    satir = list(BASLANGIC = list(sutun = 1, tur = "sozluk", sozluk = GY_G10)),
                    blok = list(DONEM = list(sutun = 1, tur = "sozluk", desen = GY_YIL_BLOK, sozluk = sz(
                      `2024_2025` = c("^2024$", "2024-2025"), `2023_2025` = c("^2023$", "2023-2025"),
                      `2022_2025` = c("^2022$", "2022-2025")))),
                    sabit = list(REF_AREA = "TR", TIME_PERIOD = "2025"))
       },
       boyut_ad = c(DONEM = "Panel dönemi", BASLANGIC = "Başlangıç gelir grubu (%10'luk)", DEGISIM = "2025'teki grup değişimi"),
       turkiye = "var", olcu = "oran", birim = "%", son_yil_sayisi = 1,
       aciklama = "Eşdeğer gelire göre %10'luk gruplarda başlangıç grubuna göre 2025'teki grup değişimi, Türkiye (satır yüzdesi; Gelir Dağılımı İstatistikleri 2025)",
       baglanti_df = ""),

  list(ad = "GELIR_EGITIM_IS_GELIRI", kod = "bulten_gelir_egitim_is_geliri", konu = "Kalkınma",
       baslik = "Yıllık ortalama esas iş geliri: eğitim düzeyi ve cinsiyet",
       oku = function() {
         capraz_oku(kir_oku(GY_GELIR, "eğitim düzeyine göre"), bas = 4:5, veri = c(6, NA), ad = "eğitim esas iş geliri",
                    sutun = list(CINSIYET = list(tur = "sozluk", duzey = 1, sozluk = CINSIYET_SZ),
                                 TIME_PERIOD = list(tur = "yil", duzey = 2)),
                    satir = list(EGITIM = list(sutun = 1, tur = "sozluk", sozluk = sz(
                      `_T` = c("^Tüm eğitim", "Tüm eğitim düzeyleri"), BITIRMEYEN = c("^Bir okul bitirmeyen", "Bir okul bitirmeyen"),
                      LISE_ALTI = c("^Lise altı", "Lise altı"), LISE = c("^Lise ve dengi", "Lise ve dengi okul"),
                      YUKSEKOGRETIM = c("^Yükseköğretim", "Yükseköğretim")))),
                    sabit = list(REF_AREA = "TR"))
       },
       boyut_ad = c(EGITIM = "Eğitim düzeyi", CINSIYET = "Cinsiyet"), turkiye = "var", olcu = "oran", birim = "TL", son_yil_sayisi = 2,
       aciklama = "Hanehalkı fertlerinin eğitim düzeyi ve cinsiyete göre yıllık ortalama esas iş geliri, Türkiye (2024, 2025)",
       baglanti_df = ""),

  list(ad = "GELIR_TURU_DAGILIM", kod = "bulten_gelir_turu_dagilim", konu = "Kalkınma",
       baslik = "Gelir türlerinin dağılımı: gelir türü ve %20'lik gelir grubu",
       oku = function() {
         capraz_oku(kir_oku(GY_GELIR, "gelir türlerinin"), bas = 4:6, veri = c(7, NA), ad = "gelir türleri",
                    sutun = list(GRUP = list(tur = "sozluk", duzey = 2, sozluk = GY_G20),
                                 TIME_PERIOD = list(tur = "yil", duzey = 3)),
                    satir = list(GELIR_TURU = list(sutun = 1, tur = "sozluk", sozluk = sz(
                      `_T` = "^Toplam", MAAS = c("^Maaş", "Maaş ve ücret"), YEVMIYE = c("^Yevmiye", "Yevmiye"),
                      MUTESEBBIS = c("^Müteşebbis", "Müteşebbis geliri"), TARIM_DISI = c("^Tarım dışı", "Tarım dışı"),
                      TARIM = c("^Tarım", "Tarım"), GAYRIMENKUL = c("^Gayrimenkul", "Gayrimenkul"),
                      MENKUL = c("^Menkul", "Menkul kıymet"), SOSYAL = c("^Sosyal transferler", "Sosyal transferler"),
                      EMEKLI = c("^Emekli", "Emekli ve dul-yetim aylıkları"), DIGER_SOSYAL = c("^Diğer sosyal", "Diğer sosyal transferler"),
                      HANELERARASI = c("^Hanelerarası", "Hanelerarası transferler"), DIGER = c("^Diğer gelirler", "Diğer gelirler")))),
                    sabit = list(REF_AREA = "TR"))
       },
       boyut_ad = c(GELIR_TURU = "Gelir türü", GRUP = "Gelir grubu (%20'lik)"), turkiye = "var", olcu = "oran", birim = "%", son_yil_sayisi = 2,
       aciklama = "Eşdeğer fert gelirine göre sıralı %20'lik gruplarda gelir türlerinin dikey dağılımı, Türkiye (2024, 2025); alt kalemler üst kalemin içindedir",
       baglanti_df = ""),

  # ---- yoksulluk -------------------------------------------------------------
  list(ad = "YOKSUL_BOLGE_AYRI_SAYI", kod = "bulten_yoksul_bolge_ayri_sayi", konu = "Kalkınma",
       baslik = "Göreli yoksul sayısı: bölge sınırına göre, eşik (bölge)",
       oku = function() gy_ayri("Yoksulluk sınırı|Yoksulluk oranı"),
       boyut_ad = c(ESIK = "Yoksulluk eşiği"), turkiye = "var", olcu = "oran", birim = "bin kişi", son_yil_sayisi = 11,
       aciklama = "Bölgeler için ayrı hesaplanan yoksulluk sınırına göre göreli yoksul sayısı (medyan gelirin %50 ve %60'ı), İBBS 2. düzey; her bölgenin sınırı farklı olduğundan bölge toplamı Türkiye değerine eşit değildir",
       baglanti_df = "BULTEN_IBBS2_IL"),

  list(ad = "YOKSUL_BOLGE_AYRI_ORAN", kod = "bulten_yoksul_bolge_ayri_oran", konu = "Kalkınma",
       baslik = "Göreli yoksulluk oranı: bölge sınırına göre, eşik (bölge)",
       oku = function() gy_ayri("Yoksulluk sınırı|Yoksul sayısı"),
       boyut_ad = c(ESIK = "Yoksulluk eşiği"), turkiye = "var", olcu = "oran", birim = "%", son_yil_sayisi = 11,
       aciklama = "Bölgeler için ayrı hesaplanan yoksulluk sınırına göre göreli yoksulluk oranı (medyan gelirin %50 ve %60'ı), İBBS 2. düzey",
       baglanti_df = "BULTEN_IBBS2_IL"),

  list(ad = "YOKSUL_BOLGE_ESIK_SAYI", kod = "bulten_yoksul_bolge_esik_sayi", konu = "Kalkınma",
       baslik = "Göreli yoksul sayısı: eşdeğer gelir ve eşik (bölge)",
       oku = function() gy_esik_bolge("Yoksulluk sınırı|Yoksulluk oranı"),
       boyut_ad = c(ESIK = "Yoksulluk eşiği"), turkiye = "var", olcu = "oran", birim = "bin kişi", son_yil_sayisi = 2,
       aciklama = "Eşdeğer hanehalkı kullanılabilir fert gelirine göre göreli yoksul sayısı (medyan gelirin %50 ve %60'ı), İBBS 2. düzey, 2024-2025; her bölgenin sınırı farklı olduğundan bölge toplamı Türkiye değerine eşit değildir",
       baglanti_df = "BULTEN_IBBS2_IL"),

  list(ad = "YOKSUL_BOLGE_ESIK_ORAN", kod = "bulten_yoksul_bolge_esik_oran", konu = "Kalkınma",
       baslik = "Göreli yoksulluk oranı: eşdeğer gelir ve eşik (bölge)",
       oku = function() gy_esik_bolge("Yoksulluk sınırı|Yoksul sayısı"),
       boyut_ad = c(ESIK = "Yoksulluk eşiği"), turkiye = "var", olcu = "oran", birim = "%", son_yil_sayisi = 2,
       aciklama = "Eşdeğer hanehalkı kullanılabilir fert gelirine göre göreli yoksulluk oranı (medyan gelirin %50 ve %60'ı), İBBS 2. düzey, 2024-2025",
       baglanti_df = "BULTEN_IBBS2_IL"),

  list(ad = "YOKSUL_TR_SINIR_SAYI", kod = "bulten_yoksul_tr_sinir_sayi", konu = "Kalkınma",
       baslik = "Göreli yoksul sayısı: Türkiye sınırına göre, eşik ve SGP (bölge)",
       # Türkiye değeri bölge toplamından yuvarlama farkıyla (en çok 3 bin kişi) ayrılır; Türkiye satırı atılıp toplamdan türetilir
       oku = function() { s <- gy_tr_sinir("Yoksulluk sınırı|Yoksulların oranı"); s$veri <- s$veri[s$veri$REF_AREA != "TR", ]; s },
       boyut_ad = c(ESIK = "Yoksulluk eşiği", SGP = "Satın alma gücü paritesi"), turkiye = "topla", olcu = "sayi", birim = "bin kişi",
       son_yil_sayisi = 11, toplanamaz = "ESIK;SGP",
       aciklama = "Türkiye için hesaplanan yoksulluk sınırına göre bölgelerdeki göreli yoksul sayısı; satın alma gücü paritesi (SGP) ile ve SGP olmadan",
       baglanti_df = "BULTEN_IBBS2_IL"),

  list(ad = "YOKSUL_TR_SINIR_PAY", kod = "bulten_yoksul_tr_sinir_pay", konu = "Kalkınma",
       baslik = "Yoksulların bölgelere dağılımı: Türkiye sınırı, eşik ve SGP",
       oku = function() gy_tr_sinir("Yoksulluk sınırı|Yoksul sayısı"),
       boyut_ad = c(ESIK = "Yoksulluk eşiği", SGP = "Satın alma gücü paritesi"), turkiye = "var", olcu = "oran", birim = "%",
       son_yil_sayisi = 11,
       aciklama = "Türkiye için hesaplanan yoksulluk sınırına göre yoksulların bölgelere dağılımı (Türkiye = 100); SGP ile ve SGP olmadan",
       baglanti_df = "BULTEN_IBBS2_IL"),

  list(ad = "YOKSUL_TR_ESIK_SAYI", kod = "bulten_yoksul_tr_esik_sayi", konu = "Kalkınma",
       baslik = "Göreli yoksul sayısı: eşik (Türkiye)",
       oku = function() gy_tr_esik("Yoksulluk sınırı|Yoksulluk oranı|Yoksulluk açığı"),
       boyut_ad = c(ESIK = "Yoksulluk eşiği"), turkiye = "var", olcu = "sayi", birim = "bin kişi", son_yil_sayisi = 2,
       toplanamaz = "ESIK",
       aciklama = "Eşdeğer hanehalkı kullanılabilir fert gelirine göre göreli yoksul sayısı, medyan gelirin %40, %50, %60 ve %70'i, Türkiye (2024-2025); eşikler iç içedir",
       baglanti_df = "BULTEN_TR_YOKSULLUK"),

  list(ad = "YOKSUL_TR_ESIK_ORAN", kod = "bulten_yoksul_tr_esik_oran", konu = "Kalkınma",
       baslik = "Göreli yoksulluk oranı: eşik (Türkiye)",
       oku = function() gy_tr_esik("Yoksulluk sınırı|Yoksul sayısı|Yoksulluk açığı"),
       boyut_ad = c(ESIK = "Yoksulluk eşiği"), turkiye = "var", olcu = "oran", birim = "%", son_yil_sayisi = 2,
       aciklama = "Göreli yoksulluk oranı, medyan gelirin %40, %50, %60 ve %70'i, Türkiye (2024-2025)",
       baglanti_df = "BULTEN_TR_YOKSULLUK"),

  list(ad = "YOKSUL_TR_ESIK_ACIK", kod = "bulten_yoksul_tr_esik_acik", konu = "Kalkınma",
       baslik = "Yoksulluk açığı: eşik (Türkiye)",
       oku = function() gy_tr_esik("Yoksulluk sınırı|Yoksul sayısı|Yoksulluk oranı"),
       boyut_ad = c(ESIK = "Yoksulluk eşiği"), turkiye = "var", olcu = "oran", birim = "%", son_yil_sayisi = 2,
       aciklama = "Yoksulluk açığı (yoksulların medyan gelirinin yoksulluk sınırından uzaklığı), medyan gelirin %40-%70'i, Türkiye (2024-2025)",
       baglanti_df = "BULTEN_TR_YOKSULLUK"),

  list(ad = "YOKSUL_PANEL_SURE", kod = "bulten_yoksul_panel_sure", konu = "Kalkınma",
       baslik = "Panel boyunca yoksullukta kalma süresi: eşik ve cinsiyet",
       oku = function() {
         capraz_oku(kir_oku(GY_YOKSUL, "panel süresince"), bas = 4:6, veri = c(7, NA), ad = "yoksullukta kalma süresi",
                    sutun = list(ESIK = list(tur = "sozluk", duzey = 1, sozluk = GY_ESIK),
                                 SURE = list(tur = "sozluk", sozluk = sz(
                                   S1 = c("[|] 1 Yıl$", "1 yıl"), S2 = c("[|] 2 Yıl$", "2 yıl"), S3 = c("[|] 3 Yıl$", "3 yıl"),
                                   S4 = c("[|] 4 Yıl$", "4 yıl"), S0 = c("Yoksul olmayan", "Yoksul olmayan")))),
                    satir = list(CINSIYET = list(sutun = 1, tur = "sozluk", sozluk = sz(`_T` = c("^Toplam", "Toplam"), E = c("^Erkek", "Erkek"), K = c("^Kadın", "Kadın")))),
                    sabit = list(REF_AREA = "TR", TIME_PERIOD = "2025"))
       },
       boyut_ad = c(SURE = "Yoksullukta kalma süresi", ESIK = "Yoksulluk eşiği", CINSIYET = "Cinsiyet"),
       turkiye = "var", olcu = "oran", birim = "%", son_yil_sayisi = 1,
       aciklama = "Dört yıllık panel boyunca yoksullukta kalma süresine göre kurumsal olmayan nüfusun dağılımı (%), Türkiye, 2025",
       baglanti_df = "BULTEN_TR_YOKSULLUK"),

  list(ad = "YOKSUL_HANETIPI_SAYI", kod = "bulten_yoksul_hanetipi_sayi", konu = "Kalkınma",
       baslik = "Göreli yoksul sayısı: hanehalkı tipi ve eşik",
       oku = function() gy_hhtip("Yoksulluk sınırı|Yoksulluk oranı"),
       boyut_ad = c(HHTIP = "Hanehalkı tipi", ESIK = "Yoksulluk eşiği"), turkiye = "var", olcu = "sayi", birim = "bin kişi",
       son_yil_sayisi = 11, toplanamaz = "HHTIP;ESIK",
       aciklama = "Hanehalkı tipine göre göreli yoksul sayısı (medyan gelirin %50 ve %60'ı), Türkiye; tek çekirdek aile alt tipleri onun içindedir",
       baglanti_df = "BULTEN_TR_YOKSULLUK"),

  list(ad = "YOKSUL_HANETIPI_ORAN", kod = "bulten_yoksul_hanetipi_oran", konu = "Kalkınma",
       baslik = "Göreli yoksulluk oranı: hanehalkı tipi ve eşik",
       oku = function() gy_hhtip("Yoksulluk sınırı|Yoksul sayısı"),
       boyut_ad = c(HHTIP = "Hanehalkı tipi", ESIK = "Yoksulluk eşiği"), turkiye = "var", olcu = "oran", birim = "%",
       son_yil_sayisi = 11,
       aciklama = "Hanehalkı tipine göre göreli yoksulluk oranı (medyan gelirin %50 ve %60'ı), Türkiye",
       baglanti_df = "BULTEN_TR_YOKSULLUK"),

  list(ad = "YOKSUL_SUREKLI_YOKSUNLUK", kod = "bulten_yoksul_surekli_yoksunluk", konu = "Kalkınma",
       baslik = "Sürekli yoksulluk ve maddi yoksunluk oranları",
       oku = function() {
         capraz_oku(kir_oku(GY_YOKSUL, "Sürekli yoksulluk"), bas = 3:4, veri = c(5, NA), ad = "sürekli yoksulluk",
                    sutun = list(TIME_PERIOD = list(tur = "yil", duzey = 2)),
                    satir = list(GOSTERGE = list(sutun = 1, tur = "sozluk", sozluk = sz(
                      SUREKLI = c("^Sürekli yoksulluk", "Sürekli yoksulluk oranı"), MADDI = c("^Maddi yoksunluk", "Maddi yoksunluk oranı"),
                      MADDI_SOSYAL = c("^Maddi ve sosyal", "Maddi ve sosyal yoksunluk oranı")))),
                    sabit = list(REF_AREA = "TR"))
       },
       boyut_ad = c(GOSTERGE = "Gösterge"), turkiye = "var", olcu = "oran", birim = "%", son_yil_sayisi = 11,
       aciklama = "Sürekli yoksulluk (medyan gelirin %60'ı), maddi yoksunluk, maddi ve sosyal yoksunluk oranları, Türkiye",
       baglanti_df = "BULTEN_TR_YOKSULLUK"),

  list(ad = "YOKSUL_YAS_RISKI", kod = "bulten_yoksul_yas_riski", konu = "Kalkınma",
       baslik = "Yoksulluk veya sosyal dışlanma riski: yaş grubu ve cinsiyet",
       oku = function() {
         capraz_oku(kir_oku(GY_YOKSUL, "Yaş gruplarına göre"), bas = 4:5, veri = c(6, NA), ad = "yaş grubu risk",
                    sutun = list(TIME_PERIOD = list(tur = "yil", duzey = 2)),
                    satir = list(YAS = list(sutun = 1, tur = "yas")),
                    blok = list(CINSIYET = list(sutun = 1, tur = "sozluk", sozluk = CINSIYET_SZ, desen = "^(Erkek|Kadın)", ilk = "Toplam")),
                    sabit = list(REF_AREA = "TR"))
       },
       boyut_ad = c(YAS = "Yaş grubu", CINSIYET = "Cinsiyet"), turkiye = "var", olcu = "oran", birim = "%", son_yil_sayisi = 11,
       aciklama = "Yoksulluk veya sosyal dışlanma riski (AROPE) altındakilerin oranı; yaş grubu ve cinsiyete göre, Türkiye",
       baglanti_df = "BULTEN_TR_YOKSULLUK")
))
