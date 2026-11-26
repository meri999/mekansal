.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
d <- dirname(normalizePath(.f, winslash = "/"))
source(file.path(d, "etiket_yardimci.R"))

esit <- function(a, b, ad) {
  if (!identical(a, b)) { cat(sprintf("HATA %s: beklenen [%s], gelen [%s]\n", ad, b, a)); quit(status = 1) }
}

# Türkçe harf ve cümle düzeni
esit(tr_lower("İŞÇİ ILIK"), "işçi ılık", "tr_lower")
esit(cumle_duzeni("Ücretli Çalışanların Yıllık Ortalama Sayısı"), "Ücretli çalışanların yıllık ortalama sayısı", "cumle1")
esit(cumle_duzeni("İş, Yönetim ve Hukuk"), "İş, yönetim ve hukuk", "cumle2")
esit(cumle_duzeni("Karbon Dioksit (CO2)"), "Karbon dioksit (CO2)", "kisaltma1")
esit(cumle_duzeni("Doğrudan Madde Girdisi (DMI) = (DE) + (I)"), "Doğrudan madde girdisi (DMI) = (DE) + (I)", "kisaltma2")
esit(cumle_duzeni("Azot Oksitler (NOx)"), "Azot oksitler (NOx)", "kisaltma3")
esit(cumle_duzeni("İnsan Sağlığı (Rdna Teknolojisi Kullanılan)"), "İnsan sağlığı (rDNA teknolojisi kullanılan)", "rdna")
esit(cumle_duzeni("Girişimin Kendi Web Sitesi Ya Da Uygulaması Üzerinden"), "Girişimin kendi web sitesi ya da uygulaması üzerinden", "yada")
esit(cumle_duzeni("Hanehalkı sayısı"), "Hanehalkı sayısı", "zaten_cumle")
esit(cumle_duzeni("Türkiye Ortalaması Nedir"), "Türkiye ortalaması nedir", "korunan")

# Birim parantezleri ve cinsiyet öneki
esit(kirilim_temizle("Maaş ve ücretler (TL)"), "Maaş ve ücretler", "tl")
esit(kirilim_temizle("Girişim Oranı(%), Düşük teknoloji"), "Girişim oranı, düşük teknoloji", "oran")
esit(kirilim_temizle("Kaba Doğum Hızı (Binde)"), "Kaba doğum hızı", "binde")
esit(kirilim_temizle("Çocuk bağımlılık oranı % (0-14 yaş)"), "Çocuk bağımlılık oranı (0-14 yaş)", "bare_yuzde")
esit(kirilim_temizle("cinsiyet: Erkek"), "Erkek", "cinsiyet")
esit(kirilim_temizle(""), "", "bos")

# Başlık kurma kipleri
S <- function(...) as.list(data.frame(..., stringsAsFactors = FALSE))
esit(baslik_kur("Hizmetler", "T", S(mod = "O", olcu = "Yükseköğretim mezunlarında beyin göçü oranı", toplam = "")),
     "Yükseköğretim mezunlarında beyin göçü oranı · Hizmetler", "O")
esit(baslik_kur("", "T", S(mod = "O", olcu = "Nüfus", toplam = "")), "Nüfus", "O_toplam")
esit(baslik_kur("cinsiyet: Kadın", "T", S(mod = "O", olcu = "Ölüm sayısı", toplam = "")), "Ölüm sayısı · Kadın", "O_cinsiyet")
esit(baslik_kur("15-19", "T", S(mod = "O", olcu = "Yaşa özel doğurganlık hızı", toplam = "")), "Yaşa özel doğurganlık hızı · 15-19 yaş", "O_yas")
esit(baslik_kur("İmalat", "T", S(mod = "O", olcu = "İmalat sanayi atığı", toplam = "^İmalat$")), "İmalat sanayi atığı", "O_toplam_regex")
esit(baslik_kur("Girişim Sayısı, Düşük teknoloji", "T", S(mod = "P", olcu = "İmalatta doğan girişimler", toplam = "")),
     "İmalatta doğan girişimler · Düşük teknoloji (girişim sayısı)", "P")
esit(baslik_kur("İstihdam, Doğan Girişimler", "T", S(mod = "R", olcu = "", toplam = "")), "Doğan girişimler · istihdam", "R")
esit(baslik_kur("", "Resmi İlan Bedelleri Tablosu Genel", S(mod = "S", olcu = "Resmi ilan bedeli", toplam = "")), "Resmi ilan bedeli", "S")
esit(baslik_kur("", "Belediye Atıksu Arıtma Çamuru İstatistikleri", NULL), "Belediye atıksu arıtma çamuru istatistikleri", "tablo_adi_E")
esit(baslik_kur("Ücretli Çalışan Kadın Sayısı", "T", NULL), "Ücretli çalışan kadın sayısı", "E")

cat("etiket_yardimci testleri geçti\n")
