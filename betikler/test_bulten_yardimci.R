.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))
source(file.path(dirname(normalizePath(.f, winslash = "/")), "bulten_yardimci.R"))

ok <- function(kosul, ad) { if (!isTRUE(kosul)) stop("KALDI: ", ad, call. = FALSE); cat("ok:", ad, "\n") }

yer <- read.csv(file.path(PORTAL, "ayar/yer_adlari.csv"), fileEncoding = "UTF-8", stringsAsFactors = FALSE)
takma <- read.csv(file.path(PORTAL, "ayar/il_ad_takma.csv"), fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# il_kodu
ok(identical(il_kodu("Adana", yer, takma), yer$kod[yer$ad_tr == "Adana"]), "il_kodu Adana")
ok(!anyNA(il_kodu(c("Adana", "Ağrı", "Afyonkarahisar", "Şanlıurfa", "Iğdır"), yer, takma)), "il_kodu Türkçe harfli adlar")
ok(is.na(il_kodu("Toplam-Total", yer, takma)), "il_kodu Toplam -> NA")
ok(identical(il_kodu("Adana Province", yer, takma), il_kodu("Adana", yer, takma)), "il_kodu çift dilli ek")
ok(identical(il_kodu("  Adana \n", yer, takma), il_kodu("Adana", yer, takma)), "il_kodu boşluk ve satır sonu")

# ibbs2_kodu
ok(ibbs2_kodu("TR10 (İstanbul)") == "TR10", "ibbs2_kodu bölge")
ok(ibbs2_kodu("TR Türkiye ") == "TR", "ibbs2_kodu Türkiye")
ok(is.na(ibbs2_kodu("Yıllar - Years")), "ibbs2_kodu başlık -> NA")

# ibbs2_il_eslemesi
e <- ibbs2_il_eslemesi(yer)
ok(nrow(e) == 81, "ibbs2_il_eslemesi 81 il")
ok(length(unique(e$ibbs2)) == 26, "ibbs2_il_eslemesi 26 bölge")
ok(e$ibbs2[e$il == "TR100"] == "TR10", "ibbs2_il_eslemesi İstanbul -> TR10")

# uzun_yaz
gecici <- tempfile(fileext = ".csv")
uzun_yaz(data.frame(REF_AREA = "TR100", TIME_PERIOD = "2025", GOSTERGE = "A", OBS_VALUE = 1), gecici)
ok(file.exists(gecici), "uzun_yaz yazar")
yinelenen <- data.frame(REF_AREA = c("TR100", "TR100"), TIME_PERIOD = "2025", GOSTERGE = "A", OBS_VALUE = c(1, 2))
ok(inherits(tryCatch(uzun_yaz(yinelenen, gecici), error = function(e) e), "error"), "uzun_yaz yineleneni reddeder")

# bulten_dosya
ok(file.exists(bulten_dosya("ic_goc_2025", "net göç hızı")), "bulten_dosya bulur")
ok(inherits(tryCatch(bulten_dosya("ic_goc_2025", "bu_dosya_yok"), error = function(e) e), "error"), "bulten_dosya yoksa hata")
uzun <- bulten_dosya("gelir_dagilimi_2025", "yüzde 10'luk gruplar itibarıyla fertlerin önceki yıllara")
ok(file.exists(uzun) && nchar(uzun) < 250, "bulten_dosya uzun yolu kısaltır")

# bulten_oku
d <- bulten_oku(bulten_dosya("ic_goc_2025", "net göç hızı"))
ok(is.data.frame(d) && nrow(d) > 80 && all(vapply(d, is.character, TRUE)), "bulten_oku karakter sütunlu data.frame")
ok(any(trimws(d[[1]]) == "Adana"), "bulten_oku Adana satırı")

cat("OK\n")
