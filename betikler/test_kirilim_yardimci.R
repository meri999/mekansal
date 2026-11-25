.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "kirilim_yardimci.R"))

ok <- function(kosul, ad) { if (!isTRUE(kosul)) stop("KALDI: ", ad, call. = FALSE); cat("ok:", ad, "\n") }

# sirala
ok(identical(sirala(c("_T", "Y25T29", "Y5T9", "Y_GE60")), c("Y5T9", "Y25T29", "Y_GE60", "_T")), "sirala yaş grupları, _T sonda")
ok(identical(sirala(c("Y_GE60", "Y60T64", "Y_LT15", "Y15T17", "_U", "_T")), c("Y_LT15", "Y15T17", "Y60T64", "Y_GE60", "_U", "_T")), "sirala <, >= ve _U/_T")
ok(identical(sirala(c("11", "13", "2", "_T", "5", "_U")), c("11", "13", "2", "5", "_U", "_T")), "sirala yaş dışı: gelen sıra, _U,_T sonda")

# ortusen_var
ok(ortusen_var(c("Y15T19", "Y15T24", "Y25T29")), "örtüşen aralık")
ok(!ortusen_var(c("Y15T19", "Y20T24")), "örtüşmeyen aralık")
ok(ortusen_var(c("Y_GE15", "Y_GE6")), "iki Y_GE örtüşür")
ok(ortusen_var(c("_T", "2", "2_1", "3")), "hiyerarşik kod örtüşür")
ok(!ortusen_var(c("_T", "_U", "1", "2")), "düz kodlar örtüşmez")

# etiket_coz
et0 <- data.frame(df_id = character(), boyut = character(), kod = character(), kod_ad = character(), stringsAsFactors = FALSE)
r <- etiket_coz("DF_X", "YAS_GRUBU", c("Y20T24", "Y_GE60", "ZZ"), et0, NULL)
ok(identical(r$etiket, c("20-24", "60+", "ZZ")) && identical(r$eksik, "ZZ"), "etiket_coz yaş kodu ve eksik")
et1 <- data.frame(df_id = c("DF_A", "DF_B"), boyut = c("SEX", "SEX"), kod = c("1", "1"), kod_ad = c("Erkek", "Baska"), stringsAsFactors = FALSE)
ok(identical(etiket_coz("DF_B", "SEX", "1", et1, NULL)$etiket, "Baska"), "etiket önce tabloya özel")
ok(identical(etiket_coz("DF_C", "SEX", c("1", "_T", "_U"), et1, NULL)$etiket, c("Erkek", "Toplam", "Bilinmiyor")), "etiket başka tablodan, _T/_U")
dz <- data.frame(df_id = "DF_C", boyut = "SEX", kod = "1", kod_ad = "Düzeltilmiş", stringsAsFactors = FALSE)
ok(identical(etiket_coz("DF_C", "SEX", "1", et1, dz)$etiket, "Düzeltilmiş"), "elle düzeltme önce gelir")

# yogunlastir
ornek <- data.frame(REF_AREA = "TR", TIME_PERIOD = "2024",
  SEX = c("1", "1", "2", "2", "_T", "_T"), YAS = c("a", "b", "a", "b", "a", "b"),
  v = c(1, 2, 3, 4, 4, 6), stringsAsFactors = FALSE)
t <- yogunlastir(ornek, c("SEX", "YAS"), "REF_AREA")
ok(identical(t$v[["2024"]][["TR"]], c(1, 2, 3, 4, 4, 6)), "yogunlastir düz dizi")
ok(identical(t$kirilimlar[[1]]$kodlar, c("1", "2", "_T")), "yogunlastir kodlar")
ok(length(dogrula(t, "sayi")) == 0, "dogrula tutarlı")
t2 <- t; t2$v[["2024"]][["TR"]][5] <- 99
ok(length(dogrula(t2, "sayi")) == 1, "dogrula bozuk _T")
ok(length(dogrula(t2, "oran")) == 0, "dogrula oranda toplamaz")
ok(inherits(try(yogunlastir(rbind(ornek, ornek[1, ]), c("SEX", "YAS"), "REF_AREA"), silent = TRUE), "try-error"), "yinelenen gözlem")
eksik <- ornek[-2, ]
ok(is.na(yogunlastir(eksik, c("SEX", "YAS"), "REF_AREA")$v[["2024"]][["TR"]][2]), "eksik hücre NA")

# iç içe/seyrek kırılım: gerçek kodları tamamen boş dilim atlanır
tn <- t; tn$v[["2024"]][["TR"]] <- c(NA, NA, NA, NA, 4, 6)
ok(length(dogrula(tn, "sayi")) == 0, "dogrula: gerçek kodları boş dilim atlanır")
# yuvarlanmış yayın: tolerans yalnız yuvarlak=TRUE iken
ty <- t; ty$v[["2024"]][["TR"]] <- c(1, 2, 3, 4, 5, 6)
ok(length(dogrula(ty, "sayi")) == 1 && length(dogrula(ty, "sayi", yuvarlak = TRUE)) == 0, "dogrula yuvarlama toleransı")

# dönem etiketi
ok(identical(donem_etiketi(c("2024-Q3", "2024")), c("2024 Ç3", "2024")), "donem_etiketi çeyrek")

# il toplamı = Türkiye
il <- data.frame(REF_AREA = c("TR", "TR100", "TR211"), TIME_PERIOD = "2024", SEX = "1", v = c(10, 6, 4), stringsAsFactors = FALSE)
ti <- yogunlastir(il, "SEX", "REF_AREA")
ok(length(dogrula(ti, "sayi", "var")) == 0, "il toplamı = TR")
ti$v[["2024"]][["TR"]][1] <- 11
ok(length(dogrula(ti, "sayi", "var")) == 1, "il toplamı ≠ TR yakalanır")
cat("TÜMÜ GEÇTİ\n")
