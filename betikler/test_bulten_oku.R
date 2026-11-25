.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "05g_bulten_il_oku.R"))
source(file.path(dirname(normalizePath(.f, winslash = "/")), "05i_bulten_tr_oku.R"))
source(file.path(dirname(normalizePath(.f, winslash = "/")), "05h_bulten_ibbs2_oku.R"))

ok <- function(kosul, ad) { if (!isTRUE(kosul)) stop("KALDI: ", ad, call. = FALSE); cat("ok:", ad, "\n") }
yakin <- function(a, b, tol = 1e-9) isTRUE(abs(a - b) < tol)
deger <- function(d, il_ad, gosterge, donem) {
  kod <- il_kodu(il_ad, YER, TAKMA)
  d$OBS_VALUE[d$REF_AREA == kod & d$GOSTERGE == gosterge & d$TIME_PERIOD == donem]
}

# --- Görev 2: iç göç ---
ig <- oku_ic_goc()
ok(all(c("REF_AREA", "TIME_PERIOD", "GOSTERGE", "OBS_VALUE") %in% names(ig)), "ic_goc sütunlar")
ok(yakin(deger(ig, "Adana", "NET_GOC_HIZI", "2025"), -2.745820102210628), "ic_goc Adana net göç hızı")
ok(deger(ig, "Adana", "ALDIGI_GOC", "2025") == 49680, "ic_goc Adana aldığı göç")
ok(deger(ig, "Adana", "VERDIGI_GOC", "2025") == 55959, "ic_goc Adana verdiği göç")
ok(yakin(deger(ig, "Ağrı", "NET_GOC_HIZI", "2025"), -26.990491005344104, 1e-6), "ic_goc Ağrı net göç hızı")
ok(yakin(deger(ig, "Antalya", "NET_GOC_HIZI", "2025"), 10.268906541298533, 1e-6), "ic_goc Antalya net göç hızı")
ok(length(unique(ig$REF_AREA)) == 81, "ic_goc 81 il")
ok(sum(ig$OBS_VALUE[ig$GOSTERGE == "NET_GOC"]) == 0, "ic_goc net göç toplamı 0")
ok(!any(ig$REF_AREA == "TR"), "ic_goc Toplam satırı il değil")

# --- Görev 3: yapı izin ---
yi <- oku_yapi_izin()
ok(all(c("RUHSAT_DAIRE", "KULLANMA_DAIRE") %in% yi$GOSTERGE), "yapı izin iki gösterge")
ok(deger(yi, "Adana", "RUHSAT_DAIRE", "2010") == 11462, "yapı izin Adana 2010 ruhsat")
ok(deger(yi, "Ankara", "RUHSAT_DAIRE", "2010") == 70320, "yapı izin Ankara 2010 ruhsat")
ok(deger(yi, "Antalya", "RUHSAT_DAIRE", "2010") == 27433, "yapı izin Antalya 2010 ruhsat")
ok(sum(yi$OBS_VALUE[yi$GOSTERGE == "RUHSAT_DAIRE" & yi$TIME_PERIOD == "2010"]) == 909746, "yapı izin 2010 il toplamı = Toplam sütunu (ruhsat)")
ok(sum(yi$OBS_VALUE[yi$GOSTERGE == "KULLANMA_DAIRE" & yi$TIME_PERIOD == "2010"]) == 430333, "yapı izin 2010 il toplamı (kullanma izni)")
ok(!"2026" %in% yi$TIME_PERIOD, "yapı izin yarım 2026 yıllık seride yok")
ok(identical(sort(unique(yi$TIME_PERIOD)), as.character(2010:2025)), "yapı izin yıllar 2010-2025")
ok(length(unique(yi$REF_AREA)) == 81, "yapı izin 81 il")

# --- Görev 4: sinema ---
sn <- oku_sinema()
ok(setequal(unique(sn$GOSTERGE), c("SALON", "KOLTUK", "FILM", "SEYIRCI")), "sinema dört gösterge")
ok(deger(sn, "Adana", "SALON", "2025") == 24, "sinema Adana salon")
ok(deger(sn, "Adana", "KOLTUK", "2025") == 5879, "sinema Adana koltuk")
ok(deger(sn, "Adana", "FILM", "2025") == 362, "sinema Adana film")
ok(deger(sn, "Adana", "SEYIRCI", "2025") == 561072, "sinema Adana seyirci")
ok(sn$OBS_VALUE[sn$REF_AREA == "TR" & sn$GOSTERGE == "SALON"] == 2161, "sinema Türkiye salon")
ok(sn$OBS_VALUE[sn$REF_AREA == "TR" & sn$GOSTERGE == "SEYIRCI"] == 27657591, "sinema Türkiye seyirci")
ok(!any(sn$REF_AREA == "TR" & sn$GOSTERGE == "FILM"), "sinema Türkiye film '-' satırı üretilmez")
ok(sum(sn$OBS_VALUE[sn$GOSTERGE == "SALON" & sn$REF_AREA != "TR"]) == 2161, "sinema il salon toplamı = Türkiye")

# --- Görev 5: beşeri kalkınma ---
bk <- oku_beseri_kalkinma()
ok(setequal(unique(bk$GOSTERGE), c("ENDEKS", "YASAM_ENDEKSI", "EGITIM_ENDEKSI", "GELIR_ENDEKSI")), "beşeri kalkınma dört gösterge")
ok(yakin(bk$OBS_VALUE[bk$REF_AREA == "TR" & bk$GOSTERGE == "ENDEKS" & bk$TIME_PERIOD == "2018"], 0.8443453743646472, 1e-9), "beşeri kalkınma Türkiye 2018")
ok(yakin(deger(bk, "Adana", "ENDEKS", "2018"), 0.82109198424884, 1e-9), "beşeri kalkınma Adana 2018 endeks")
ok(yakin(deger(bk, "Adana", "YASAM_ENDEKSI", "2018"), 0.89339539351016, 1e-9), "beşeri kalkınma Adana 2018 yaşam endeksi")
ok(identical(sort(unique(bk$TIME_PERIOD)), as.character(2018:2022)), "beşeri kalkınma yıllar 2018-2022")
ok(length(deger(bk, "Adana", "ENDEKS", "2019")) == 1, "beşeri kalkınma yıl aşağı doldurulmuş (2019 Adana)")
ok(all(tapply(bk$REF_AREA[bk$GOSTERGE == "ENDEKS" & bk$REF_AREA != "TR"], bk$TIME_PERIOD[bk$GOSTERGE == "ENDEKS" & bk$REF_AREA != "TR"], function(z) length(unique(z))) == 81), "beşeri kalkınma her yıl 81 il")

# --- Görev 6: uluslararası göç il ---
ug <- oku_ugoc_il()
ok(setequal(unique(ug$GOSTERGE), c("GELEN", "GIDEN", "NET", "GELEN_YABANCI", "GIDEN_YABANCI")), "ugoc beş gösterge")
ok(identical(sort(unique(ug$TIME_PERIOD)), c("2024", "2025")), "ugoc iki yıl: 2024, 2025")
tr_gelen <- ug$OBS_VALUE[ug$REF_AREA == "TR" & ug$GOSTERGE == "GELEN" & ug$TIME_PERIOD == "2025"]
tr_giden <- ug$OBS_VALUE[ug$REF_AREA == "TR" & ug$GOSTERGE == "GIDEN" & ug$TIME_PERIOD == "2025"]
ok(tr_gelen == 393829 && tr_giden == 403216, "ugoc Türkiye gelen/giden 2025 (bülten metni)")
ok(ug$OBS_VALUE[ug$REF_AREA == "TR" & ug$GOSTERGE == "GELEN" & ug$TIME_PERIOD == "2024"] == 314588, "ugoc Türkiye gelen 2024")
ok(sum(ug$OBS_VALUE[ug$GOSTERGE == "GELEN" & ug$REF_AREA != "TR" & ug$TIME_PERIOD == "2025"]) == 393829, "ugoc il gelen toplamı = Türkiye (2025)")
ok(sum(ug$OBS_VALUE[ug$GOSTERGE == "GIDEN" & ug$REF_AREA != "TR" & ug$TIME_PERIOD == "2025"]) == 403216, "ugoc il giden toplamı = Türkiye (2025)")
ok(abs(deger(ug, "İstanbul", "GELEN", "2025") / tr_gelen - 0.422) < 0.001, "ugoc İstanbul gelen payı %42,2 (bülten metni)")
ok(deger(ug, "Adana", "GELEN", "2025") == 4199 && deger(ug, "Adana", "GIDEN", "2025") == 5846, "ugoc Adana gelen/giden")
ok(deger(ug, "Adana", "NET", "2025") == -1647, "ugoc Adana net")
ok(deger(ug, "Adana", "GELEN_YABANCI", "2025") == 2315 && deger(ug, "Adana", "GIDEN_YABANCI", "2025") == 2314, "ugoc Adana yabancı gelen/giden")
ok(length(unique(ug$REF_AREA[ug$REF_AREA != "TR"])) == 81, "ugoc 81 il")

# --- Görev 9: Türkiye düzeyli ---
tr <- function(d, gosterge, donem) d$OBS_VALUE[d$GOSTERGE == gosterge & d$TIME_PERIOD == donem]
dt <- oku_dis_ticaret()
ok(all(dt$REF_AREA == "TR"), "dış ticaret yalnız Türkiye")
ok(yakin(tr(dt, "IHRACAT_YILLIK", "2013"), 161.480914702), "dış ticaret 2013 ihracat (milyar $)")
ok(yakin(tr(dt, "IHRACAT_YILLIK", "2025"), 273.233729847), "dış ticaret 2025 ihracat")
ok(yakin(tr(dt, "ITHALAT_YILLIK", "2025"), 365.40630125), "dış ticaret 2025 ithalat")
ok(yakin(tr(dt, "DENGE_YILLIK", "2025"), tr(dt, "IHRACAT_YILLIK", "2025") - tr(dt, "ITHALAT_YILLIK", "2025"), 1e-6), "dış ticaret denge = ihracat - ithalat")
ok(yakin(tr(dt, "KARSILAMA_YILLIK", "2025"), 74.77531, 1e-4), "dış ticaret 2025 karşılama oranı")
ok(yakin(tr(dt, "IHRACAT_AYLIK", "2025-01"), 21.16001938), "dış ticaret 2025-01 ihracat")
ok(yakin(tr(dt, "IHRACAT_AYLIK", "2026-09"), 25.966598771), "dış ticaret 2026-09 ihracat")
ok(yakin(tr(dt, "IHRACAT_AYLIK", "2026-01"), 20.330300329), "dış ticaret 2026-01 '(r)' yıl hücresi okunur")
ok(yakin(sum(dt$OBS_VALUE[dt$GOSTERGE == "IHRACAT_AYLIK" & grepl("^2025-", dt$TIME_PERIOD)]), tr(dt, "IHRACAT_YILLIK", "2025"), 1e-3), "dış ticaret 2025 aylık toplam = yıllık")
ok(!"2026-10" %in% dt$TIME_PERIOD, "dış ticaret boş 2026-10 üretilmez")

hy <- oku_hayat_tablosu()
ok(yakin(tr(hy, "BEKLENEN_YASAM_TOPLAM", "2025"), 78.49498405119255, 1e-9), "hayat 2023-2025 toplam beklenen yaşam")
ok(yakin(tr(hy, "BEKLENEN_YASAM_ERKEK", "2025"), 75.87128211504941, 1e-9), "hayat erkek")
ok(yakin(tr(hy, "BEKLENEN_YASAM_KADIN", "2025"), 81.14415234748841, 1e-9), "hayat kadın")
ok(length(tr(hy, "BEKLENEN_YASAM_TOPLAM", "2024")) == 1, "hayat 2022-2024 dönemi de var")
ok(tr(hy, "BEKLENEN_YASAM_KADIN", "2025") > tr(hy, "BEKLENEN_YASAM_ERKEK", "2025"), "hayat kadın > erkek")

km <- oku_kumes()
ok(setequal(unique(km$GOSTERGE), c("TAVUK_YUMURTASI", "KESILEN_TAVUK", "TAVUK_ETI", "KESILEN_HINDI", "HINDI_ETI")), "kümes beş ürün")
ok(yakin(tr(km, "TAVUK_YUMURTASI", "2024-01"), 1838422.306), "kümes 2024-01 yumurta")
ok(yakin(tr(km, "TAVUK_YUMURTASI", "2026-09"), 2076674.462), "kümes 2026-09 yumurta ('(r)' sütunu atlanır)")
ok(yakin(sum(km$OBS_VALUE[km$GOSTERGE == "TAVUK_YUMURTASI" & grepl("^2024-", km$TIME_PERIOD)]), 21155089.454, 1e-2), "kümes 2024 aylık toplam = yıllık toplam")

# --- Final inceleme düzeltmeleri ---
# #1 uluslararası göç vatandaşlık tablosu: sütun başlıkları ve tutarlılık doğrulanır
vt <- ugoc_tablo("İllere ve vatandaşlığa göre")
ok(isTRUE(ugoc_vatandaslik_dogrula(vt$d)), "ugoc vatandaşlık doğrulaması gerçek dosyada geçer")
b1 <- vt$d; b1[5, c(8, 9)] <- b1[5, c(9, 8)]
ok(inherits(tryCatch(ugoc_vatandaslik_dogrula(b1), error = function(e) e), "error"), "ugoc Türk/yabancı sütunları yer değiştirirse durur")
b2 <- vt$d[, -10]
ok(inherits(tryCatch(ugoc_vatandaslik_dogrula(b2), error = function(e) e), "error"), "ugoc boş ara sütun kalkarsa durur")

# #2 dış ticaret: yıllık blok başlık satırından sonra başlar, tanınmayan etiket durdurur
dt_d <- bulten_oku(bulten_dosya("dis_ticaret_2026_09", "yıllara ve aylara göre dış ticaret"))
bos_satir <- dt_d[1, ]; bos_satir[] <- NA_character_
kaymis <- rbind(dt_d[1:7, ], bos_satir, dt_d[8:nrow(dt_d), ])
ok(identical(dis_ticaret_ayristir(kaymis), dis_ticaret_ayristir(dt_d)), "dış ticaret başlık bir satır kayınca aynı sonuç")
ok(length(tr(dis_ticaret_ayristir(kaymis), "IHRACAT_YILLIK", "2013")) == 1, "dış ticaret kayan başlıkta yıllık 2013 düşmez")
garip <- dt_d[9, ]; garip[1, 1] <- "Garip etiket"
ok(inherits(tryCatch(dis_ticaret_ayristir(rbind(dt_d[1:8, ], garip, dt_d[9:nrow(dt_d), ])), error = function(e) e), "error"), "dış ticaret tanınmayan etiket durdurur")

# #4 iç göç: sütun başlıkları ve aldığı - verdiği = net
d_ig <- bulten_oku(bulten_dosya("ic_goc_2025", "net göç hızı"))
ig_baslik <- which(grepl("^İl", trimws(d_ig[[1]])) & grepl("Province", d_ig[[1]]))[1]
ok(isTRUE(ic_goc_dogrula(d_ig, ig_baslik)), "ic_goc doğrulaması gerçek dosyada geçer")
b3 <- d_ig; b3[ig_baslik, c(3, 4)] <- b3[ig_baslik, c(4, 3)]
ok(inherits(tryCatch(ic_goc_dogrula(b3, ig_baslik), error = function(e) e), "error"), "ic_goc aldığı/verdiği başlığı yer değiştirirse durur")
b4 <- d_ig; b4[ig_baslik + 2, 5] <- "999999"
ok(inherits(tryCatch(ic_goc_dogrula(b4, ig_baslik), error = function(e) e), "error"), "ic_goc aldığı - verdiği != net ise durur")

# #5 il sayısı kontrolü
ok(inherits(tryCatch(il_say_dogrula(data.frame(REF_AREA = sprintf("TR%03d", 1:80)), "deneme"), error = function(e) e), "error"), "il_say_dogrula 80 ilde durur")
ok(isTRUE(il_say_dogrula(data.frame(REF_AREA = c("TR", sprintf("TR%03d", 1:81))), "deneme")), "il_say_dogrula 81 il + TR geçer")

# #3 ve #7: gösterge tanımları
gl <- read.csv(file.path(PORTAL, "ayar/gostergeler_il.csv"), fileEncoding = "UTF-8", stringsAsFactors = FALSE, na.strings = character())
yan <- gl[gl$df_id == "BULTEN_IC_GOC_IL" & gl$filtre %in% c("GOSTERGE=ALDIGI_GOC", "GOSTERGE=VERDIGI_GOC"), ]
ok(nrow(yan) == 2 && all(!nzchar(yan$bolge)), "aldığı/verdiği göç bölge toplamı alınmaz (bölge içi göç çift sayılır)")
hz <- gl[gl$df_id == "BULTEN_IC_GOC_IL" & gl$filtre == "GOSTERGE=NET_GOC_HIZI", ]
ok(nrow(hz) == 1 && hz$degisim == "fark" && hz$iyi == "notr", "net göç hızı: değişim 'fark', yargısız renk")

# --- Görev 8: İBBS-2 (gelir ve yoksulluk) ---
ib <- oku_ibbs2()
ib_d <- function(kod, g, y) ib$OBS_VALUE[ib$REF_AREA == kod & ib$GOSTERGE == g & ib$TIME_PERIOD == y]
ok(setequal(unique(ib$GOSTERGE), c("YOKSULLUK_DISLANMA_RISKI", "GINI", "P80_P20", "GORELI_YOKSULLUK")), "ibbs2 dört gösterge")
ok(ib_d("TR", "GORELI_YOKSULLUK", "2025") == 12.99 && ib_d("TR10", "GORELI_YOKSULLUK", "2025") == 9.89, "ibbs2 göreli yoksulluk %50: Türkiye ve TR10 2025")
ok(ib_d("TR10", "YOKSULLUK_DISLANMA_RISKI", "2025") == 16.62, "ibbs2 TR10 yoksulluk/dışlanma riski 2025")
ok(ib_d("TR", "YOKSULLUK_DISLANMA_RISKI", "2025") == 27.86, "ibbs2 Türkiye yoksulluk/dışlanma riski 2025")
ok(ib_d("TR10", "GINI", "2014") == 0.367, "ibbs2 TR10 Gini 2014")
ok(ib_d("TR", "GINI", "2025") == 0.41, "ibbs2 Türkiye Gini 2025")
ok(all(tapply(ib$REF_AREA, ib$GOSTERGE, function(z) length(unique(z))) == 27), "ibbs2 her gösterge 26 bölge + Türkiye")
ok(identical(sort(unique(ib$TIME_PERIOD[ib$GOSTERGE == "GINI"])), as.character(2014:2025)), "ibbs2 Gini yılları 2014-2025")
ibil <- ibbs2_il_a_yay(ib)
ok(ib_d("TR21", "GINI", "2014") == ibil$OBS_VALUE[ibil$REF_AREA == il_kodu("Kırklareli", YER, TAKMA) & ibil$GOSTERGE == "GINI" & ibil$TIME_PERIOD == "2014"], "ibbs2 il genişletmesi: Kırklareli = TR21 bölge değeri")
ok(length(setdiff(unique(ibil$REF_AREA), "TR")) == 81, "ibbs2 il genişletmesi 81 il")
ok(ibil$OBS_VALUE[ibil$REF_AREA == "TR" & ibil$GOSTERGE == "GINI" & ibil$TIME_PERIOD == "2025"] == 0.41, "ibbs2 il genişletmesi Türkiye satırı korunur")

# --- Türkiye düzeyli sinema ve tiyatro serileri ---
sy <- oku_sinema_tr()
ok(identical(sort(unique(sy$TIME_PERIOD)), as.character(2000:2025)), "sinema TR yıllar 2000-2025")
ok(tr(sy, "SALON", "2000") == 606 && tr(sy, "KOLTUK", "2000") == 200853, "sinema TR 2000 salon, koltuk")
ok(tr(sy, "FILM", "2000") == 24096 && tr(sy, "SEYIRCI", "2000") == 17086152, "sinema TR 2000 film, seyirci")
ok(tr(sy, "SALON", "2025") == 2161 && tr(sy, "SEYIRCI", "2025") == 27657591, "sinema TR 2025 = il tablosundaki Türkiye satırı")
ok(tr(sy, "FILM_YERLI", "2000") + tr(sy, "FILM_YABANCI", "2000") == tr(sy, "FILM", "2000"), "sinema TR yerli + yabancı film = toplam (2000)")
ty <- oku_tiyatro_tr()
ok(identical(sort(unique(ty$TIME_PERIOD)), as.character(2000:2025)), "tiyatro sezon yılı bitiş yılı: 1999/'00 -> 2000 ... 2024/'25 -> 2025")
ok(tr(ty, "SALON", "2000") == 108 && tr(ty, "ESER", "2000") == 735 && tr(ty, "GOSTERI", "2000") == 11215, "tiyatro 1999/'00 salon, eser, gösteri")
ok(tr(ty, "SALON", "2025") == 1101 && tr(ty, "ESER", "2025") == 10216, "tiyatro 2024/'25 salon, eser")

# --- tiyatro il, göç nedeni, Türkiye yoksulluk/hayvan/bitkisel/yapı izin ---
ti <- oku_tiyatro_il()
adana <- il_kodu("Adana", YER, TAKMA)
td <- function(g) ti$OBS_VALUE[ti$REF_AREA == adana & ti$GOSTERGE == g]
ok(td("SALON") == 25 && td("KOLTUK") == 14603 && td("ESER") == 164 && td("GOSTERI") == 1746, "tiyatro il Adana salon, koltuk, eser, gösteri")
ok(td("SEYIRCI") >= 67431 + 99808, "tiyatro il Adana seyirci en az devlet + özel")
gn <- oku_goc_nedeni()
gd <- function(g) gn$OBS_VALUE[gn$REF_AREA == adana & gn$GOSTERGE == g]
ok(abs(gd("ALDIGI_IS_PAYI") - 100 * (5999 + 3524) / 49680) < 1e-9, "göç nedeni Adana aldığı göçte iş payı")
ok(all(gn$OBS_VALUE >= 0 & gn$OBS_VALUE <= 100), "göç nedeni payları 0-100")
yk <- oku_yoksulluk_tr()
ok(tr(yk, "SUREKLI_YOKSULLUK", "2025") == 13.63 && tr(yk, "MADDI_SOSYAL_YOKSUNLUK", "2025") == 11.85, "yoksulluk TR 2025 değerleri")
ok(!"2006" %in% yk$TIME_PERIOD[yk$GOSTERGE == "SUREKLI_YOKSULLUK"], "yoksulluk TR dipnotlu '-(1)' hücreler boş kalır")
hv <- oku_hayvan_sayilari()
ok(tr(hv, "TOPLAM", "2025-12") == 75723815 && tr(hv, "KOYUN", "2026-06") == 49473446, "hayvan sayıları Aralık 2025 toplam, Haziran 2026 koyun")
ok(tr(hv, "BUYUKBAS", "2025-12") + tr(hv, "KUCUKBAS", "2025-12") < tr(hv, "TOPLAM", "2025-12"), "hayvan büyükbaş + küçükbaş < toplam")
bt <- oku_bitkisel()
ok(tr(bt, "BUGDAY", "2025") == 17950000 && tr(bt, "BUGDAY", "2026") == 24750000, "bitkisel buğday 2025, 2026 (ton)")
yi <- oku_yapi_izin_tr()
ok(tr(yi, "RUHSAT_DAIRE", "2025-Q1") == 169509 && tr(yi, "KULLANMA_BINA", "2024-Q1") == 30927, "yapı izin TR çeyrek değerleri")

cat("OK\n")
