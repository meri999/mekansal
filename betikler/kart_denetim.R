.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))

# Sitedeki TÜM kartları (gösterge) tek tek etiket sorunlarına göre işaretler.
# Çıktı: veri/kart_denetim.csv (kart başına sorun etiketleri) ve özet tablosu.
# Kullanım: Rscript betikler/kart_denetim.R [site]   (varsayılan docs)

suppressMessages(library(jsonlite))
arg <- commandArgs(TRUE)
site <- if (length(arg)) arg[1] else "docs"
G <- fromJSON(file.path(PORTAL, site, "veri", "veri.json"), simplifyVector = FALSE)$gostergeler

son_deger <- function(g) {
  v <- unlist(g$veri[["TR"]]); if (is.null(v)) v <- unlist(g$veri[[1]])
  if (length(v)) tail(v, 1) else NA
}
imza <- function(g) {
  v <- unlist(g$veri[["TR"]]); if (is.null(v)) return(NA_character_)
  paste(round(v, 3), collapse = ",")
}

d <- do.call(rbind, lapply(G, function(g) data.frame(
  id = g$id, konu = g$konu, grup = g$grup, baslik = g$baslik,
  not = if (is.null(g$not)) "" else g$not,
  birim = if (is.null(g$birim)) "" else g$birim,
  tablo = sub("^DF_", "", g$df_id), oto = isTRUE(g$oto), tr = isTRUE(g$sadece_tr),
  son = son_deger(g), imza = imza(g), stringsAsFactors = FALSE)))

b <- d$baslik; u <- d$birim; bl <- tolower(b)
ol <- function(...) grepl(paste(..., sep = "|"), bl, perl = TRUE)
# Birim denetimi yalnız ETİKET üzerinde (başlığın " · " öncesi); tablo adı eşleşmeye girmez.
el <- tolower(sub(" · .*$", "", b))
# Çıplak "Erkek/Kadın" etiketlerinde ölçü " · " sonrasındadır.
cip <- grepl("^(cinsiyet: )?(erkek|kadın)$", el)
el[cip] <- tolower(sub("^.* · ", "", b[cip]))
ou <- function(...) grepl(paste(..., sep = "|"), el, perl = TRUE)

etiket <- list()
ekle <- function(ad, ix) { for (i in which(ix)) etiket[[i]] <<- c(etiket[[i]], ad) }
etiket <- vector("list", nrow(d))

# --- BAŞLIK sorunları ---
ekle("B1 kodlu etiket (kod 0 / kod 2)", grepl("(^|, )kod [0-9A-Za-z_]+", b))
ekle("B2 kesik başlık (…)", grepl("…", b, fixed = TRUE))
ekle("B3 cinsiyet/Erkek/Kadın öneki + kesik tablo adı", d$oto & grepl("^(cinsiyet: )?(erkek|kadın) · ", bl))
ekle("B4 tablo adı başlık olmuş (kırılım adı, ölçü adı yok)",
     d$oto & !grepl(" · ", b, fixed = TRUE) & !grepl("[(]", b) & grepl(" göre ", bl) & nchar(b) > 25)
ekle("B5 çift boyutlu bileşik etiket (a, b)", d$oto & grepl("\\), ", b) )
ekle("B6 bağlamsız çıplak etiket (Hizmetler, Hindi, 15-19...)",
     d$oto & nchar(b) <= 22 & !grepl(" · ", b, fixed = TRUE) & !ol("sayı|oran|hız|pay|süre|miktar|harcama|tutar|endeks|maliyet|değer|ücret|fiyat|nüfus|ölüm|doğum|kapasite|yaş|ortalama|toplam|harcaması|kullan|girişim|vergi|atık|kilometre"))
ekle("B7 başlıkta birim/ölçek (Binde, %, TL, Milyon)", grepl("\\((binde|yüz binde|%|tl|milyon tl|\\$)\\)|\\s%\\s|\\(milyon\\)", bl))
ekle("B8 Başlık Her Kelime Büyük (TÜİK biçimi)", nchar(b) > 20 & grepl("^([A-ZÇĞİÖŞÜ][a-zçğıöşü]+ ){3,}", b))
ekle("B9 aşırı uzun başlık (>110)", nchar(b) > 110)

# --- BİRİM çelişkileri ---
ekle("U1 başlıktaki birim ≠ birim alanı",
     (grepl("milyon tl", el) & u != "milyon TL") | (grepl("\\(tl\\)", el) & !grepl("TL", u)) |
     (grepl("binde", el) & !grepl("‰|binde", u)) | (grepl("yüz binde", el) & u != "yüz binde"))
ekle("U2 'sayısı' ama birim %/‰/oran", ou("sayısı|sayı\\b") & !ou("değişim|oran|pay") & u %in% c("%", "‰", "oran", "Yüzde"))
ekle("U3 'oranı/payı' ama birim kişi/adet/TL",
     ou("oranı|payı|\\(%\\)") & u %in% c("kişi", "adet", "TL", "girişim", "Sayı"))
ekle("U4 süre ama birim kişi/adet", ou("süre") & u %in% c("kişi", "adet", "Sayı"))
ekle("U5 'çalışan/istihdam' ama birim girişim", ou("çalışan|istihdam") & u == "girişim")
ekle("U6 ciro/TL/ücret ama birim kişi/girişim", ou("ciro|maaş|ücret(?!li)|harcama|vergi|maliyet|(^| )değer(i)?( |$)") & u %in% c("kişi", "girişim"))
ekle("U7 birim kişi/km² ama başlık yoğunluk değil", u == "kişi/km²" & !ou("yoğun"))
ekle("U8 çocuk sayısı hız (TDH) ‰ yazılmış", ou("doğurganlık hızı \\(çocuk") & u == "‰")

# --- KOPYA / gruplama ---
ak <- paste(d$imza)
ilk <- !duplicated(ak) & !is.na(d$imza)
ekle("K1 aynı değerleri taşıyan başka kart var (kopya veri)", d$oto & !is.na(d$imza) & (duplicated(ak) | duplicated(ak, fromLast = TRUE)) & nchar(d$imza) > 20)

# --- özet ---
d$sorun <- vapply(etiket, function(x) paste(x, collapse = " | "), "")
d$sorun_sayisi <- vapply(etiket, length, 1L)
cik <- file.path(PORTAL, "veri", "kart_denetim.csv")
write.csv(d[, c("id", "konu", "grup", "baslik", "birim", "not", "tablo", "oto", "tr", "son", "sorun_sayisi", "sorun")],
          cik, row.names = FALSE, fileEncoding = "UTF-8")

tum <- unlist(etiket)
cat(sprintf("\nToplam kart: %d | otomatik: %d | elle: %d\n", nrow(d), sum(d$oto), sum(!d$oto)))
cat(sprintf("Sorun işaretli kart: %d (%%%.0f) | işaretsiz: %d\n", sum(d$sorun_sayisi > 0),
            100 * mean(d$sorun_sayisi > 0), sum(d$sorun_sayisi == 0)))
cat(sprintf("  otomatik kartlarda sorunlu: %d / %d | elle kartlarda sorunlu: %d / %d\n",
            sum(d$oto & d$sorun_sayisi > 0), sum(d$oto), sum(!d$oto & d$sorun_sayisi > 0), sum(!d$oto)))
cat("\nSorun türüne göre kart sayısı:\n")
print(sort(table(tum), decreasing = TRUE))
cat("\nGrup bazında en sorunlu alt kategoriler (sorunlu kart / toplam):\n")
gk <- aggregate(cbind(sorunlu = sorun_sayisi > 0, toplam = 1) ~ konu + grup, d, sum)
gk <- gk[order(-gk$sorunlu), ]; print(head(gk, 20), row.names = FALSE)
cat("\nKayıt:", cik, "\n")
