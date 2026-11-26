.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
d <- dirname(normalizePath(.f, winslash = "/"))
source(file.path(d, "bolge_turet.R"))

esit <- function(a, b, ad, tol = 1e-9) {
  ok <- isTRUE(all.equal(unname(a), unname(b), tolerance = tol))
  if (!ok) { cat(sprintf("HATA %s: beklenen [%s], gelen [%s]\n", ad, paste(b, collapse = ","), paste(a, collapse = ","))); quit(status = 1) }
}

# Sahte coğrafya: TR1 = TR100, TR101; TR2 = TR200 (tek il)
il_sayi <- c(TR1 = 2L, TR2 = 1L)
yillar <- c("2020", "2021", "2022")
mk <- function(...) { l <- list(...); list(yillar = yillar, veri = lapply(l, I)) }

nuf <- mk(TR100 = c(100, 110, 120), TR101 = c(300, 290, 280), TR200 = c(50, 50, 50))
dog <- mk(TR100 = c(2, 3, 4), TR101 = c(6, 6, 6), TR200 = c(1, 1, 1), TR1 = c(8, 9, 10))

# bölge bileşeni: yoksa illerden toplanır, bir il eksikse NA
b <- bolge_bilesen(nuf, il_sayi)
esit(b[, "TR1"], c(400, 400, 400), "bilesen_topla")
esit(b[, "TR2"], c(50, 50, 50), "bilesen_tek_il")
eksik <- mk(TR100 = c(1, NA, 3), TR101 = c(1, 1, 1))
esit(bolge_bilesen(eksik, il_sayi)[, "TR1"], c(2, NA, 4), "bilesen_eksik_il")
# bölge sütunu zaten varsa o kullanılır
esit(bolge_bilesen(dog, il_sayi)[, "TR1"], c(8, 9, 10), "bilesen_hazir")

# oran: payda yıl ortası nüfus
o <- bolge_oran(dog, nuf, il_sayi, carpan = 1000, payda_ort = TRUE)
esit(o[2, "TR1"], 9 / ((400 + 400) / 2) * 1000, "oran_ort_nufus")
esit(is.na(o[1, "TR1"]), TRUE, "oran_ort_ilk_yil_NA")
o2 <- bolge_oran(dog, nuf, il_sayi, carpan = 100, payda_ort = FALSE)
esit(o2[3, "TR100"], 4 / 120 * 100, "oran_il")

# artış hızı
a <- bolge_artis(nuf, il_sayi)
esit(a[2, "TR100"], (110 - 100) / ((110 + 100) / 2) * 1000, "artis_il")
esit(a[2, "TR1"], 0, "artis_bolge")

# ağırlıklı ortalama: tüm iller varsa; ağırlık = nüfus
x <- mk(TR100 = c(10, 10, 10), TR101 = c(20, 20, 20), TR200 = c(5, 5, 5))
w <- bolge_agirlikli(x, nuf, il_sayi)
esit(w[1, "TR1"], (10 * 100 + 20 * 300) / 400, "agirlikli")
x2 <- mk(TR100 = c(10, NA, 10), TR101 = c(20, 20, 20), TR200 = c(5, 5, 5))
esit(is.na(bolge_agirlikli(x2, nuf, il_sayi)[2, "TR1"]), TRUE, "agirlikli_eksik_il_NA")

# yoğunluk: alan = nüfus / yoğunluk; bölge = toplam nüfus / toplam alan
y <- mk(TR100 = c(10, 10, 10), TR101 = c(30, 30, 30), TR200 = c(5, 5, 5))
yo <- bolge_yogunluk(y, nuf, il_sayi)
esit(yo[1, "TR1"], 400 / (100 / 10 + 300 / 30), "yogunluk")

# doğrulama: il değerleri formülle yeniden üretiliyor mu
hedef <- mk(TR100 = c(1, 2, 3), TR101 = c(2, 2, 2), TR200 = c(1, 1, 1))
yeni <- matrix(c(1, 2, 3, 2, 2, 2, 1, 1, 1), 3, dimnames = list(yillar, c("TR100", "TR101", "TR200")))
r <- bolge_dogrula(hedef, yeni)
esit(r$n, 9L, "dogrula_n"); esit(r$medyan, 0, "dogrula_medyan")
yeni2 <- yeni * 1.5
r2 <- bolge_dogrula(hedef, yeni2)
esit(round(r2$medyan, 6), 0.5, "dogrula_sapma")
esit(bolge_dogrula(hedef, yeni2 * NA)$n, 0L, "dogrula_bos")

cat("TUM TESTLER GECTI\n")
