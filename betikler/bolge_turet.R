# İBBS-1 bölge değerini il göstergelerinden türetme (05_gosterge_uret.R yükler).
# Kural: bölge değeri yalnız, aynı formülle ILLERİN değerini yeniden üretebildiğimiz
# (oran, artış hızı) ya da tanım gereği doğru olan (ağırlıklı ortalama, yoğunluk)
# göstergelerde üretilir. Tanımlar ayar/bolge_turet.csv'dedir.

IL_RE <- "^TR[0-9A-C][0-9]{2}$"
BOLGE_RE <- "^TR[0-9A-C]$"

# Gösterge -> yıl x yer matrisi
seri_matris <- function(g) {
  v <- lapply(g$veri, function(x) as.numeric(unlist(x)))
  m <- do.call(cbind, v)
  rownames(m) <- as.character(unlist(g$yillar))
  m
}

# Matrisi verilen yıllara genişletir (olmayan yıl = NA)
genislet <- function(m, yil) {
  out <- matrix(NA_real_, length(yil), ncol(m), dimnames = list(yil, colnames(m)))
  ok <- yil %in% rownames(m)
  out[ok, ] <- m[yil[ok], , drop = FALSE]
  out
}

# Bir önceki takvim yılının satırı (yoksa NA)
onceki_yil <- function(m) {
  m[match(as.character(as.integer(rownames(m)) - 1L), rownames(m)), , drop = FALSE]
}

# Bölge bileşeni: bölge serisi varsa o, yoksa BÜTÜN illerin toplamı (biri eksikse NA)
bolge_bilesen <- function(g, il_sayi) {
  m <- seri_matris(g)
  out <- matrix(NA_real_, nrow(m), length(il_sayi), dimnames = list(rownames(m), names(il_sayi)))
  for (b in names(il_sayi)) {
    if (b %in% colnames(m)) { out[, b] <- m[, b]; next }
    ils <- grep(paste0("^", b, "[0-9]{2}$"), colnames(m), value = TRUE)
    if (length(ils) == il_sayi[[b]]) out[, b] <- rowSums(m[, ils, drop = FALSE])
  }
  out
}

# İller + bölgeler yan yana
birlesik <- function(g, il_sayi) {
  m <- seri_matris(g)
  cbind(m[, grepl(IL_RE, colnames(m)), drop = FALSE], bolge_bilesen(g, il_sayi))
}

# pay / payda * carpan (isteğe bağlı: payda = yıl ortası = (t + t-1) / 2)
bolge_oran <- function(pay, payda, il_sayi, carpan = 1, payda_ort = FALSE) {
  P <- birlesik(pay, il_sayi); Q <- birlesik(payda, il_sayi)
  yil <- sort(union(rownames(P), rownames(Q)))
  yer <- intersect(colnames(P), colnames(Q))
  P <- genislet(P, yil)[, yer, drop = FALSE]; Q <- genislet(Q, yil)[, yer, drop = FALSE]
  if (payda_ort) Q <- (Q + onceki_yil(Q)) / 2
  r <- P / Q * carpan
  r[!is.finite(r)] <- NA_real_
  r
}

# Yıllık artış hızı (‰): (N_t - N_t-1) / ((N_t + N_t-1) / 2) * 1000
bolge_artis <- function(nufus, il_sayi) {
  M <- birlesik(nufus, il_sayi)
  r <- (M - onceki_yil(M)) / ((M + onceki_yil(M)) / 2) * 1000
  r[!is.finite(r)] <- NA_real_
  r
}

# Bölge = sum(x_il * w_il) / sum(w_il); bölgenin bütün illeri iki seride de varsa
bolge_agirlikli <- function(x, w, il_sayi) {
  X <- seri_matris(x); W <- seri_matris(w)
  W <- genislet(W, rownames(X))
  out <- matrix(NA_real_, nrow(X), length(il_sayi), dimnames = list(rownames(X), names(il_sayi)))
  for (b in names(il_sayi)) {
    ils <- grep(paste0("^", b, "[0-9]{2}$"), colnames(X), value = TRUE)
    if (length(ils) != il_sayi[[b]] || !all(ils %in% colnames(W))) next
    out[, b] <- rowSums(X[, ils, drop = FALSE] * W[, ils, drop = FALSE]) /
      rowSums(W[, ils, drop = FALSE] + 0 * X[, ils, drop = FALSE])
  }
  out[!is.finite(out)] <- NA_real_
  out
}

# Yoğunluk: alan_il = nüfus_il / yoğunluk_il; bölge = toplam nüfus / toplam alan
bolge_yogunluk <- function(yog, nufus, il_sayi) {
  Y <- seri_matris(yog); N <- genislet(seri_matris(nufus), rownames(Y))
  out <- matrix(NA_real_, nrow(Y), length(il_sayi), dimnames = list(rownames(Y), names(il_sayi)))
  for (b in names(il_sayi)) {
    ils <- grep(paste0("^", b, "[0-9]{2}$"), colnames(Y), value = TRUE)
    if (length(ils) != il_sayi[[b]] || !all(ils %in% colnames(N))) next
    out[, b] <- rowSums(N[, ils, drop = FALSE]) / rowSums(N[, ils, drop = FALSE] / Y[, ils, drop = FALSE])
  }
  out[!is.finite(out)] <- NA_real_
  out
}

# İl değerlerini formülle yeniden üretme hatası (göreli): n, medyan, p95
bolge_dogrula <- function(hedef, yeni) {
  H <- seri_matris(hedef)
  yil <- intersect(rownames(H), rownames(yeni))
  yer <- intersect(colnames(H)[grepl(IL_RE, colnames(H))], colnames(yeni))
  a <- H[yil, yer, drop = FALSE]; b <- yeni[yil, yer, drop = FALSE]
  ok <- is.finite(a) & is.finite(b) & a != 0
  hata <- abs(b[ok] - a[ok]) / abs(a[ok])
  list(n = length(hata),
       medyan = if (length(hata)) median(hata) else NA_real_,
       p95 = if (length(hata)) unname(quantile(hata, 0.95)) else NA_real_)
}

# Göstergeyi başlığa göre bul (elle tanımlı olan öncelikli)
bolge_bul <- function(cikti, baslik) {
  ix <- which(vapply(cikti, function(g) identical(g$baslik, baslik) && !isTRUE(g$sadece_tr), TRUE))
  if (!length(ix)) return(NULL)
  elle <- ix[!vapply(cikti[ix], function(g) isTRUE(g$oto), TRUE)]
  cikti[[if (length(elle)) elle[1] else ix[1]]]
}

# kurallar: hedef, yontem (oran|artis|agirlikli|yogunluk), pay, payda, carpan, payda_ort, aciklama
# Dönüş: list(cikti, rapor)
bolge_uygula <- function(cikti, kurallar, il_sayi, medyan_esik = 0.01, p95_esik = 0.05) {
  rapor <- data.frame(hedef = character(), yontem = character(), durum = character(),
                      n = integer(), medyan_hata = numeric(), p95_hata = numeric(),
                      bolge_hucre = integer(), stringsAsFactors = FALSE)
  rap <- function(k, durum, n = NA_integer_, med = NA_real_, p95 = NA_real_, hucre = 0L) {
    rapor[nrow(rapor) + 1, ] <<- list(k$hedef, k$yontem, durum, n, med, p95, hucre)
  }
  for (i in seq_len(nrow(kurallar))) {
    k <- kurallar[i, ]
    ix <- which(vapply(cikti, function(g) identical(g$baslik, k$hedef) && !isTRUE(g$sadece_tr), TRUE))
    if (!length(ix)) { rap(k, "hedef yok"); next }
    ix <- ix[1]; hedef <- cikti[[ix]]
    b1 <- if (nzchar(k$pay)) bolge_bul(cikti, k$pay) else NULL
    b2 <- if (nzchar(k$payda)) bolge_bul(cikti, k$payda) else NULL
    if (k$yontem %in% c("oran") && (is.null(b1) || is.null(b2))) { rap(k, "bileşen yok"); next }
    if (k$yontem %in% c("artis", "agirlikli", "yogunluk") && is.null(b1)) { rap(k, "bileşen yok"); next }

    yeni <- tryCatch(switch(k$yontem,
      oran = bolge_oran(b1, b2, il_sayi, as.numeric(k$carpan), isTRUE(as.integer(k$payda_ort) == 1L)),
      artis = bolge_artis(b1, il_sayi),
      agirlikli = bolge_agirlikli(hedef, b1, il_sayi),
      yogunluk = bolge_yogunluk(hedef, b1, il_sayi),
      stop("bilinmeyen yöntem")), error = function(e) e)
    if (inherits(yeni, "error")) { rap(k, paste("hata:", conditionMessage(yeni))); next }

    # doğrulama: il değerleri aynı formülle yeniden üretilebiliyor mu (tanım gereği olanlarda yok)
    dog <- if (k$yontem %in% c("oran", "artis")) bolge_dogrula(hedef, yeni) else NULL
    if (!is.null(dog)) {
      if (dog$n < 50) { rap(k, "doğrulanamadı (az karşılaştırma)", dog$n, dog$medyan, dog$p95); next }
      if (dog$medyan > medyan_esik || dog$p95 > p95_esik) { rap(k, "reddedildi (il değeri tutmuyor)", dog$n, dog$medyan, dog$p95); next }
    }

    yil <- as.character(unlist(hedef$yillar))
    bol <- grep(BOLGE_RE, colnames(yeni), value = TRUE)
    bol <- setdiff(bol, names(hedef$veri))
    ond <- hedef$ondalik
    hucre <- 0L
    for (b in bol) {
      v <- yeni[match(yil, rownames(yeni)), b]
      if (!any(is.finite(v))) next
      hedef$veri[[b]] <- I(round(v, ond + 3))
      hucre <- hucre + sum(is.finite(v))
    }
    if (hucre == 0L) { rap(k, "bölge değeri üretilemedi", if (is.null(dog)) NA_integer_ else dog$n); next }
    hedef$bolge_hesap <- TRUE
    ek <- paste0("Bölge değeri, ", k$aciklama, " hesaplanmıştır.")
    hedef$not <- if (is.null(hedef$not) || !nzchar(hedef$not)) ek else paste(hedef$not, ek)
    cikti[[ix]] <- hedef
    rap(k, "üretildi", if (is.null(dog)) NA_integer_ else dog$n,
        if (is.null(dog)) NA_real_ else dog$medyan, if (is.null(dog)) NA_real_ else dog$p95, hucre)
  }
  list(cikti = cikti, rapor = rapor)
}
