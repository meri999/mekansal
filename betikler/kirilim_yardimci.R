# Kırılım üretici için saf işlevler (05f_kirilim_uret.R kaynak olarak yükler).
# Test: Rscript betikler/test_kirilim_yardimci.R

# Yaş kodunu [alt, üst] aralığına çevirir; yaş kodu değilse NULL.
yas_aralik <- function(k) {
  if (grepl("^Y[0-9]+T[0-9]+$", k)) return(as.numeric(strsplit(sub("^Y", "", k), "T")[[1]]))
  if (grepl("^Y_GE[0-9]+$", k)) return(c(as.numeric(sub("^Y_GE", "", k)), Inf))
  if (grepl("^Y_LT[0-9]+$", k)) return(c(-Inf, as.numeric(sub("^Y_LT", "", k)) - 1))
  if (grepl("^Y[0-9]+$", k)) { n <- as.numeric(sub("^Y", "", k)); return(c(n, n)) }
  NULL
}

# Yaş kodları aralığa göre (Y_LT başta, Y_GE sonda); öteki kodlar gelen sırada.
# _U ve _T her zaman en sonda (önce _U).
sirala <- function(kodlar) {
  kodlar <- unique(kodlar)
  gercek <- kodlar[!kodlar %in% c("_U", "_T")]
  ar <- lapply(gercek, yas_aralik)
  if (length(gercek) && !any(vapply(ar, is.null, TRUE))) {
    anahtar <- vapply(ar, function(a) {
      if (is.infinite(a[2])) 1000 + a[1] else if (is.infinite(a[1])) -1000 + a[2] else a[1]
    }, 0)
    gercek <- gercek[order(anahtar, vapply(ar, function(a) a[2], 0))]
  }
  c(gercek, intersect("_U", kodlar), intersect("_T", kodlar))
}

# Kod listesinde birbirini kapsayan gruplar var mı? (yaş aralıkları ya da 2 ve 2_1)
ortusen_var <- function(kodlar) {
  kodlar <- setdiff(unique(kodlar), c("_U", "_T"))
  ar <- Filter(Negate(is.null), lapply(kodlar, yas_aralik))
  if (length(ar) > 1) {
    for (i in seq_along(ar)) for (j in seq_along(ar)) {
      if (i < j && ar[[i]][1] <= ar[[j]][2] && ar[[j]][1] <= ar[[i]][2]) return(TRUE)
    }
  }
  any(vapply(kodlar, function(k) any(startsWith(kodlar, paste0(k, "_"))), TRUE))
}

# Etiket: elle düzeltme > tabloya özel > aynı kırılımın başka tablodaki etiketi > kural.
# Döner: list(etiket, eksik); eksik = ham kodla kalan kodlar.
etiket_coz <- function(df_id, boyut, kodlar, et, duzeltme) {
  ara <- function(x) {
    if (is.null(x) || !nrow(x)) return(rep(NA_character_, length(kodlar)))
    x <- x[x$boyut == boyut, ]
    ozel <- x[x$df_id == df_id, ]
    a <- ozel$kod_ad[match(kodlar, ozel$kod)]
    baska <- x[x$df_id != df_id, ]
    b <- baska$kod_ad[match(kodlar, baska$kod)]
    ifelse(is.na(a), b, a)
  }
  ad <- ara(et)
  d <- NULL
  if (!is.null(duzeltme) && nrow(duzeltme)) {
    z <- duzeltme[duzeltme$df_id == df_id & duzeltme$boyut == boyut, ]
    d <- z$kod_ad[match(kodlar, z$kod)]
    ad <- ifelse(is.na(d), ad, d)
  }
  eksik <- character()
  for (i in which(is.na(ad))) {
    k <- kodlar[i]
    a <- yas_aralik(k)
    ad[i] <- if (k == "_T") "Toplam"
      else if (k == "_U") "Bilinmiyor"
      else if (grepl("^Y[0-9]+T[0-9]+$", k)) sub("T", "-", sub("^Y", "", k))
      else if (grepl("^Y_GE[0-9]+$", k)) paste0(sub("^Y_GE", "", k), "+")
      else if (grepl("^Y_LT[0-9]+$", k)) paste0("<", sub("^Y_LT", "", k))
      else if (grepl("^Y[0-9]+$", k)) sub("^Y", "", k)
      else { eksik <- c(eksik, k); k }
  }
  list(etiket = unname(ad), eksik = eksik)
}

# Uzun tabloyu yoğun düz dizilere çevirir. d: kirilimlar, cografya, TIME_PERIOD
# ve sayısal v sütunları olan data.frame. Düz dizide en soldaki kırılım en yavaş
# değişir; kaynakta olmayan hücre NA.
yogunlastir <- function(d, kirilimlar, cografya, donem = "TIME_PERIOD", ad = "") {
  if (anyDuplicated(d[, c(cografya, donem, kirilimlar), drop = FALSE])) {
    stop(ad, ": yinelenen gözlem", call. = FALSE)
  }
  kodlar <- lapply(kirilimlar, function(k) sirala(d[[k]]))
  n <- vapply(kodlar, length, 1L)
  adim <- rev(cumprod(c(1, rev(n)[-length(n)])))
  konum <- do.call(cbind, lapply(seq_along(kirilimlar), function(i) match(d[[kirilimlar[i]]], kodlar[[i]]) - 1))
  d$.i <- 1 + as.vector(konum %*% adim)
  donemler <- sort(unique(d[[donem]]))
  yerler <- unique(d[[cografya]])
  yerler <- c(intersect("TR", yerler), sort(setdiff(yerler, "TR")))
  v <- list()
  for (dn in donemler) {
    x <- d[d[[donem]] == dn, ]
    v[[dn]] <- lapply(split(x, x[[cografya]]), function(y) {
      s <- rep(NA_real_, prod(n)); s[y$.i] <- y$v; s
    })
    v[[dn]] <- v[[dn]][intersect(yerler, names(v[[dn]]))]
  }
  list(kirilimlar = lapply(seq_along(kirilimlar), function(i) list(kol = kirilimlar[i], kodlar = kodlar[[i]])),
       donemler = donemler, yerler = yerler, v = v)
}

# Tutarlılık denetimi; hata mesajı listesi döner, boşsa geçti.
# Yalnız olcu == "sayi": _T hücresi = gerçek kodların toplamı (örtüşen kırılımlar
# atlanır); turkiye == "var" ise illerin toplamı = Türkiye.
dogrula <- function(tablo, olcu, turkiye = "topla", yuvarlak = FALSE, atla = character()) {
  if (olcu != "sayi") return(character())
  hata <- character()
  dims <- vapply(tablo$kirilimlar, function(k) length(k$kodlar), 1L)
  for (j in seq_along(dims)) {
    kodlar <- tablo$kirilimlar[[j]]$kodlar
    if (!"_T" %in% kodlar || ortusen_var(kodlar) || tablo$kirilimlar[[j]]$kol %in% atla) next
    ti <- match("_T", kodlar); gercek <- setdiff(seq_along(kodlar), ti)
    # gerçek kodları tamamen boş dilim (iç içe/seyrek kırılım) atlanır;
    # yuvarlak yayında (bin kişi) her bileşen için ±0,5 pay tanınır
    bozuk_mu <- function(z) {
      r <- z[gercek]
      if (all(is.na(r)) || is.na(z[ti])) return(FALSE)
      abs(sum(r, na.rm = TRUE) - z[ti]) > if (yuvarlak) 0.5 * (sum(!is.na(r)) + 1) else 0
    }
    bozuk <- 0
    for (dn in names(tablo$v)) for (yer in names(tablo$v[[dn]])) {
      a <- aperm(array(tablo$v[[dn]][[yer]], dim = rev(dims)), rev(seq_along(dims)))
      bozuk <- bozuk + sum(apply(a, seq_along(dims)[-j], bozuk_mu))
    }
    if (bozuk) hata <- c(hata, sprintf("%s: _T hücresi gerçek kodların toplamına eşit değil (%d hücre)", tablo$kirilimlar[[j]]$kol, bozuk))
  }
  if (turkiye == "var") {
    bozuk <- 0
    for (dn in names(tablo$v)) {
      x <- tablo$v[[dn]]
      if (is.null(x$TR) || length(x) < 2) next
      s <- colSums(do.call(rbind, x[names(x) != "TR"]), na.rm = TRUE)
      bozuk <- bozuk + sum(!is.na(x$TR) & s != x$TR)
    }
    if (bozuk) hata <- c(hata, sprintf("illerin toplamı Türkiye'ye eşit değil (%d hücre)", bozuk))
  }
  hata
}

# "2024-Q3" -> "2024 Ç3"; yıllık dönemler olduğu gibi kalır
donem_etiketi <- function(x) sub("^([0-9]{4})-Q([1-4])$", "\\1 Ç\\2", x)
