# Bülten çapraz tablolarını (iki ya da daha çok kategori eksenli) Kırılım Gezgini'nin
# ham biçimine çeviren genel okuyucu ve tablo kayıtları.
# Kullanım: yollar.R ve bulten_yardimci.R yüklendikten sonra source edilir
# (betikler/05j_bulten_kirilim_oku.R çalıştırıcıdır).
#
# Ham çıktı: veri/ham/BULTEN_KIR_<AD>.csv  (REF_AREA, TIME_PERIOD, OBS_VALUE, kırılım sütunları)
# Etiketler: ayar/kirilim_etiket_duzeltme.csv (df_id,boyut,kod,kod_ad)  -- df_id başına yeniden yazılır
# Boyut adları: ayar/kirilim_boyut_ad.csv (df_id,boyut,boyut_ad)
#
# Kurallar: sütun ve satır kategorileri başlık METNİYLE eşlenir; tanınmayan sütun ya da satır
# durdurur (sessizce atlanmaz). "-" ve boş hücre NA'dır (0 değil). Bir tabloda dönem türü
# (yıllık / çeyreklik / aylık / sezon) karışmaz.

KIR_ETIKET_DOSYA <- file.path(PORTAL, "ayar/kirilim_etiket_duzeltme.csv")
KIR_BOYUT_DOSYA <- file.path(PORTAL, "ayar/kirilim_boyut_ad.csv")
KIR_HAM <- file.path(PORTAL, "veri/ham")
KIR_BITIS <- "^(TÜİK|TurkStat|Kaynak|Source|Tablodaki rakamlar|Figures in)"
IBBS2_RE <- "^TR[0-9A-C][0-9]$"
IL_TAKMA <- read.csv(file.path(PORTAL, "ayar/il_ad_takma.csv"), fileEncoding = "UTF-8", stringsAsFactors = FALSE)
KIR_YER <- read.csv(file.path(PORTAL, "ayar/yer_adlari.csv"), fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# --- dosya ------------------------------------------------------------------
# bulten_dosya() + uzantısı .xls olup içi xlsx olan dosyaları (bitkisel üretim) imzadan tanır.
kir_oku <- function(klasor, parca) {
  yol <- bulten_dosya(klasor, parca)
  if (readxl::format_from_signature(yol) == "xlsx" && !grepl("[.]xlsx$", yol, ignore.case = TRUE)) {
    y2 <- tempfile(fileext = ".xlsx")
    if (!file.copy(yol, y2)) stop("xlsx kopyalanamadı: ", yol, call. = FALSE)
    yol <- y2
  }
  bulten_oku(yol)
}

# --- metin ------------------------------------------------------------------
bosmu <- function(x) is.na(x) | !nzchar(trimws(gsub(" ", " ", x)))

# Çift dilli hücreden Türkçe kısım: "Toplam\nTotal" -> "Toplam"; "Eğitim - Education" -> "Eğitim";
# "AB-27 ülke-EU-27 countries" -> "AB-27 ülke" (tire sayısı tekse ortadaki tire ayırır);
# dipnot işaretleri "(1)" atılır.
tr_ad <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  unname(vapply(x, function(s) {
    s <- gsub("\r", "", gsub(" ", " ", s))
    if (grepl("\n", s)) {
      sat <- trimws(strsplit(s, "\n")[[1]]); sat <- sat[nzchar(sat)]
      if (!length(sat)) return("")
      tr <- sat[1]
      for (k in seq_along(sat)[-1]) {
        if (grepl("^[a-zçğıöşü(%]", sat[k])) tr <- paste(tr, sat[k]) else break
      }
      s <- tr
    } else if (grepl(" - ", s, fixed = TRUE)) {
      s <- sub(" - .*$", "", s)
    } else if (grepl("[[:alpha:])]-\\s?[[:upper:]]", s)) {
      p <- gregexpr("-", s, fixed = TRUE)[[1]]
      n <- length(p)
      k <- if (n %% 2 == 1) p[(n + 1) / 2] else {
        aday <- p[grepl("^\\s?[[:upper:]]", substring(s, p + 1))]
        if (length(aday)) max(aday) else p[n]
      }
      s <- substr(s, 1, k - 1)
    }
    s <- gsub("\\s*\\([0-9]+\\)", "", s)
    trimws(gsub("\\s+", " ", s))
  }, ""))
}

# ASCII kısa kod: "Tayin / iş değişikliği" -> "TAYIN_IS_DEGISIKLIGI"
kir_slug <- function(x) {
  x <- chartr("çğıöşüÇĞİÖŞÜâîûÂÎÛ", "cgiosuCGIOSUaiuAIU", x)
  x <- toupper(gsub("[^A-Za-z0-9]+", "_", x))
  x <- gsub("^_+|_+$", "", x)
  substr(x, 1, 30)
}

# Hücre -> sayı. "10 216(1)" -> 10216; "-", "-(1)", boş -> NA; tanınmayan metin -> hata.
kir_sayi <- function(x, yer = "") {
  x <- trimws(gsub(" ", " ", as.character(x)))
  x[is.na(x)] <- ""
  y <- gsub("\\(([0-9]+|r)\\)", "", x)
  y <- gsub("(?<=[0-9]) (?=[0-9]{3})", "", y, perl = TRUE)
  y <- trimws(y)
  bos <- y %in% c("", "-", "–", "...", "..")
  v <- suppressWarnings(as.numeric(y))
  bozuk <- !bos & is.na(v)
  if (any(bozuk)) stop("sayı değil (", yer, "): ", paste(head(unique(x[bozuk]), 3), collapse = " | "), call. = FALSE)
  v[bos] <- NA_real_
  v
}

# --- kodlama ---------------------------------------------------------------
# Yaş etiketi -> kod: "0-4" -> Y0T4, "65+" -> Y_GE65, "30" -> Y30, Toplam -> _T
yas_kodu <- function(e) {
  e <- trimws(gsub("\\s+", "", tr_ad_yas(e)))
  ifelse(grepl("^Toplam", e), "_T",
  ifelse(grepl("^[0-9]+-[0-9]+$", e), paste0("Y", sub("-", "T", e)),
  ifelse(grepl("^[0-9]+\\+$", e), paste0("Y_GE", sub("\\+", "", e)),
  ifelse(grepl("^[0-9]+$", e), paste0("Y", e), NA_character_))))
}
# yaş hücreleri tire içerir ("0-4"); çift dilli ayırma yalnız "Toplam-Total" için
tr_ad_yas <- function(e) { e <- as.character(e); ifelse(grepl("^Toplam", trimws(e)), "Toplam", trimws(e)) }

# Kodlayıcı: tur + (gerekirse) sözlük. Etiket kaydı ortamda tutulur (geliş sırası).
kodlayici <- function(tanim, boyut) {
  tur <- if (is.null(tanim$tur)) "oto" else tanim$tur
  toplam <- if (is.null(tanim$toplam)) "^(Toplam|TOPLAM)($|\\s|-)" else tanim$toplam
  env <- new.env(); env$kod <- character(); env$ad <- character()
  kaydet <- function(k, a) {
    if (!k %in% env$kod) { env$kod <- c(env$kod, k); env$ad <- c(env$ad, a) }
    k
  }
  f <- function(ham) {
    a <- if (tur %in% c("yas")) tr_ad_yas(ham) else tr_ad(ham)
    k <- switch(tur,
      oto = {
        if (grepl(toplam, a)) "_T" else {
          k0 <- kir_slug(a)
          if (!nzchar(k0)) NA_character_ else {
            # aynı kısaltma farklı etikete denk gelirse sonek (_2, _3 ...)
            aday <- k0; j <- 1
            while (aday %in% env$kod && env$ad[match(aday, env$kod)] != a) { j <- j + 1; aday <- paste0(k0, "_", j) }
            aday
          }
        }
      },
      yil = { m <- regmatches(a, regexpr("(19|20)[0-9]{2}", a)); if (length(m)) m else NA_character_ },
      ham = if (nzchar(a)) a else NA_character_,
      yas = yas_kodu(ham),
      sezon = { m <- regmatches(ham, regexec("^\\s*((19|20)[0-9]{2})/'?([0-9]{2})\\s*$", ham))[[1]]
                if (length(m)) paste0(m[2], "/", m[4]) else NA_character_ },
      il = { if (grepl("^(Toplam|Türkiye)", a)) "TR" else il_kodu(a, KIR_YER, IL_TAKMA) },
      ibbs2 = { k0 <- ibbs2_kodu(ham); if (!is.na(k0) && (k0 == "TR" || grepl(IBBS2_RE, k0))) k0 else NA_character_ },
      sozluk = {
        s <- tanim$sozluk
        hedef <- if (isTRUE(tanim$tam)) ham else a
        i <- which(vapply(s$desen, function(re) grepl(re, hedef, perl = TRUE), TRUE))[1]
        if (is.na(i)) NA_character_ else { if (!is.na(s$ad[i])) a <- s$ad[i]; s$kod[i] }
      },
      stop("bilinmeyen kodlama türü: ", tur, call. = FALSE))
    if (is.na(k)) return(NA_character_)
    if (tur == "yas") a <- if (k == "_T") "Toplam" else a
    if (tur == "il" || tur == "ibbs2") a <- k
    kaydet(k, a)
  }
  list(f = f, etiket = function() data.frame(boyut = boyut, kod = env$kod, kod_ad = env$ad, stringsAsFactors = FALSE))
}

# Sözlük kısayolu: c(KOD = "desen", ...) ya da list(KOD = c("desen", "ad"))
sz <- function(...) {
  x <- list(...)
  data.frame(kod = names(x),
             desen = vapply(x, function(z) z[1], ""),
             ad = vapply(x, function(z) if (length(z) > 1) z[2] else NA_character_, ""),
             stringsAsFactors = FALSE)
}
CINSIYET_SZ <- sz(`_T` = "^Toplam", E = "^Erkek", K = "^Kadın")

# --- başlık ----------------------------------------------------------------
# Başlık satırlarındaki etiketleri sağa doğru doldurur: üst düzey boşsa soldakini alır;
# alt düzey yalnız tüm üst düzeyleri de boş (aynı birleşik hücrenin devamı) olan sütunda doldurulur.
baslik_matrisi <- function(d, satirlar) {
  m <- matrix("", length(satirlar), ncol(d))
  for (i in seq_along(satirlar)) m[i, ] <- tr_ad(unlist(d[satirlar[i], ]))
  ham <- m
  if (ncol(d) > 1) for (j in 2:ncol(d)) for (i in seq_along(satirlar)) {
    if (nzchar(ham[i, j])) next
    ust_bos <- if (i == 1) TRUE else all(!nzchar(ham[seq_len(i - 1), j]))
    if (ust_bos) m[i, j] <- m[i, j - 1]
  }
  m
}

# --- genel okuyucu ---------------------------------------------------------
# d: bulten_oku() çıktısı.
# bas: başlık satırları. veri: c(ilk, son) veri satırları (son NA ise bitiş deseni ya da dosya sonu).
# sutun: sütun eksenleri, adlandırılmış liste; her biri kodlayici tanımı
#   (tur = oto/yil/ham/yas/sozluk..., duzey = başlık düzeyi; sozluk türünde duzey verilmezse
#    tüm düzeylerin " | " ile birleşimi eşlenir).
# sutun_atla: başlık birleşimi bu desenlerden birine uyan sütunlar alınmaz.
# satir: satır eksenleri; her biri tanım + sutun (hücre sütunu), doldur (boşsa yukarıdakini al),
#   yoksay (bu desene uyan etiket boş sayılır).
# blok: veri içermeyen ara başlık satırları (yıl, bölge, grup); tanım + sutun + desen (+ ilk).
# sabit: tüm hücrelere eklenecek sütunlar (REF_AREA, TIME_PERIOD ...).
# satir_atla: veri içermeyen ve bu desene uyan satırlar atlanır.
capraz_oku <- function(d, bas, veri = c(max(bas) + 1, NA), sutun = list(), sutun_atla = character(),
                       veri_sutun = NULL, satir = list(), blok = list(), sabit = list(),
                       satir_atla = character(), bitis = KIR_BITIS, ad = "") {
  H <- baslik_matrisi(d, bas)
  son <- if (is.na(veri[2])) nrow(d) else veri[2]
  satirlar <- veri[1]:son
  bit <- which(grepl(bitis, trimws(d[[1]][satirlar])))
  if (length(bit)) satirlar <- satirlar[seq_len(bit[1] - 1)]
  sat_sut <- unique(c(vapply(satir, function(s) s$sutun, 1), vapply(blok, function(s) s$sutun, 1)))
  if (is.null(veri_sutun)) {
    veri_sutun <- setdiff(seq_len(ncol(d)), sat_sut)
    veri_sutun <- veri_sutun[vapply(veri_sutun, function(j) any(!bosmu(d[satirlar, j])), TRUE)]
  }
  # sütun eksenleri
  sk <- lapply(names(sutun), function(e) kodlayici(sutun[[e]], e)); names(sk) <- names(sutun)
  scod <- list(); sec <- integer()
  for (j in veri_sutun) {
    birlesik <- paste(H[, j], collapse = " | ")
    if (length(sutun_atla) && any(vapply(sutun_atla, function(re) grepl(re, birlesik, perl = TRUE), TRUE))) next
    kod <- vapply(names(sutun), function(e) {
      t <- sutun[[e]]
      hucre <- if (!is.null(t$duzey)) H[t$duzey, j] else birlesik
      sk[[e]]$f(hucre)
    }, "")
    if (any(is.na(kod))) {
      stop(ad, ": tanınmayan sütun ", j, " [", birlesik, "] eksen: ", paste(names(sutun)[is.na(kod)], collapse = ","), call. = FALSE)
    }
    scod[[length(scod) + 1]] <- kod; sec <- c(sec, j)
  }
  if (!length(sec)) stop(ad, ": veri sütunu bulunamadı", call. = FALSE)
  # satır eksenleri
  rk <- lapply(names(satir), function(e) kodlayici(satir[[e]], e)); names(rk) <- names(satir)
  bk <- lapply(names(blok), function(e) kodlayici(blok[[e]], e)); names(bk) <- names(blok)
  durum <- lapply(blok, function(b) NA_character_)
  for (e in names(blok)) durum[[e]] <- if (is.null(blok[[e]]$ilk)) NA_character_ else bk[[e]]$f(blok[[e]]$ilk)
  onceki <- lapply(satir, function(s) NA_character_)
  cikti <- list()
  for (r in satirlar) {
    hucre <- unlist(d[r, ])
    if (all(bosmu(hucre))) next
    veri_var <- any(!bosmu(hucre[sec]))
    # blok satırı
    if (!veri_var) {
      bulundu <- FALSE
      for (e in names(blok)) {
        b <- blok[[e]]; et <- trimws(hucre[b$sutun])
        if (!bosmu(et) && grepl(b$desen, et, perl = TRUE)) {
          k <- bk[[e]]$f(et)
          if (is.na(k)) stop(ad, ": tanınmayan blok etiketi (", e, ") satır ", r, ": ", et, call. = FALSE)
          durum[[e]] <- k; bulundu <- TRUE
          # alt bloklar sıfırlanır
          sonra <- names(blok)[seq_along(blok) > match(e, names(blok))]
          for (s2 in sonra) if (is.null(blok[[s2]]$ilk)) durum[[s2]] <- NA_character_
          for (e2 in names(satir)) onceki[[e2]] <- NA_character_
          break
        }
      }
      if (bulundu) next
      et <- paste(hucre[!bosmu(hucre)], collapse = " | ")
      if (length(satir_atla) && any(vapply(satir_atla, function(re) grepl(re, et, perl = TRUE), TRUE))) next
      stop(ad, ": veri içermeyen tanınmayan satır ", r, ": ", et, call. = FALSE)
    }
    # veri satırı
    kod <- character()
    for (e in names(satir)) {
      s <- satir[[e]]; et <- hucre[s$sutun]
      if (!bosmu(et) && !is.null(s$yoksay) && grepl(s$yoksay, trimws(et), perl = TRUE)) et <- NA
      if (bosmu(et)) {
        if (isTRUE(s$doldur) && !is.na(onceki[[e]])) { kod[e] <- onceki[[e]]; next }
        if (!is.null(s$bos)) { kod[e] <- rk[[e]]$f(s$bos); next }
        stop(ad, ": satır ", r, " boş etiket (", e, ")", call. = FALSE)
      }
      k <- rk[[e]]$f(et)
      if (is.na(k)) stop(ad, ": tanınmayan satır etiketi (", e, ") satır ", r, ": ", et, call. = FALSE)
      kod[e] <- k; onceki[[e]] <- k
    }
    for (e in names(blok)) {
      if (is.na(durum[[e]])) stop(ad, ": satır ", r, " için blok değeri yok (", e, ")", call. = FALSE)
    }
    v <- kir_sayi(hucre[sec], paste(ad, "satır", r))
    parca <- data.frame(OBS_VALUE = v, stringsAsFactors = FALSE)
    for (e in names(sutun)) parca[[e]] <- vapply(scod, function(z) z[[e]], "")
    for (e in names(satir)) parca[[e]] <- kod[[e]]
    for (e in names(blok)) parca[[e]] <- durum[[e]]
    cikti[[length(cikti) + 1]] <- parca
  }
  df <- do.call(rbind, cikti)
  for (e in names(sabit)) df[[e]] <- sabit[[e]]
  etiket <- do.call(rbind, c(lapply(sk, function(k) k$etiket()), lapply(rk, function(k) k$etiket()),
                             lapply(bk, function(k) k$etiket())))
  list(veri = df, etiket = etiket)
}

# Birden çok okuma sonucunu birleştirir (aynı eksenler; etiket ilk gelen kalır).
kir_birlestir <- function(...) {
  x <- list(...)
  sut <- unique(unlist(lapply(x, function(s) names(s$veri))))
  veri <- do.call(rbind, lapply(x, function(s) { for (k in setdiff(sut, names(s$veri))) s$veri[[k]] <- NA_character_; s$veri[, sut] }))
  et <- do.call(rbind, lapply(x, function(s) s$etiket))
  et <- et[!duplicated(et[, c("boyut", "kod")]), ]
  list(veri = veri, etiket = et)
}

# Okuma sonucunu ham biçime getirir: NA değerler atılır, eksik REF_AREA/TIME_PERIOD durdurur,
# yinelenen gözlem durdurur. eksenler: kırılım sütunları (sırayla).
kir_son <- function(s, eksenler, ad = "") {
  v <- s$veri
  for (k in c("REF_AREA", "TIME_PERIOD", eksenler)) {
    if (!k %in% names(v)) stop(ad, ": sütun yok: ", k, call. = FALSE)
    if (any(is.na(v[[k]]) | !nzchar(v[[k]]))) stop(ad, ": boş değer: ", k, call. = FALSE)
  }
  fazla <- setdiff(names(v), c("REF_AREA", "TIME_PERIOD", "OBS_VALUE", eksenler))
  if (length(fazla)) stop(ad, ": tanımsız sütun: ", paste(fazla, collapse = ","), call. = FALSE)
  anahtar <- do.call(paste, c(v[, c("REF_AREA", "TIME_PERIOD", eksenler)], sep = "|"))
  if (anyDuplicated(anahtar)) stop(ad, ": yinelenen gözlem: ", anahtar[duplicated(anahtar)][1], call. = FALSE)
  v <- v[!is.na(v$OBS_VALUE), c("REF_AREA", "TIME_PERIOD", "OBS_VALUE", eksenler)]
  rownames(v) <- NULL
  et <- s$etiket[s$etiket$boyut %in% eksenler, ]
  list(veri = v, etiket = et, eksenler = eksenler)
}

# --- yazma -----------------------------------------------------------------
# df_id'ye ait satırları silip yenilerini ekler (yeniden çalıştırınca çoğalmaz).
kir_csv_guncelle <- function(yol, yeni, df_id, sutunlar) {
  eski <- if (file.exists(yol)) read.csv(yol, fileEncoding = "UTF-8", colClasses = "character", na.strings = character()) else
    as.data.frame(setNames(replicate(length(sutunlar), character(), simplify = FALSE), sutunlar))
  eski <- eski[eski$df_id != df_id, sutunlar, drop = FALSE]
  yeni <- yeni[, sutunlar, drop = FALSE]
  utils::write.csv(rbind(eski, yeni), yol, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(yol)
}

kir_yaz <- function(s, ad, boyut_ad, ham = KIR_HAM, etiket_dosya = KIR_ETIKET_DOSYA, boyut_dosya = KIR_BOYUT_DOSYA) {
  df_id <- paste0("BULTEN_KIR_", ad)
  utils::write.csv(s$veri, file.path(ham, paste0(df_id, ".csv")), row.names = FALSE, fileEncoding = "UTF-8", na = "")
  et <- data.frame(df_id = df_id, boyut = s$etiket$boyut, kod = s$etiket$kod, kod_ad = s$etiket$kod_ad, stringsAsFactors = FALSE)
  kir_csv_guncelle(etiket_dosya, et, df_id, c("df_id", "boyut", "kod", "kod_ad"))
  eksik <- setdiff(s$eksenler, names(boyut_ad))
  if (length(eksik)) stop(ad, ": boyut adı yok: ", paste(eksik, collapse = ","), call. = FALSE)
  ba <- data.frame(df_id = df_id, boyut = s$eksenler, boyut_ad = unname(boyut_ad[s$eksenler]), stringsAsFactors = FALSE)
  kir_csv_guncelle(boyut_dosya, ba, df_id, c("df_id", "boyut", "boyut_ad"))
  invisible(df_id)
}

# Test ve inceleme için: etiket ya da kodla hücre değeri.
# ornek: kir_hucre(s, TIME_PERIOD = "2025", NEDEN = "Eğitim", CINSIYET = "Kadın")
kir_hucre <- function(s, ...) {
  k <- list(...); v <- s$veri
  for (e in names(k)) {
    hedef <- k[[e]]
    et <- s$etiket[s$etiket$boyut == e, ]
    kod <- if (hedef %in% v[[e]]) hedef else et$kod[match(hedef, et$kod_ad)]
    v <- v[v[[e]] %in% kod, ]
  }
  if (nrow(v) > 1) stop("kir_hucre: birden çok hücre (", nrow(v), ")", call. = FALSE)
  if (!nrow(v)) return(NA_real_)
  v$OBS_VALUE
}
