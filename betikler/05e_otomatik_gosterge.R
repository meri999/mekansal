.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))

# veri/tarama.csv'deki tablolardan otomatik gösterge adayları üretir
# ve ayar/otomatik_gosterge.csv dosyasına yazar.
# (05_gosterge_uret.R bu dosyayı, elle yazılan gostergeler_il/tr.csv'ye
# ek olarak okur.)
#
# Kurallar:
#   - Yalnız yıllık seriler alınır.
#   - Her boyut için: tek kod varsa sabit, "_T" varsa _T, yoksa serbest
#     bırakılır (en çok 12 kombinasyon).
#   - Elle tanımlanmış göstergelerde kullanılan tablolar atlanır.
#   - Her aday için (il/ülke, yıl) başına tek gözlem koşulu denetlenir.
#
# Atlanan tablolar ve nedenleri veri/otomatik_atlanan.csv içinde.
# Kullanım: Rscript betikler/05e_otomatik_gosterge.R [--id=DF_A,DF_B]

suppressMessages(library(data.table))

GEO_RE <- "^TR([0-9A-C]([0-9]{0,2}))?$"
META <- c("STRUCTURE", "STRUCTURE_ID", "ACTION", "TIME_PERIOD", "OBS_VALUE",
          "OBS_STATUS", "CONF_STATUS", "DECIMALS", "DECIMAL", "UNIT_MEASURE",
          "UNIT_MULT", "TIME_FORMAT", "DATAFLOW", "FREQ")


# --- yardımcı fonksiyonlar ---------------------------------------------

csv_oku <- function(yol, ...) {
  read.csv(file.path(PORTAL, yol), fileEncoding = "UTF-8",
           stringsAsFactors = FALSE, ...)
}

# Ham veri dosyasının yolunu bulur, yoksa NA döner.
ham_yol <- function(id) {
  for (klasor in c("ham", "ham_ekonomi", "ham_tum")) {
    for (uzanti in c(".csv", ".csv.gz")) {
      f <- file.path(PORTAL, "veri", klasor, paste0(id, uzanti))
      if (file.exists(f)) return(f)
    }
  }
  NA_character_
}

ham_oku <- function(f) {
  tryCatch({
    if (grepl("[.]gz$", f)) {
      read.csv(gzfile(f), colClasses = "character",
               check.names = FALSE, encoding = "UTF-8")
    } else {
      fread(f, colClasses = "character", encoding = "UTF-8",
            data.table = FALSE, showProgress = FALSE)
    }
  }, error = function(e) NULL)
}

# Kod için Türkçe etiketi döndürür; bulunamazsa kodun kendisini verir.
etiket <- function(id, boyut, kod) {
  if (is.null(et)) return(kod)
  x <- et[et$df_id == id & et$boyut == boyut & et$kod == kod, ]
  if (nrow(x)) x$kod_ad[1] else kod
}

# otomatik_karar.csv'de elle verilen kararları (durum, başlık, birim...)
# aday tablosuna işler.
karar_uygula <- function(a) {
  kf <- file.path(PORTAL, "ayar/otomatik_karar.csv")
  if (file.exists(kf)) {
    kr <- csv_oku("ayar/otomatik_karar.csv", na.strings = character())
    i <- match(paste(a$df_id, a$filtre), paste(kr$df_id, kr$filtre))
    j <- !is.na(i)
    a$durum[j] <- kr$durum[i[j]]

    for (k in intersect(c("baslik", "birim", "ondalik", "grup", "bolge",
                          "carpan"), names(kr))) {
      v <- kr[[k]][i[j]]
      dolu <- which(j)[nzchar(v)]
      if (!k %in% names(a)) a[[k]] <- ""
      if (length(dolu)) {
        if (k %in% c("ondalik", "carpan")) {
          a[[k]][dolu] <- as.numeric(v[nzchar(v)])
        } else {
          a[[k]][dolu] <- v[nzchar(v)]
        }
      }
    }
  }
  if (!"bolge" %in% names(a)) a$bolge <- ""
  a
}


# --- girdiler ----------------------------------------------------------

arg <- commandArgs(TRUE)
sec <- sub("^--id=", "", grep("^--id=", arg, value = TRUE))
cik <- file.path(PORTAL, "ayar/otomatik_gosterge.csv")

tar <- csv_oku("veri/tarama.csv")
tar <- tar[tar$durum == "ok", ]
if (length(sec)) tar <- tar[tar$id %in% strsplit(sec, ",")[[1]], ]

kat <- csv_oku("ayar/katalog.csv")
esl <- csv_oku("ayar/kategori_esleme.csv")

# data.frame olarak bırakıldı: data.table olsaydı boyut/kod sütun adları
# fonksiyon argümanlarıyla karışıyordu.
et <- NULL
if (file.exists(file.path(PORTAL, "veri/etiket_kod.csv"))) {
  et <- csv_oku("veri/etiket_kod.csv", colClasses = "character")
}
# Türkiye düzeyli tabloların etiketleri (04b_etiket_tr.R)
if (file.exists(file.path(PORTAL, "veri/etiket_tr.csv"))) {
  et_tr <- csv_oku("veri/etiket_tr.csv", colClasses = "character")
  et <- if (is.null(et)) et_tr else rbind(et, et_tr[, names(et)])
}

gostergeler_il <- csv_oku("ayar/gostergeler_il.csv")
gostergeler_tr <- csv_oku("ayar/gostergeler_tr.csv")
elle <- unique(c(gostergeler_il$df_id,
                 gostergeler_tr$df_id,
                 gostergeler_il$payda_df))

# Yalnız kararları yeniden uygula, tarama yapma
if ("--sadece-karar" %in% arg) {
  a <- karar_uygula(csv_oku("ayar/otomatik_gosterge.csv",
                            na.strings = character()))
  write.csv(a, cik, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  print(table(a$durum))
  quit(status = 0)
}


# --- tarama ------------------------------------------------------------

atla <- list()
aday <- list()

atlandi <- function(id, neden) {
  atla[[length(atla) + 1]] <<- data.frame(id = id, neden = neden)
}

for (i in seq_len(nrow(tar))) {
  id <- tar$id[i]

  if (id %in% elle) {
    atlandi(id, "elle_tanimli")
    next
  }

  f <- ham_yol(id)
  if (is.na(f)) {
    atlandi(id, "dosya_yok")
    next
  }

  k <- kat[kat$id == id, ]
  m <- esl[esl$kategori == k$kategori, ]
  if (!nrow(m)) {
    atlandi(id, "kategori_eslemesi_yok")
    next
  }

  d <- ham_oku(f)
  if (is.null(d) || !nrow(d)) {
    atlandi(id, "okunamadi")
    next
  }

  # Coğrafya sütunu: birden fazlaysa en çok farklı kodu olanı seç
  gcol <- strsplit(tar$cografya[i], ";")[[1]]
  gcol <- gcol[gcol %in% names(d)]
  if (!length(gcol)) {
    atlandi(id, "cografya_sutunu_yok")
    next
  }
  nk <- vapply(gcol, function(g) {
    length(unique(d[[g]][grepl(GEO_RE, d[[g]])]))
  }, 1L)
  if (sum(nk >= 10) >= 2) {
    atlandi(id, "matris_iki_cografya")
    next
  }
  g <- gcol[which.max(nk)]

  d$GEO <- d[[g]]
  d <- d[grepl(GEO_RE, d$GEO), ]
  d$DEGER <- suppressWarnings(as.numeric(d$OBS_VALUE))
  d <- d[!is.na(d$DEGER), ]
  if (!nrow(d)) {
    atlandi(id, "deger_yok")
    next
  }
  if (!all(grepl("^[0-9]{4}$", d$TIME_PERIOD))) {
    atlandi(id, "yillik_degil")
    next
  }

  # Boyutları sabitle ya da serbest bırak
  boyutlar <- setdiff(names(d), c(META, g, gcol, "GEO", "DEGER"))
  filt <- character()
  serbest <- list()
  for (b in boyutlar) {
    v <- setdiff(unique(d[[b]]), c(NA, ""))
    if (length(v) <= 1) next

    if ("_T" %in% v) {
      filt <- c(filt, paste0(b, "=_T"))
      next
    }
    v <- setdiff(v, "_Z")
    if (length(v) > 1) {
      serbest[[b]] <- sort(v)
    } else if (length(v) == 1) {
      filt <- c(filt, paste0(b, "=", v))
    }
  }

  if (length(serbest)) {
    komb <- expand.grid(serbest, stringsAsFactors = FALSE)
  } else {
    komb <- data.frame(x = NA)
  }
  if (nrow(komb) > 30) {
    atlandi(id, sprintf("cok_kombinasyon_%d", nrow(komb)))
    next
  }

  # Toplam cinsiyet satırı varsa kadın/erkek kesitlerini de dene
  cinsiyet <- list()
  if ("SEX" %in% boyutlar && "SEX=_T" %in% filt) {
    for (s in setdiff(unique(d$SEX), c("_T", "_Z"))) {
      f_s <- sub("SEX=_T", paste0("SEX=", s), paste(filt, collapse = ";"))
      cinsiyet[[length(cinsiyet) + 1]] <- c(f_s, s)
    }
  }

  # Denenecek filtre kombinasyonları
  denemeler <- list()
  for (r in seq_len(nrow(komb))) {
    ek_f <- character()
    ek_ad <- ""
    if (length(serbest)) {
      ek_f <- paste0(names(serbest), "=", unlist(komb[r, , drop = TRUE]))
      ek_ad <- paste(
        vapply(names(serbest), function(b) {
          e <- etiket(id, b, komb[r, b])
          if (identical(e, komb[r, b])) paste("kod", e) else e
        }, ""),
        collapse = ", ")
    }
    denemeler[[length(denemeler) + 1]] <-
      list(f = paste(c(filt, ek_f), collapse = ";"), ad = ek_ad)
  }
  if (length(serbest) == 0) {
    for (e in cinsiyet) {
      denemeler[[length(denemeler) + 1]] <-
        list(f = e[1], ad = paste0("cinsiyet: ", etiket(id, "SEX", e[2])))
    }
  }

  n_ok <- 0
  for (t in denemeler) {
    # filtreyi uygula
    x <- d
    for (p in strsplit(strsplit(t$f, ";")[[1]], "=")) {
      if (length(p) == 2) x <- x[x[[p[1]]] == p[2], ]
    }
    if (!nrow(x)) next
    # (il, yıl) başına tek gözlem olmalı
    if (anyDuplicated(x[, c("GEO", "TIME_PERIOD")])) next

    # birim
    birimler <- character()
    if ("UNIT_MEASURE" %in% names(x)) {
      birimler <- setdiff(unique(x$UNIT_MEASURE), c("_Z", ""))
    }
    birim <- paste(birimler, collapse = "/")
    birim_ad <- ""
    if (length(birimler) == 1) {
      birim_ad <- etiket(id, "UNIT_MEASURE", birimler)
      if (identical(birim_ad, birimler)) birim_ad <- ""
    }

    # kapsam: kaç il, kaç bölge, Türkiye var mı
    il_sayisi <- length(unique(x$GEO[grepl("^TR[0-9A-C][0-9]{2}$", x$GEO)]))
    bolge_sayisi <- length(unique(x$GEO[grepl("^TR[0-9A-C]$", x$GEO)]))
    tr_var <- "TR" %in% x$GEO

    baslik <- k$ad_tr
    kisa <- if (nchar(k$ad_tr) <= 60) k$ad_tr else paste0(sub("[[:space:]]+[^[:space:]]*$", "", substr(k$ad_tr, 1, 60)), "…")
    if (nzchar(t$ad)) baslik <- t$ad
    ondalik <- if (max(x$DEGER) < 100 && any(x$DEGER %% 1 != 0)) 1 else 0

    aday[[length(aday) + 1]] <- data.frame(
      konu = m$konu,
      grup = m$grup,
      baslik = baslik,
      df_id = id,
      filtre = sub("^;", "", t$f),
      payda = NA,
      payda_df = NA,
      carpan = NA,
      birim = birim_ad,
      ondalik = ondalik,
      degisim = "yuzde",
      iyi = "notr",
      kaynak = paste0("TÜİK · ", k$kategori),
      not = if (nzchar(t$ad)) k$ad_tr else "",
      ayrisma = "hayir",
      sonraki = NA,
      sadece_tr = il_sayisi == 0 && bolge_sayisi == 0,
      il_sayisi = il_sayisi,
      bolge_sayisi = bolge_sayisi,
      tr_var = tr_var,
      ilk = min(x$TIME_PERIOD),
      son = max(x$TIME_PERIOD),
      birim_ham = birim,
      durum = "aday",
      stringsAsFactors = FALSE
    )
    n_ok <- n_ok + 1
  }

  if (n_ok == 0) atlandi(id, "gecerli_seri_yok_tek_gozlem_kosulu")

  cat(sprintf("[%d/%d] %-40s aday=%d\n", i, nrow(tar), id, n_ok))
  flush.console()
  rm(d)
  invisible(gc())
}


# --- çıktı -------------------------------------------------------------

if (length(aday)) {
  a <- do.call(rbind, aday)
  # Başlık yalnız etiket. Aynı grupta aynı başlık çıkarsa, çakışanlara tablo adının
  # başı eklenir; çakışma bitene kadar eklenen kısım 30'ar karakter uzatılır.
  a$baslik <- trimws(gsub("[[:space:]]+", " ", a$baslik))
  for (uz in c(30, 60, 90, 1000)) {
    anahtar <- paste(a$konu, a$grup, a$baslik)
    kopya <- duplicated(anahtar) | duplicated(anahtar, fromLast = TRUE)
    i_k <- which(kopya & nzchar(a$not))
    if (!length(i_k)) break
    ek <- vapply(a$not[i_k], function(s) {
      if (nchar(s) <= uz) s else paste0(sub("[[:space:]]+[^[:space:]]*$", "", substr(s, 1, uz)), "…")
    }, "")
    a$baslik[i_k] <- paste0(sub(" · .*$", "", a$baslik[i_k]), " · ", ek)
  }

  # --id verildiyse yalnız o tabloları yenile, diğerlerini koru
  if (length(sec) && file.exists(cik)) {
    eski <- csv_oku("ayar/otomatik_gosterge.csv", na.strings = character())
    eski <- eski[!eski$df_id %in% a$df_id, ]
    for (k in setdiff(names(a), names(eski))) eski[[k]] <- ""
    for (k in setdiff(names(eski), names(a))) a[[k]] <- ""
    a <- rbind(eski[, names(a)], a)
  }

  a <- karar_uygula(a)
  write.csv(a, cik, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  cat(sprintf("\nAday gösterge: %d (il düzeyli: %d, yalnız TR: %d)\n",
              nrow(a), sum(a$il_sayisi > 0), sum(a$sadece_tr)))
}

if (length(atla)) {
  z <- do.call(rbind, atla)
  write.csv(z, file.path(PORTAL, "veri/otomatik_atlanan.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  print(table(z$neden))
}
