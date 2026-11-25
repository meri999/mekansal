.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))

# ayar/gostergeler_il.csv + veri/ham/*.csv  ->  docs/veri/veri.json
# Kullanım (herhangi bir klasörden): Rscript betikler/05_gosterge_uret.R
#   --siki   herhangi bir gösterge üretilemezse hata koduyla çık

suppressMessages(library(jsonlite))

GEO_RE <- "^TR([0-9A-C]([0-9]{0,2}))?$"
NA_DEGER <- c("", "NA")

csv_oku <- function(yol, ...) {
  read.csv(file.path(PORTAL, yol), fileEncoding = "UTF-8",
           stringsAsFactors = FALSE, ...)
}


# --- gösterge tanımları ------------------------------------------------

tanim <- csv_oku("ayar/gostergeler_il.csv", na.strings = NA_DEGER)
tr_tanim <- csv_oku("ayar/gostergeler_tr.csv", na.strings = NA_DEGER)

for (k in setdiff(names(tanim), names(tr_tanim))) {
  tr_tanim[[k]] <- NA_character_
}
tr_tanim$ayrisma <- "hayir"

if (!"sik" %in% names(tanim)) tanim$sik <- NA_character_   # il tanımında "M" = aylık
tanim$sadece_tr <- FALSE
tr_tanim$sadece_tr <- TRUE
if (!"bolge" %in% names(tanim)) tanim$bolge <- NA_character_
tr_tanim$bolge <- NA_character_
tanim <- rbind(tanim, tr_tanim[, names(tanim)])

# Otomatik üretilen adaylar (05e): yalnız durum = "yayin" olanlar siteye girer
oto_yol <- file.path(PORTAL, "ayar/otomatik_gosterge.csv")
if (file.exists(oto_yol)) {
  oto <- csv_oku("ayar/otomatik_gosterge.csv", na.strings = NA_DEGER)
  oto <- oto[!is.na(oto$durum) & oto$durum == "yayin", ]
  if (nrow(oto)) {
    for (k in setdiff(names(tanim), names(oto))) oto[[k]] <- NA_character_
    oto$sik <- NA_character_
    oto$sadece_tr <- as.logical(oto$sadece_tr)
    oto$oto <- TRUE
    tanim$oto <- FALSE
    tanim <- rbind(tanim, oto[, names(tanim)])
  }
}
if (!"oto" %in% names(tanim)) tanim$oto <- FALSE


# --- yer adları (ayar/yer_adlari.csv) ----------------------------------

duzey_of <- function(k) {
  ifelse(k == "TR", "ulke",
         ifelse(nchar(k) == 3, "bolge",
                ifelse(nchar(k) == 4, "alt_bolge", "il")))
}

yer <- csv_oku("ayar/yer_adlari.csv")
yer <- yer[grepl(GEO_RE, yer$kod), ]
yer$duzey <- duzey_of(yer$kod)


# --- ham veri ve seri çıkarma ------------------------------------------

cache <- new.env()

ham <- function(id) {
  if (!is.null(cache[[id]])) return(cache[[id]])

  yol <- file.path(PORTAL, "veri/ham", paste0(id, ".csv"))
  if (!file.exists(yol)) {
    yol <- file.path(PORTAL, "veri/ham_ekonomi", paste0(id, ".csv"))
  }
  if (!file.exists(yol)) {
    yol <- file.path(PORTAL, "veri/ham_tum", paste0(id, ".csv.gz"))
    if (file.exists(yol)) yol <- gzfile(yol)
  }
  d <- read.csv(yol, colClasses = "character", check.names = FALSE,
                encoding = "UTF-8")

  # IKAMET_YERI yalnız gerçek yer kodu içeriyorsa kullanılır
  # (göç tablolarında "_Z" olabilir)
  g <- "REF_AREA"
  if ("IKAMET_YERI" %in% names(d) && any(grepl(GEO_RE, d$IKAMET_YERI))) {
    g <- "IKAMET_YERI"
  }
  d$GEO <- d[[g]]
  if (!"TR" %in% d$GEO) d$GEO[d$GEO == "_T"] <- "TR"
  d <- d[grepl(GEO_RE, d$GEO), ]
  d$DEGER <- suppressWarnings(as.numeric(d$OBS_VALUE))

  cache[[id]] <- d
  d
}

# "BOYUT=a+b;BOYUT2=c" -> list(BOYUT = c("a", "b"), BOYUT2 = "c")
filtre_ayir <- function(f) {
  if (is.na(f) || !nzchar(f)) return(list())
  p <- strsplit(strsplit(f, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)
  degerler <- lapply(p, function(x) {
    strsplit(paste(x[-1], collapse = "="), "+", fixed = TRUE)[[1]]
  })
  setNames(degerler, vapply(p, `[`, "", 1))
}

# Filtreye uyan seri: (GEO, TIME_PERIOD) başına tam olarak beklenen
# sayıda gözlem olmalı.
seri <- function(id, filtre) {
  d <- ham(id)
  fl <- filtre_ayir(filtre)
  for (b in names(fl)) {
    if (!b %in% names(d)) stop("boyut yok: ", b)
    d <- d[d[[b]] %in% fl[[b]], ]
  }
  d <- d[!is.na(d$DEGER), ]

  # "AY=M12": aylık stok serisinden yıl sonu (Aralık) değeri alınır;
  # dönem "2024-12" -> "2024" (yıllık seri, ADNKS nüfusuyla eşleşir)
  if (identical(fl$AY, "M12")) d$TIME_PERIOD <- substr(d$TIME_PERIOD, 1, 4)

  beklenen <- prod(vapply(fl, length, 1L))
  n <- tapply(d$DEGER, list(d$GEO, d$TIME_PERIOD), length)
  if (nrow(d) == 0) stop("filtre bos")

  # "+" ile birden çok kod seçildiyse en çok beklenen sayı kabul edilir
  esnek <- any(grepl("+", filtre, fixed = TRUE))
  sorunlu <- if (esnek) n > beklenen else n != beklenen
  if (any(sorunlu, na.rm = TRUE)) {
    stop(sprintf("belirsiz filtre: bir il-yilda %d gozlem var (beklenen %d)",
                 max(n, na.rm = TRUE), beklenen))
  }

  a <- aggregate(DEGER ~ GEO + TIME_PERIOD, d, sum)
  names(a) <- c("geo", "donem", "deger")
  a
}


# --- bölge (IBBS-1) türetme --------------------------------------------

# İlin kodunun ilk 3 karakteri bölge kodudur (TR100 -> TR1). Yalnız
# bölgenin TÜM illeri o yıl mevcutsa toplanır.
il_re <- "^TR[0-9A-C][0-9]{2}$"
il_sayi <- table(substr(yer$kod[grepl(il_re, yer$kod)], 1, 3))

bolge_topla <- function(a) {
  x <- a[grepl(il_re, a$geo), ]
  if (!nrow(x)) return(NULL)
  x$geo <- substr(x$geo, 1, 3)
  s <- aggregate(deger ~ geo + donem, x, sum)
  n <- aggregate(deger ~ geo + donem, x, length)
  s <- s[n$deger == il_sayi[s$geo], ]
  if (nrow(s)) s else NULL
}

# Türetilen bölge satırlarından, a'da zaten olanları çıkarır
yeni_olanlar <- function(bm, a) {
  bm[!paste(bm$geo, bm$donem) %in% paste(a$geo, a$donem), ]
}


# --- göstergeleri üret -------------------------------------------------

cikti <- list()
uyari <- character()

for (i in seq_len(nrow(tanim))) {
  t <- tanim[i, ]
  bolge_hesap <- FALSE

  r <- tryCatch({
    a <- seri(t$df_id, t$filtre)

    if (!is.na(t$payda)) {
      # oran göstergesi: pay / payda * çarpan
      payda_df <- if (is.na(t$payda_df)) t$df_id else t$payda_df
      b <- seri(payda_df, t$payda)
      m <- merge(a, b, by = c("geo", "donem"), suffixes = c("", "_p"))
      k <- if (is.na(t$carpan)) 100 else as.numeric(t$carpan)

      bn <- bolge_topla(data.frame(geo = m$geo, donem = m$donem,
                                   deger = m$deger))
      bp <- bolge_topla(data.frame(geo = m$geo, donem = m$donem,
                                   deger = m$deger_p))
      a <- data.frame(geo = m$geo, donem = m$donem,
                      deger = ifelse(m$deger_p == 0, NA,
                                     m$deger / m$deger_p * k))

      if (!is.null(bn) && !is.null(bp) && !isTRUE(t$bolge == "hayir")) {
        bm <- merge(bn, bp, by = c("geo", "donem"), suffixes = c("", "_p"))
        bm <- bm[bm$deger_p != 0, ]
        bm <- data.frame(geo = bm$geo, donem = bm$donem,
                         deger = bm$deger / bm$deger_p * k)
        bm <- yeni_olanlar(bm, a)
        if (nrow(bm)) {
          a <- rbind(a, bm)
          bolge_hesap <- TRUE
        }
      }
    } else {
      if (!is.na(t$carpan)) a$deger <- a$deger * as.numeric(t$carpan)

      if (isTRUE(t$bolge == "topla")) {
        bm <- bolge_topla(a)
        if (!is.null(bm)) {
          bm <- yeni_olanlar(bm, a)
          if (nrow(bm)) {
            a <- rbind(a, bm)
            bolge_hesap <- TRUE
          }
        }
      }
    }

    a <- a[!is.na(a$deger), ]
    if (isTRUE(t$sadece_tr)) a <- a[a$donem >= "2010", ]

    yillar <- sort(unique(a$donem))
    kodlar <- sort(unique(a$geo))
    v <- lapply(setNames(kodlar, kodlar), function(k) {
      x <- a[a$geo == k, ]
      I(round(x$deger[match(yillar, x$donem)], as.numeric(t$ondalik) + 3))
    })

    # "sonraki": elle güncellenen (bülten) göstergeler için
    sonraki <- NULL
    if ("sonraki" %in% names(t) && !is.na(t$sonraki)) sonraki <- t$sonraki

    list(
      id = sprintf("g%02d", i),
      konu = t$konu,
      grup = t$grup,
      baslik = t$baslik,
      birim = t$birim,
      ondalik = as.numeric(t$ondalik),
      degisim = t$degisim,
      iyi = t$iyi,
      ayrisma = t$ayrisma,
      kaynak = t$kaynak,
      not = if (is.na(t$not)) NULL else t$not,
      df_id = t$df_id,
      sonraki = sonraki,
      sik = if (is.na(t$sik)) NULL else t$sik,
      bolge_hesap = if (bolge_hesap) TRUE else NULL,
      oto = if (isTRUE(t$oto)) TRUE else NULL,
      sadece_tr = if (isTRUE(t$sadece_tr)) TRUE else NULL,
      yillar = I(yillar),
      veri = v
    )
  }, error = function(e) {
    uyari <<- c(uyari, sprintf("%02d %s: %s", i, t$baslik,
                               conditionMessage(e)))
    NULL
  })

  if (!is.null(r)) cikti[[length(cikti) + 1]] <- r
}


# --- çıktı -------------------------------------------------------------

kullanilan <- unique(unlist(lapply(cikti, function(g) names(g$veri))))
yer <- yer[yer$kod %in% kullanilan, ]
yer <- yer[order(yer$duzey, yer$kod), ]

dir.create(file.path(SITE, "veri"), recursive = TRUE, showWarnings = FALSE)
write_json(list(uretim = format(Sys.time(), "%Y-%m-%d %H:%M"),
                yerler = yer, gostergeler = cikti),
           file.path(SITE, "veri/veri.json"),
           auto_unbox = TRUE, na = "null", digits = NA, null = "null")

boyut_kb <- file.size(file.path(SITE, "veri/veri.json")) / 1024
cat(sprintf("Uretilen gosterge: %d/%d | dosya: %.0f KB\n",
            length(cikti), nrow(tanim), boyut_kb))
if (length(uyari)) {
  cat("\nATLANAN GOSTERGELER:\n", paste(uyari, collapse = "\n"), "\n")
}

# özet: her gösterge için yıl aralığı, il sayısı, TR ve İstanbul'un son değeri
for (g in cikti) {
  ist <- g$veri[["TR100"]]
  tr <- g$veri[["TR"]]
  son <- length(g$yillar)
  cat(sprintf("%s %-48s %s-%s  il=%d  TR=%s  Ist=%s\n",
              g$id, substr(g$baslik, 1, 48), g$yillar[1], g$yillar[son],
              sum(grepl("^TR[0-9A-C][0-9]{2}$", names(g$veri))),
              if (is.null(tr)) "-" else tr[son],
              if (is.null(ist)) "-" else ist[son]))
}

# --siki: herhangi bir gösterge üretilemediyse hata ile çık
# (GitHub Actions bozuk veriyi yayınlamasın)
siki <- "--siki" %in% commandArgs(trailingOnly = TRUE)
if (siki && (length(uyari) || length(cikti) < nrow(tanim))) {
  cat("SIKI MOD: gosterge atlandi, cikiliyor\n")
  quit(status = 4)
}
