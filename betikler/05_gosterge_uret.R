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
  # elle tanımlı tablolar zaten var; eski taramadan kalan otomatik kopyaları ele
  oto <- oto[!oto$df_id %in% tanim$df_id, ]
  if (!"etiket_ham" %in% names(tanim)) tanim$etiket_ham <- NA_character_
  if (!"baglam" %in% names(tanim)) tanim$baglam <- NA_character_
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

# --- birim çıkarımı (yalnız otomatik göstergeler için) -------------------
# Sıra: tanımdaki birim > ayar/birim_kurallari.csv (tablo + etiket) > etiketten tahmin.
# ÖNEMLİ: eşleştirme yalnız ETİKET üzerinde yapılır (başlığın " · " öncesi). Tablo adı
# (not) hiçbir zaman eşleştirmeye girmez; tablo adı kuralları çarpıştırıyordu.
# Kural dosyası sütunları: tablo (df_id regex, boş=hepsi), desen (etiket regex, boş=hepsi),
# birim, ek_not.
birim_kurallari <- NULL
if (file.exists(file.path(PORTAL, "ayar/birim_kurallari.csv"))) {
  birim_kurallari <- csv_oku("ayar/birim_kurallari.csv", na.strings = character())
}

# Karttaki ölçü etiketi: " · " öncesi. "Erkek / cinsiyet: Kadın" gibi çıplak
# kırılım etiketlerinde asıl ölçü " · " sonrasındaki (kesik) tablo adıdır.
etiket_cikar <- function(baslik) {
  parca <- strsplit(baslik, " · ", fixed = TRUE)[[1]]
  e <- parca[1]
  if (length(parca) > 1 && grepl("^(cinsiyet: )?(erkek|kadın)$", tolower(e))) e <- parca[2]
  e
}

birim_kural <- function(df_id, etiket) {
  if (is.null(birim_kurallari)) return(NULL)
  for (j in seq_len(nrow(birim_kurallari))) {
    t_ok <- !nzchar(birim_kurallari$tablo[j]) ||
      grepl(birim_kurallari$tablo[j], df_id, perl = TRUE)
    d_ok <- !nzchar(birim_kurallari$desen[j]) ||
      grepl(birim_kurallari$desen[j], etiket, perl = TRUE, ignore.case = TRUE)
    if (t_ok && d_ok) {
      return(list(birim = birim_kurallari$birim[j], ek_not = birim_kurallari$ek_not[j]))
    }
  }
  NULL
}

# Etiketten tahmin: sıra önemlidir (ilk eşleşen kazanır). Eşleşme yoksa "" döner.
BIRIM_TAHMIN <- list(
  c("\\(milyon tl\\)", "milyon TL"),
  c("\\(tl\\)|\\btl\\b|\\bciro\\b|ciro\\(", "TL"),
  c("çocuk sayısı\\)", "çocuk"),
  c("yüz binde", "yüz binde"),
  c("\\(%\\)|%|oranı|(^|[^a-zçğıöşü])payı|\\boran\\b", "%"),
  c("yoğunluğu|yoğunluk", "kişi/km²"),
  c("binde|hızı", "‰"),
  c("boşanma", "boşanma"),
  c("evlenme|evlilik|evlenen", "evlenme"),
  c("doğum sayısı|doğumlar|doğum$|doğumların", "doğum"),
  c("ölüm sayısı|ölümler|ölüm$", "ölüm"),
  c("(^|[^a-zçğıöşü])ölü([^a-zçğıöşü]|$)|ölen|intihar|yaralı", "kişi"),
  c("iş bulma süresi", "ay"),
  c("süre", "yıl"),
  c("ortanca yaş|ortalama yaş|anne yaşı", "yaş"),
  c("endeks", "endeks"),
  c("hanehalkı|hane sayısı", "hane"),
  c("çalışan|istihdam|personel|işçi", "kişi"),
  c("girişim", "girişim"),
  c("nüfus|kişi|mezun", "kişi"),
  c("kaza", "kaza"),
  c("sayısı|adet|belediye|tesis", "adet"))

birim_tahmin <- function(etiket) {
  s <- tolower(etiket)
  for (k in BIRIM_TAHMIN) if (grepl(k[1], s, perl = TRUE)) return(k[2])
  ""
}

# TÜİK'in kendi birim etiketlerini tek biçime getirir.
birim_duzelt <- function(b) {
  d <- c("Sayı" = "adet", "Yüzde" = "%", "Ton" = "ton", "Bin Ton" = "bin ton",
         "Bin Metreküp" = "bin m³", "Endeks" = "endeks")
  if (b %in% names(d)) unname(d[b]) else b
}

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

    # otomatik adaylarda ondalık boş olabilir: veriden türet (NA olursa tüm seri boş çıkar)
    # birim: tanımdaki > ayar/birim_kurallari.csv > başlıktan tahmin
    birim_bos <- is.na(t$birim) || !nzchar(t$birim) || (isTRUE(t$oto) && t$birim == "Sayı")
    etk <- if (isTRUE(t$oto)) { eh <- t$etiket_ham; if (!is.na(eh) && nzchar(eh)) eh else etiket_cikar(t$baslik) } else ""
    kural <- if (isTRUE(t$oto) && birim_bos) birim_kural(t$df_id, etk) else NULL
    birim_son <- if (!birim_bos) t$birim else if (!is.null(kural)) kural$birim else if (isTRUE(t$oto)) birim_tahmin(etk) else ""
    if (isTRUE(t$oto) && nzchar(birim_son)) birim_son <- birim_duzelt(birim_son)
    not_son <- if (is.na(t$not)) NULL else t$not
    if (!is.null(kural) && nzchar(kural$ek_not)) not_son <- paste(c(not_son, kural$ek_not), collapse = " ")

    ond <- suppressWarnings(as.numeric(t$ondalik))
    if (is.na(ond)) ond <- if (nrow(a) && max(abs(a$deger)) < 100 && any(a$deger %% 1 != 0)) 2 else 0

    if (isTRUE(t$sadece_tr) && length(unique(a$donem[a$geo == "TR"])) < 2) stop("Turkiye serisi yok ya da tek gozlem: grafik cizilemez")
    yillar <- sort(unique(a$donem))
    kodlar <- sort(unique(a$geo))
    v <- lapply(setNames(kodlar, kodlar), function(k) {
      x <- a[a$geo == k, ]
      I(round(x$deger[match(yillar, x$donem)], ond + 3))
    })

    # "sonraki": elle güncellenen (bülten) göstergeler için
    sonraki <- NULL
    if ("sonraki" %in% names(t) && !is.na(t$sonraki)) sonraki <- t$sonraki

    list(
      id = sprintf("g%02d", i),
      konu = t$konu,
      grup = t$grup,
      baslik = t$baslik,
      birim = birim_son,
      ondalik = ond,
      degisim = t$degisim,
      iyi = t$iyi,
      ayrisma = t$ayrisma,
      kaynak = t$kaynak,
      not = not_son,
      df_id = t$df_id,
      sonraki = sonraki,
      sik = if (is.na(t$sik)) NULL else t$sik,
      bolge_hesap = if (bolge_hesap) TRUE else NULL,
      oto = if (isTRUE(t$oto)) TRUE else NULL,
      sadece_tr = if (isTRUE(t$sadece_tr)) TRUE else NULL,
      yillar = I(yillar),
      baglam_gecici = if (isTRUE(t$oto) && !is.na(t$baglam)) t$baglam else "",
      tablo_adi_gecici = if (isTRUE(t$oto) && !is.na(t$not)) t$not else "",
      veri = v
    )
  }, error = function(e) {
    uyari <<- c(uyari, sprintf("%02d %s: %s", i, t$baslik,
                               conditionMessage(e)))
    NULL
  })

  if (!is.null(r)) cikti[[length(cikti) + 1]] <- r
}

# Birimi hâlâ boş otomatik kart (örn. "Erkek · tablo adı" kırılımları): aynı tablonun
# birimi olan kartlarının en sık birimini devral.
for (i in seq_along(cikti)) {
  g <- cikti[[i]]
  if (!isTRUE(g$oto) || nzchar(g$birim)) next
  kardes <- vapply(cikti, function(h) if (identical(h$df_id, g$df_id) && nzchar(h$birim)) h$birim else "", "")
  kardes <- kardes[nzchar(kardes)]
  if (length(kardes)) cikti[[i]]$birim <- names(sort(table(kardes), decreasing = TRUE))[1]
}

# --- kalite kapıları (yalnız otomatik göstergeler elenir) -----------------
# 1) Sabit seri: bütün yıllarda aynı değer (örn. her yıl %100) bilgi taşımaz.
# 2) Kopya veri: aynı yıllar ve aynı değerler başka kartta var. Elle tanımlı kart
#    varsa otomatik kopyalar düşer; yoksa en kısa başlıklı otomatik kart kalır.
# Elenenler veri/elenen_kartlar.csv'ye yazılır (neden ve tutulan kart).
seri_imza <- function(g) {
  paste(c(unlist(g$yillar),
          vapply(g$veri, function(v) paste(signif(unlist(v), 7), collapse = ","), "")),
        collapse = "|")
}
imzalar <- vapply(cikti, seri_imza, "")
oto_mu <- vapply(cikti, function(g) isTRUE(g$oto), TRUE)
sabit <- vapply(cikti, function(g) {
  v <- unlist(g$veri[["TR"]]); v <- v[!is.na(v)]
  isTRUE(g$oto) && length(v) >= 3 && length(unique(v)) == 1
}, TRUE)
at <- rep(FALSE, length(cikti)); neden <- rep("", length(cikti)); tutulan <- rep("", length(cikti))
at[sabit] <- TRUE; neden[sabit] <- "sabit seri"
for (s in unique(imzalar[duplicated(imzalar) | duplicated(imzalar, fromLast = TRUE)])) {
  ix <- which(imzalar == s & !at)
  if (length(ix) < 2) next
  elle <- ix[!oto_mu[ix]]
  if (length(elle)) {
    kal <- elle[1]
  } else {
    bs <- vapply(cikti[ix], function(g) nchar(g$baslik), 1L)
    kal <- ix[which.min(bs)]
  }
  dus <- setdiff(ix[oto_mu[ix]], kal)
  at[dus] <- TRUE; neden[dus] <- "kopya veri"; tutulan[dus] <- cikti[[kal]]$id
}
# 3) a kartının bütün yerleri (Türkiye/il/bölge) b kartında da var ve ortak yıllardaki
#    (en az 3) değerler birebir aynıysa a, b'nin kopyasıdır (örn. farklı tablolardan
#    gelen toplam nüfus). Elle tanımlı kart kalır; yoksa kısa başlıklı otomatik kart.
tum_seri <- lapply(cikti, function(g) {
  y <- unlist(g$yillar)
  lapply(g$veri, function(v) setNames(signif(unlist(v), 7), y))
})
kopya_mi <- function(a, b) {
  sa <- tum_seri[[a]]; sb <- tum_seri[[b]]
  if (!length(sa) || !all(names(sa) %in% names(sb))) return(FALSE)
  degisken <- FALSE
  for (k in names(sa)) {
    ort <- intersect(names(sa[[k]]), names(sb[[k]]))
    va <- sa[[k]][ort]; vb <- sb[[k]][ort]
    ok <- !is.na(va) & !is.na(vb)
    if (sum(ok) < 3 && k == "TR") return(FALSE)
    if (sum(ok) && any(abs(va[ok] - vb[ok]) > 1e-4 * pmax(1, abs(va[ok])))) return(FALSE)
    if (sum(ok) >= 3 && length(unique(va[ok])) > 1) degisken <- TRUE
  }
  degisken
}
for (a in which(oto_mu & !at)) {
  if (at[a]) next
  for (b in seq_along(cikti)) {
    if (b == a || at[b]) next
    if (oto_mu[b] && nchar(cikti[[b]]$baslik) > nchar(cikti[[a]]$baslik)) next
    if (kopya_mi(a, b)) {
      at[a] <- TRUE; neden[a] <- "ortak yıllarda aynı değerler"; tutulan[a] <- cikti[[b]]$id; break
    }
  }
}
if (any(at)) {
  elenen <- data.frame(
    id = vapply(cikti[at], function(g) g$id, ""),
    baslik = vapply(cikti[at], function(g) g$baslik, ""),
    grup = vapply(cikti[at], function(g) g$grup, ""),
    neden = neden[at], tutulan_kart = tutulan[at])
  write.csv(elenen, file.path(PORTAL, "veri/elenen_kartlar.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  cat(sprintf("Kalite kapısı: %d otomatik kart elendi (sabit seri: %d, kopya veri: %d)\n",
              sum(at), sum(neden[at] == "sabit seri"), sum(neden[at] == "kopya veri")))
  cikti <- cikti[!at]
}

# --- başlık çakışmalarını çöz (yalnız otomatik kartlar ek alır) ----------
# Aynı konu+grup+başlık (aynı düzeyde) iki kartta varsa: önce "(bağlam)" (sözlükteki kısa
# tanım, örn. "bankalar"), hâlâ çakışıyorsa tablo adının başı (30, 60, 90 karakter...).
bas_anahtar <- function(L) vapply(L, function(g) paste(g$konu, g$grup, g$baslik, isTRUE(g$sadece_tr)), "")
carpisan <- function() {
  k <- bas_anahtar(cikti)
  which(duplicated(k) | duplicated(k, fromLast = TRUE))
}
for (i in carpisan()) {
  g <- cikti[[i]]
  if (isTRUE(g$oto) && nzchar(g$baglam_gecici)) cikti[[i]]$baslik <- paste0(g$baslik, " (", g$baglam_gecici, ")")
}
baz <- vapply(cikti, function(g) g$baslik, "")
for (uz in c(30, 60, 90, 1000)) {
  i_k <- carpisan()
  i_k <- i_k[vapply(cikti[i_k], function(g) isTRUE(g$oto) && nzchar(g$tablo_adi_gecici), TRUE)]
  if (!length(i_k)) break
  for (i in i_k) {
    s <- cikti[[i]]$tablo_adi_gecici
    ek <- if (nchar(s) <= uz) s else paste0(sub("[[:space:]]+[^[:space:]]*$", "", substr(s, 1, uz)), "…")
    cikti[[i]]$baslik <- paste0(baz[i], " · ", ek)
  }
}
cikti <- lapply(cikti, function(g) { g$baglam_gecici <- NULL; g$tablo_adi_gecici <- NULL; g })


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
