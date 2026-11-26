# Otomatik gösterge başlıklarını kurmak için yardımcılar (05e tarafından yüklenir).
# Başlık = ölçü adı · kırılım. Ölçü adı ayar/etiket_sozluk.csv'den, kırılım etiketi TÜİK
# kod listesinden gelir ve burada temizlenir (Title Case -> cümle düzeni, birim parantezleri).

# Türkçe büyük/küçük harf (i/İ, ı/I) ayrımı
tr_lower <- function(x) {
  x <- gsub("İ", "i", x, fixed = TRUE)
  x <- gsub("I", "ı", x, fixed = TRUE)
  tolower(x)
}

tr_ilk_buyuk <- function(x) {
  vapply(x, function(s) {
    if (is.na(s) || !nzchar(s)) return(s)
    c1 <- substr(s, 1, 1)
    u <- if (c1 == "i") "İ" else if (c1 == "ı") "I" else toupper(c1)
    paste0(u, substring(s, 2))
  }, "", USE.NAMES = FALSE)
}

KORUNAN_KELIME <- c("Türkiye", "İBBS", "TÜİK", "Ar-Ge", "ADNKS", "ISCED")

# "Ücretli Çalışanların Yıllık Ortalama Sayısı" -> "Ücretli çalışanların yıllık ortalama sayısı".
# Yalnız her kelimesi büyük harfle başlayan (TÜİK biçimi) etiketlere dokunur;
# kısaltmalar (DNA, NOx, CO2, (DMI)) ve özel adlar korunur.
cumle_duzeni <- function(x) {
  if (is.na(x) || !nzchar(x)) return(x)
  w <- strsplit(x, " ", fixed = TRUE)[[1]]
  uz <- nchar(w) > 2
  cap <- grepl("^[(]?[A-ZÇĞİÖŞÜ]", w) & uz
  if (sum(uz) < 2 || sum(cap) < 2 || sum(cap) / sum(uz) < 0.6) return(x)
  korun <- function(s) {
    s %in% KORUNAN_KELIME || grepl("[0-9]", s) ||
      nchar(gsub("[^A-ZÇĞİÖŞÜ]", "", s)) >= 2 ||
      grepl("^[(]?[A-ZÇĞİÖŞÜ][)]?$", s)
  }
  # "Algı/Kabul" gibi bölü işaretli sözcükleri parça parça işle
  duzelt <- function(s) {
    if (!grepl("/", s, fixed = TRUE)) return(if (korun(s)) s else tr_lower(s))
    p <- strsplit(s, "/", fixed = TRUE)[[1]]
    paste(vapply(p, function(q) if (korun(q)) q else tr_lower(q), "", USE.NAMES = FALSE), collapse = "/")
  }
  w2 <- vapply(w, duzelt, "", USE.NAMES = FALSE)
  out <- paste(w2, collapse = " ")
  out <- gsub("rdna", "rDNA", out, fixed = TRUE)
  out <- gsub("(dna", "(DNA", out, fixed = TRUE)
  tr_ilk_buyuk(out)
}

# Etiketten ölçü birimi parantezlerini (birim zaten birim alanında) ve "cinsiyet:" önekini atar.
kirilim_temizle <- function(x) {
  if (is.na(x) || !nzchar(x)) return("")
  x <- sub("^cinsiyet: ", "", x)
  x <- gsub(" ", " ", x, fixed = TRUE)
  x <- gsub("[[:space:]]*[(](%|binde|yüz binde|tl|milyon tl|milyon|[$])[)]", "", x, ignore.case = TRUE)
  x <- gsub("[[:space:]]%[[:space:]]", " ", x)
  x <- gsub("[(][[:space:]]+", "(", x)
  x <- gsub("[[:space:]]+[)]", ")", x)
  x <- trimws(gsub("[[:space:]]+", " ", x))
  cumle_duzeni(x)
}

# Sözlük satırına göre başlık kurar.
#   raw   : kırılım etiketi (TÜİK kod listesinden; boşsa kart tablonun toplamıdır)
#   tablo : tablo adı (etiket bulunmayan kartlar için)
#   s     : etiket_sozluk.csv satırı (list/data.frame satırı) ya da NULL
baslik_kur <- function(raw, tablo, s) {
  lab <- kirilim_temizle(raw)
  var <- nzchar(lab)
  mod <- if (is.null(s)) "E" else s$mod
  olcu <- if (is.null(s)) "" else s$olcu
  if (mod == "S" && nzchar(olcu)) return(olcu)
  if (mod == "O" && nzchar(olcu)) {
    if (!var) return(olcu)
    if (!is.null(s$toplam) && nzchar(s$toplam) && grepl(s$toplam, raw, perl = TRUE)) return(olcu)
    if (grepl("^[0-9]+-[0-9]+$", lab)) lab <- paste0(lab, " yaş")
    return(paste0(olcu, " · ", lab))
  }
  if (mod %in% c("P", "R") && var) {
    p <- strsplit(raw, ", ", fixed = TRUE)[[1]]
    if (length(p) >= 2) {
      p1 <- kirilim_temizle(p[1])
      p2 <- kirilim_temizle(paste(p[-1], collapse = ", "))
      if (mod == "P") return(paste0(olcu, " · ", p2, " (", tr_lower(p1), ")"))
      return(paste0(p2, " · ", tr_lower(p1)))
    }
  }
  # E (ve varsayılan): etiketin kendisi ölçüdür; etiket yoksa tablo adı
  if (var) lab else cumle_duzeni(trimws(gsub("[[:space:]]+", " ", tablo)))
}
