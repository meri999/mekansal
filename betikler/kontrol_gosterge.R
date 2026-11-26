.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))

# Üretilen göstergeleri denetler (başlık, etiket, birim, seri doluluğu, kopya).
# Kullanım: Rscript betikler/kontrol_gosterge.R [site] [karsilastirilacak_site]
#   site                    varsayılan "docs"
#   karsilastirilacak_site  verilirse iki sitenin gösterge kümesi aynı mı diye bakılır
# HATA varsa çıkış kodu 1 olur (yayından önce çalıştırın).

suppressMessages(library(jsonlite))

arg <- commandArgs(TRUE)
site <- if (length(arg) >= 1) arg[1] else "docs"
diger <- if (length(arg) >= 2) arg[2] else NA

oku <- function(s) {
  fromJSON(file.path(PORTAL, s, "veri", "veri.json"), simplifyVector = FALSE)$gostergeler
}

hata <- character()
uyari <- character()
H <- function(g, m) hata <<- c(hata, sprintf("[%s] %s: %s", g$id, substr(g$baslik, 1, 70), m))
U <- function(g, m) uyari <<- c(uyari, sprintf("[%s] %s: %s", g$id, substr(g$baslik, 1, 70), m))

dolu <- function(v) sum(!vapply(v, is.null, TRUE))

G <- oku(site)
cat(sprintf("%s: %d gösterge\n", site, length(G)))

anahtar <- character()
for (g in G) {
  dz <- if (isTRUE(g$sadece_tr)) "T" else "I"
  anahtar <- c(anahtar, paste(g$konu, g$grup, g$baslik, dz, sep = "|"))

  # --- başlık / etiket ---
  b <- g$baslik
  if (is.null(b) || !nzchar(trimws(b))) { H(g, "başlık boş"); next }
  if (grepl("^[A-Z0-9_]{4,}$", b)) H(g, "başlık ham kod")
  if (grepl("(^|, )kod [0-9A-Za-z_]+", b)) U(g, "çözülmemiş etiket kodu (kod X)")
  if (grepl("\\((\\s*[0-9]+|_[A-Z])\\s*\\)\\s*$", b)) U(g, "başlık sonunda ham kod")
  if (grepl("[A-Z]{3,}_[A-Z_]+", b)) U(g, "başlıkta ham kod benzeri ifade")
  if (nchar(b) > 100) U(g, sprintf("başlık uzun (%d karakter)", nchar(b)))
  if (grepl("\\s{2,}|^\\s|\\s$", b)) U(g, "başlıkta fazla boşluk")

  # --- birim / alanlar ---
  if (is.null(g$birim)) H(g, "birim alanı boş (null)")
  else if (!nzchar(g$birim)) U(g, "birim yok")
  for (a in c("konu", "grup", "kaynak", "degisim")) {
    if (is.null(g[[a]]) || !nzchar(g[[a]])) H(g, paste("alan eksik:", a))
  }

  # --- seri ---
  y <- unlist(g$yillar)
  if (!length(y)) { H(g, "yıl listesi boş"); next }
  top <- 0
  for (k in names(g$veri)) {
    v <- g$veri[[k]]
    if (length(v) != length(y)) H(g, sprintf("%s serisi yıl sayısıyla uyumsuz", k))
    top <- top + dolu(v)
  }
  if (top == 0) H(g, "hiç değer yok (boş seri)")
  if (isTRUE(g$sadece_tr)) {
    tr <- g$veri[["TR"]]
    n <- if (is.null(tr)) 0 else dolu(tr)
    if (n == 0) H(g, "Türkiye serisi boş")
    else if (n < 2) H(g, "Türkiye serisi tek gözlemli (grafik çizilemez)")
  }
}

# --- aynı grupta tekrar eden başlık ---
tek <- table(anahtar)
for (k in names(tek)[tek > 1]) hata <- c(hata, sprintf("aynı başlık %d kez: %s", tek[[k]], k))

# --- il görünümünde boş kalan alt kategori (bilgi) ---
bol <- c(nufus = "nufus", egitim = "egitim", kalkinma = "kalkinma", yasam = "yasam", goc = "goc")
grup_il <- list(); grup_tr <- list()
for (g in G) {
  k <- paste(if (!is.null(bol[g$konu]) && !is.na(bol[g$konu])) bol[g$konu] else "ekonomi", g$grup, sep = "|")
  if (isTRUE(g$sadece_tr)) grup_tr[[k]] <- TRUE else grup_il[[k]] <- TRUE
}
sadece <- setdiff(names(grup_tr), names(grup_il))
bilgi <- sprintf("yalnız Türkiye düzeyli alt kategori: %d (il görünümünde menüde boş görünür, tıklanınca Türkiye'ye geçer)", length(sadece))

# --- kart kalitesi kapıları (betikler/kart_denetim.R) ---
# Birim-etiket çelişkisi (U1-U8), kopya veri (K1) ve çözülmemiş kod etiketi (B1)
# otomatik kartlarda HATA sayılır; diğer işaretler (başlık biçimi, kesik başlık vb.) UYARI.
if (is.na(diger) || TRUE) {
  gizli <- capture.output(source(file.path(dirname(normalizePath(.f, winslash = "/")), "kart_denetim.R"),
                                 local = new.env()))
  kd <- read.csv(file.path(PORTAL, "veri", "kart_denetim.csv"), fileEncoding = "UTF-8",
                 colClasses = "character")
  agir <- kd$oto == "TRUE" & grepl("(^|[|] )(U[1-8]|K1|B1) ", kd$sorun)
  for (i in which(agir)) {
    hata <- c(hata, sprintf("[%s] %s: %s", kd$id[i], substr(kd$baslik[i], 1, 70), kd$sorun[i]))
  }
  hafif <- kd$sorun_sayisi != "0" & !agir
  uyari <- c(uyari, sprintf("[%s] %s: kart denetimi işareti: %s", kd$id[hafif], substr(kd$baslik[hafif], 1, 60), kd$sorun[hafif]))
}

# --- iki sitenin gösterge kümesi ---
if (!is.na(diger)) {
  G2 <- oku(diger)
  ak <- function(L) vapply(L, function(g) paste(g$konu, g$grup, g$baslik, isTRUE(g$sadece_tr), sep = "|"), "")
  a1 <- ak(G); a2 <- ak(G2)
  cat(sprintf("%s: %d gösterge\n", diger, length(G2)))
  if (!setequal(a1, a2) || length(a1) != length(a2)) {
    hata <- c(hata, sprintf("%s ve %s gösterge kümeleri farklı (yalnız %s: %d, yalnız %s: %d)",
                            site, diger, site, length(setdiff(a1, a2)), diger, length(setdiff(a2, a1))))
  } else cat("iki sitenin gösterge kümesi aynı\n")
}

cat(sprintf("\nHATA: %d | UYARI: %d\n%s\n", length(hata), length(uyari), bilgi))
if (length(hata)) { cat("\n--- HATA ---\n"); cat(head(hata, 40), sep = "\n") }
if (length(uyari)) {
  cat("\n--- UYARI (ilk 25) ---\n"); cat(head(uyari, 25), sep = "\n")
  tur <- sub("^.*?: ", "", uyari)
  cat("\nUyarı türleri:\n"); print(sort(table(sub(" \\(.*$", "", tur)), decreasing = TRUE))
}
if (length(hata)) quit(status = 1)
