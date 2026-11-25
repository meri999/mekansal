# Yol ayarları: betikler hangi klasörden çalıştırılırsa çalıştırılsın
# doğru dosyaları bulur. Her betik en başta şu iki satırla bu dosyayı yükler:
#   .f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
#   source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))
#
# PORTAL : bu portalın kök klasörü (betikler/, ayar/, docs/ depoda; veri/ yalnız bilgisayarda).
# Portal tek başına çalışır; başka bir projeye bağımlı değildir.
# API anahtarı PORTAL/.Renviron içinde (TUIK_API_KEY=...), git'e girmez.

# Etkileşimli oturumda --file bulunmaz
if (!exists(".f") || is.na(.f)) {
  .f <- file.path(getwd(), "betikler", "x.R")
}
PORTAL <- dirname(dirname(normalizePath(.f, winslash = "/", mustWork = FALSE)))

for (.k in c(file.path(PORTAL, "ayar"))) {
  if (!dir.exists(.k)) {
    stop("Portal klasoru bulunamadi: ", .k,
         "\n(Betik ", PORTAL, " altinda calisiyor)")
  }
}
if (!file.exists(file.path(PORTAL, "ayar", "katalog.csv"))) {
  stop("ayar/katalog.csv bulunamadi: ", file.path(PORTAL, "ayar"))
}

# SITE : site klasörü. PORTAL_SITE ortam değişkeni (PORTAL'a göre) ile
# değiştirilebilir; varsayılan "site".
SITE <- file.path(PORTAL, "docs")
if (nzchar(Sys.getenv("PORTAL_SITE"))) {
  SITE <- file.path(PORTAL, Sys.getenv("PORTAL_SITE"))
}
