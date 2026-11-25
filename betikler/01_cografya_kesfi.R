.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))

# Katalogdaki her TÜİK tablosunun boyutlarını tarar; il/bölge (IBBS)
# kırılımı olanları işaretler.
# Çalıştırma (herhangi bir klasörden):
#   Rscript betikler/01_cografya_kesfi.R <parça> <parça_sayısı>

source(file.path(PORTAL, "betikler", "tuik_api.R"))
library(xml2)


# --- girdiler ----------------------------------------------------------

kat <- read.csv(file.path(PORTAL, "ayar/katalog.csv"),
                fileEncoding = "UTF-8", stringsAsFactors = FALSE)

arg <- commandArgs(trailingOnly = TRUE)
parca <- as.integer(arg[1])
n_parca <- as.integer(arg[2])

cikti <- sprintf(file.path(PORTAL, "veri/kesif_parca%d.csv"), parca)
ana <- file.path(PORTAL, "veri/kesif_cografya.csv")

# Önceki parçalarda taranmış tablolar tekrar istenmez
onceki <- c(ana, list.files(file.path(PORTAL, "veri"), "^kesif_parca.*csv$",
                            full.names = TRUE))
yapilan <- do.call(rbind, lapply(onceki, function(f) {
  x <- read.csv(f, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  x[, "id", drop = FALSE]
}))

atla <- readLines(file.path(PORTAL, "ayar/atlanan_kategoriler.txt"),
                  encoding = "UTF-8")
kat <- kat[!kat$kategori %in% atla, ]
kat <- kat[!kat$id %in% yapilan$id, ]
kat <- kat[seq_len(nrow(kat)) %% n_parca == parca - 1, ]


# --- yardımcı fonksiyonlar ---------------------------------------------

# İsteği en çok n kez dener, aralarda bekler
tekrar <- function(f, n = 3) {
  for (k in 1:n) {
    s <- tryCatch(f(), error = \(e) e)
    if (!inherits(s, "error")) return(s)
    Sys.sleep(10 * k)
  }
  NULL
}

# Tablonun kod listesini (Enumeration) boyut boyut okur
kod_listesi <- function(dims) {
  vapply(dims, \(d) {
    e <- xml_find_first(d, ".//*[local-name()='Enumeration']/*")
    if (is.na(xml_attr(e, "id"))) "" else xml_attr(e, "id")
  }, "")
}


# --- tarama ------------------------------------------------------------

tok <- tuik_token()
t0 <- Sys.time()

for (i in seq_len(nrow(kat))) {
  # Token iki dakikada bir yenilenir
  if (difftime(Sys.time(), t0, units = "mins") > 2) {
    tok <- tuik_token()
    t0 <- Sys.time()
  }

  id <- kat$id[i]
  yol <- sprintf("dataflow/TR/%s/%s?references=children", id, kat$ver[i])
  r <- tekrar(\() tuik_get(yol, tok))

  satir <- data.frame(id = id, ad_tr = kat$ad_tr[i],
                      kategori = kat$kategori[i], durum = "hata",
                      cografya_boyut = NA, kod_listesi = NA,
                      tum_boyutlar = NA)

  if (!is.null(r) && resp_status(r) == 200) {
    x <- read_xml(resp_body_string(r))
    dims <- xml_find_all(x, paste0(
      "//*[local-name()='DataStructure']",
      "//*[local-name()='DimensionList']",
      "/*[local-name()='Dimension']"))
    id_d <- xml_attr(dims, "id")
    cl <- kod_listesi(dims)

    cog <- grepl("IBBS|^CL_IL$|^CL_IL_|CL_NUTS|CL_BOLGE", cl) |
      id_d %in% c("REF_AREA", "IKAMET_YERI", "GEO", "IL", "LEVEL")
    cog <- cog & !grepl("^CL_(YIL|YAS)", cl)

    satir$durum <- "ok"
    satir$cografya_boyut <- paste(id_d[cog], collapse = ";")
    satir$kod_listesi <- paste(cl[cog], collapse = ";")
    satir$tum_boyutlar <- paste(id_d, collapse = ";")
  }

  write.table(satir, cikti, sep = ",", row.names = FALSE,
              append = file.exists(cikti), col.names = !file.exists(cikti),
              fileEncoding = "UTF-8")
  cat(sprintf("[%d/%d] %s %s %s\n", i, nrow(kat), id, satir$durum,
              satir$kod_listesi))
  flush.console()
}

cat("Bitti.\n")
