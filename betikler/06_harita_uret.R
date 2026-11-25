.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))

# NOT: İl haritası 2026-11-24'ten beri betikler/06b_harita_il_gisco.py ile GISCO NUTS3'ten üretilir.
#      Bu betiğin "il" satırını çalıştırırsanız harita_il dosyaları eski TRmaps (kaba) sürümüne döner.
# TRmaps (MIT lisanslı) il ve bölge sınırları
#   -> sadeleştirilmiş GeoJSON (docs/veri/)

suppressMessages({
  library(sf)
  library(TRmaps)
  library(jsonlite)
})
sf_use_s2(FALSE)


# --- veri ile harita kodlarını karşılaştır -----------------------------

v <- fromJSON(file.path(PORTAL, "docs/veri/veri.json"), simplifyVector = FALSE)
yer <- do.call(rbind, lapply(v$yerler, as.data.frame))
il_k <- yer$kod[yer$duzey == "il"]
bo_k <- yer$kod[yer$duzey == "bolge"]

il <- tr_nuts3
il$kod <- il$NUTS3_code
bo <- tr_nuts1
bo$kod <- bo$NUTS1_code

cat("il kodu eslesmesi: haritada", nrow(il), "| veride", length(il_k),
    "| ortak", length(intersect(il$kod, il_k)), "\n")
cat("haritada olup veride olmayan:", setdiff(il$kod, il_k),
    "| veride olup haritada olmayan:", setdiff(il_k, il$kod), "\n")
cat("bolge kodu eslesmesi: ortak", length(intersect(bo$kod, bo_k)),
    "/", length(bo_k), "| fark:", setdiff(bo_k, bo$kod),
    setdiff(bo$kod, bo_k), "\n")


# --- sadeleştir ve yaz -------------------------------------------------

yaz <- function(x, ad_kolon, dosya, tol) {
  x <- st_make_valid(x)
  x <- st_simplify(x, preserveTopology = TRUE, dTolerance = tol)

  # Ad önce veri dosyasından, eksikse haritanın kendi adından
  x$ad <- yer$ad_tr[match(x$kod, yer$kod)]
  if (!is.null(ad_kolon)) {
    eksik <- is.na(x$ad)
    x$ad[eksik] <- x[[ad_kolon]][eksik]
  }
  x <- x[, c("kod", "ad")]

  unlink(dosya)
  st_write(x, dosya, driver = "GeoJSON", quiet = TRUE,
           layer_options = "COORDINATE_PRECISION=3")
  cat(sprintf("%s: %d oge, %.0f KB\n",
              dosya, nrow(x), file.size(dosya) / 1024))
}

yaz(il, "name_tr", file.path(PORTAL, "docs/veri/harita_il.geojson"), 0.01)
yaz(bo, "NUTS1_name", file.path(PORTAL, "docs/veri/harita_bolge.geojson"), 0.02)
