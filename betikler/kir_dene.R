# Bülten kırılım tablosu tanımını ortak dosyalara YAZMADAN dener ve 05f'deki doğrulamayı uygular.
# Kullanım (depo kökünden): Rscript betikler/kir_dene.R betikler/kirilim_bulten/<dosya>.R [kod1,kod2]
# Her tablo için: oku() -> kir_son() -> yogunlastir() -> dogrula() ve kısa özet basar.
a <- commandArgs(TRUE)
.f <- "betikler/kir_dene.R"
source("betikler/yollar.R")
source("betikler/bulten_yardimci.R")
source("betikler/bulten_kirilim.R")
source("betikler/kirilim_yardimci.R")
KIR_TABLOLAR <- list()
source(a[1], encoding = "UTF-8")
secili <- if (length(a) > 1) strsplit(a[2], ",")[[1]] else NULL
tamam <- TRUE
for (t in KIR_TABLOLAR) {
  if (!is.null(secili) && !t$kod %in% secili) next
  cat("==", t$kod, "\n")
  r <- tryCatch({
    s <- t$oku()
    if (!"REF_AREA" %in% names(s$veri)) s$veri$REF_AREA <- "TR"
    s <- kir_son(s, names(t$boyut_ad), ad = t$kod)
    d <- s$veri; d$v <- d$OBS_VALUE
    cog <- "REF_AREA"; kir <- names(t$boyut_ad)
    if (length(kir) == 1) { d$SUTUN_ <- "V"; kir <- c(kir, "SUTUN_") }
    y <- yogunlastir(d, kir, cog, ad = t$kod)
    if (t$turkiye == "topla") {
      for (dn in names(y$v)) { m <- do.call(rbind, y$v[[dn]]); y$v[[dn]]$TR <- ifelse(colSums(!is.na(m)) == 0, NA, colSums(m, na.rm = TRUE)) }
    }
    h <- dogrula(y, t$olcu, t$turkiye, yuvarlak = identical(t$birim, "bin kişi"), atla = strsplit(if (is.null(t$toplanamaz)) "" else t$toplanamaz, ";")[[1]])
    cat(sprintf("  gözlem %d, yer %d, dönem %s, kırılım %s\n", nrow(d), length(y$yerler), paste(range(y$donemler), collapse = ".."),
                paste(vapply(y$kirilimlar, function(k) sprintf("%s(%d)", k$kol, length(k$kodlar)), ""), collapse = " x ")))
    for (k in s$etiket$boyut |> unique()) cat("  ", k, ":", paste(head(s$etiket$kod_ad[s$etiket$boyut == k], 12), collapse = " | "), "\n")
    if (length(h)) stop("DOĞRULAMA: ", paste(h, collapse = "; "), call. = FALSE)
    "tamam"
  }, error = function(e) { cat("  HATA:", conditionMessage(e), "\n"); NULL })
  if (is.null(r)) tamam <- FALSE
}
if (!tamam) quit(status = 1)
