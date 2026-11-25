.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))

# Türkiye düzeyli tabloların kod etiketlerini çeker
# (argümanlar: tablo kimlikleri) -> veri/etiket_tr.csv

source(file.path(PORTAL, "betikler", "tuik_api.R"))
library(xml2)

ids <- commandArgs(TRUE)
kat <- read.csv(file.path(PORTAL, "ayar/katalog.csv"),
                fileEncoding = "UTF-8", stringsAsFactors = FALSE)
cikti <- file.path(PORTAL, "veri/etiket_tr.csv")
yapilan <- character()
if (file.exists(cikti)) {
  yapilan <- unique(read.csv(cikti, fileEncoding = "UTF-8")$df_id)
}


# --- yardımcı fonksiyonlar ---------------------------------------------

# Düğümün Türkçe adı; boşsa İngilizce adı
ad <- function(n) {
  v <- xml_text(xml_find_first(
    n, "./*[local-name()='Name'][@*[local-name()='lang']='tr']"))
  if (is.na(v) || v == "") {
    v <- xml_text(xml_find_first(
      n, "./*[local-name()='Name'][@*[local-name()='lang']='en']"))
  }
  v
}

tok_al <- function() {
  for (k in 1:6) {
    t <- tryCatch(tuik_token(), error = function(e) NULL)
    if (!is.null(t)) return(t)
    Sys.sleep(15 * k)
  }
  stop("token")
}


# --- çekme döngüsü -----------------------------------------------------

tok <- tok_al()
t0 <- Sys.time()
hata <- 0

for (id in setdiff(ids, yapilan)) {
  if (difftime(Sys.time(), t0, units = "mins") > 2) {
    tok <- tok_al()
    t0 <- Sys.time()
  }

  yol <- sprintf("dataflow/TR/%s/%s?references=descendants",
                 id, kat$ver[kat$id == id])
  r <- tryCatch(tuik_get(yol, tok), error = function(e) NULL)
  b <- ""
  if (!is.null(r) && resp_status(r) == 200) b <- resp_body_string(r)

  if (!nzchar(b) || grepl("^<!DOCTYPE|^<html", b)) {
    hata <- hata + 1
    cat(sprintf("%s HATA\n", id))
    if (hata >= 3) {
      cat("Ust uste 3 hata, duruyorum.\n")
      quit(status = 2)
    }
    Sys.sleep(30)
    next
  }
  hata <- 0
  x <- read_xml(b)

  # kavram adları: (şema, kavram) -> ad
  kav <- xml_find_all(
    x, "//*[local-name()='ConceptScheme']//*[local-name()='Concept']")
  kt <- data.frame(
    sema = vapply(kav, function(n) xml_attr(xml_parent(n), "id"), ""),
    kavram = xml_attr(kav, "id"),
    ad = vapply(kav, ad, "")
  )

  dims <- xml_find_all(
    x, paste0("//*[local-name()='DataStructure']",
              "//*[local-name()='DimensionList']",
              "/*[local-name()='Dimension']"))
  cls <- xml_find_all(x, "//*[local-name()='Codelist']")

  # kod listeleri: (liste/sürüm, kod) -> kod adı
  kod <- do.call(rbind, lapply(cls, function(cl) {
    cd <- xml_find_all(cl, "./*[local-name()='Code']")
    if (!length(cd)) return(NULL)
    data.frame(
      kl = paste0(xml_attr(cl, "id"), "/", xml_attr(cl, "version")),
      kod = xml_attr(cd, "id"),
      kod_ad = vapply(cd, ad, ""),
      stringsAsFactors = FALSE
    )
  }))

  # boyutlar: her boyutun adı ve kod listesi
  s <- do.call(rbind, lapply(dims, function(d) {
    e <- xml_find_first(d, ".//*[local-name()='Enumeration']/*")
    cn <- xml_find_first(d, ".//*[local-name()='ConceptIdentity']/*")
    a <- kt$ad[kt$sema == xml_attr(cn, "maintainableParentID") &
                 kt$kavram == xml_attr(cn, "id")]
    kl <- NA
    if (!is.na(xml_attr(e, "id"))) {
      kl <- paste0(xml_attr(e, "id"), "/", xml_attr(e, "version"))
    }
    data.frame(
      boyut = xml_attr(d, "id"),
      boyut_ad = if (length(a)) a[1] else xml_attr(d, "id"),
      kl = kl
    )
  }))

  # il/IBBS kod listeleri etiketlenmez
  s <- s[!is.na(s$kl) & !grepl("^CL_IBBS|^CL_IL", s$kl), ]
  m <- merge(s, kod, by = "kl")
  m$df_id <- id

  write.table(m[, c("df_id", "boyut", "boyut_ad", "kod", "kod_ad")], cikti,
              sep = ",", row.names = FALSE, append = file.exists(cikti),
              col.names = !file.exists(cikti), fileEncoding = "UTF-8")
  cat(sprintf("%s ok %d kod\n", id, nrow(m)))
  flush.console()
  Sys.sleep(3)
}
cat("Bitti.\n")
