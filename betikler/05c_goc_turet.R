.f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
source(file.path(dirname(normalizePath(.f, winslash = "/")), "yollar.R"))

# DF_ADNKS_T09 (ikamet edilen il x nüfus kütüğüne kayıtlı il)
#   -> il bazlı "kütük-ikamet farkı" serileri
# Çıktı: veri/ham/DF_TURET_ADNKS_T09.csv
#   (05_gosterge_uret.R bunu IKAMET_YERI sütunuyla okur)
#
#   GOSTERGE=YERLESIK : ilde ikamet eden nüfus
#   GOSTERGE=KAYITLI  : kütüğü ilde kayıtlı nüfus (nerede yaşarsa yaşasın)
#   GOSTERGE=AYNI     : hem kütüğü hem ikameti ilde olan nüfus
#   GOSTERGE=ALINAN   : ilde yaşayan ama kütüğü başka ilde olan nüfus
#                       (YERLESIK - AYNI)
#   GOSTERGE=VERILEN  : kütüğü ilde olup başka ilde yaşayan nüfus
#                       (KAYITLI - AYNI)
#   GOSTERGE=NET      : ALINAN - VERILEN = YERLESIK - KAYITLI
#
# Türkiye satırı (TR): YERLESIK ve KAYITLI toplam;
# ALINAN = VERILEN = illerin ALINAN toplamı.
# Bu bir STOK ölçüsüdür (adrese dayalı kayıt ile fiili ikametin farkı),
# yıllık göç AKIŞI değildir.

girdi <- file.path(PORTAL, "veri/ham/DF_ADNKS_T09.csv")
if (!file.exists(girdi)) {
  stop("Once DF_ADNKS_T09 indirilmeli: ", girdi)
}

il_re <- "^TR[0-9A-C][0-9]{2}$"
anahtar <- c("NUFUS_KAYIT_IL", "IKAMET_YERI", "TIME_PERIOD")


# --- veriyi oku --------------------------------------------------------

d <- read.csv(girdi, colClasses = "character", check.names = FALSE,
              encoding = "UTF-8")
d <- d[, c(anahtar, "OBS_VALUE")]
d$v <- suppressWarnings(as.numeric(d$OBS_VALUE))
d <- d[!is.na(d$v), ]

if (anyDuplicated(d[, anahtar])) {
  stop("T09'da yinelenen il-il-yil gozlemi var")
}


# --- üç temel seri -----------------------------------------------------

# ilde ikamet edenler (kütük ili toplam)
yerlesik <- d[d$NUFUS_KAYIT_IL == "_T" & grepl(il_re, d$IKAMET_YERI),
              c("IKAMET_YERI", "TIME_PERIOD", "v")]
names(yerlesik)[3] <- "YERLESIK"

# kütüğü ilde olanlar (ikamet yeri toplam)
kayitli <- d[d$IKAMET_YERI == "_T" & grepl(il_re, d$NUFUS_KAYIT_IL),
             c("NUFUS_KAYIT_IL", "TIME_PERIOD", "v")]
names(kayitli) <- c("IKAMET_YERI", "TIME_PERIOD", "KAYITLI")

# kütüğü de ikameti de aynı ilde olanlar
ayni <- d[d$NUFUS_KAYIT_IL == d$IKAMET_YERI & grepl(il_re, d$IKAMET_YERI),
          c("IKAMET_YERI", "TIME_PERIOD", "v")]
names(ayni)[3] <- "AYNI"

m <- Reduce(function(a, b) merge(a, b, by = c("IKAMET_YERI", "TIME_PERIOD")),
            list(yerlesik, kayitli, ayni))
m$ALINAN <- m$YERLESIK - m$AYNI
m$VERILEN <- m$KAYITLI - m$AYNI
m$NET <- m$YERLESIK - m$KAYITLI


# --- Türkiye satırı ----------------------------------------------------

tr <- aggregate(cbind(YERLESIK, KAYITLI, AYNI, ALINAN, VERILEN) ~ TIME_PERIOD,
                m, sum)
tr$IKAMET_YERI <- "TR"
tr$VERILEN <- tr$ALINAN
tr$NET <- NA   # ulusal net fark anlamsız (0)
m <- rbind(m, tr[, names(m)])


# --- uzun biçime çevir ve yaz ------------------------------------------

gostergeler <- c("YERLESIK", "KAYITLI", "AYNI", "ALINAN", "VERILEN", "NET")
uzun <- do.call(rbind, lapply(gostergeler, function(g) {
  data.frame(IKAMET_YERI = m$IKAMET_YERI,
             TIME_PERIOD = m$TIME_PERIOD,
             GOSTERGE = g,
             OBS_VALUE = m[[g]])
}))
uzun <- uzun[!is.na(uzun$OBS_VALUE), ]
uzun <- uzun[order(uzun$GOSTERGE, uzun$IKAMET_YERI, uzun$TIME_PERIOD), ]

write.csv(uzun, file.path(PORTAL, "veri/ham/DF_TURET_ADNKS_T09.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat(sprintf("DF_TURET_ADNKS_T09: %d satir, %d il, %s-%s\n",
            nrow(uzun), length(unique(m$IKAMET_YERI)) - 1,
            min(m$TIME_PERIOD), max(m$TIME_PERIOD)))

# Kontrol için son yılın birkaç satırı
son <- max(m$TIME_PERIOD)
print(m[m$TIME_PERIOD == son &
          m$IKAMET_YERI %in% c("TR", "TR100", "TR510", "TRB21"), ])
