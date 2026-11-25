# Mekansal Ayrışma Portalı

Türkiye geneli ile il (81) ve bölge (İBBS-1, 12) düzeyinde nüfus, aile, ekonomi, fiyat, işgücü ve eğitim göstergelerini gösteren statik bir veri portalı.

**Canlı site:** https://meri999.github.io/mekansal/

## Depo yapısı

| Klasör | İçerik |
|---|---|
| `docs/` | Yayınlanan site (`index.html`, `goc-agi.html`, `veri/*.js`). GitHub Pages bu klasörden yayın yapar. |
| `betikler/` | Veriyi indiren, işleyen ve siteyi paketleyen R, Python ve shell betikleri (numara sırasıyla çalışır). |
| `ayar/` | Gösterge tanımları, kategori eşlemeleri, il adı düzeltmeleri. |

`veri/` klasörü (ham ve ara dosyalar) depoda **yoktur**; betiklerle yeniden üretilir.

## Veri kaynakları

- **TÜİK Veri Portalı** (SDMX API) ve TÜİK bültenleri. Göstergeler TÜİK'in yayımladığı tablolardan alınır; "Hesaplanmış" etiketli göstergeler TÜİK tarafından yayımlanmaz, ilgili tablolardan türetilir.
- **Harita sınırları:** [htastan/TRmaps](https://github.com/htastan/TRmaps) (MIT).

## Veriyi yeniden üretme

Gerekenler: R (`httr2`, `jsonlite`, `readxl`), bir TÜİK API anahtarı.

1. Depo kökünde `.Renviron` dosyası oluşturup `TUIK_API_KEY=...` yazın (dosya git'e girmez).
2. TÜİK API tablolarını indirin: `Rscript betikler/03_indir_secili.R`
3. Elle indirilen bülten tablolarını (`veri/manuel_indirilenler/`) `05a`, `05b`, `05g`... betikleriyle okuyun.
4. Göstergeleri üretin: `Rscript betikler/05_gosterge_uret.R`
5. Siteyi paketleyin: `sh betikler/07_site_paketle.sh`

Otomatik (GitHub Actions) güncelleme şu an yoktur; güncelleme yerel bilgisayarda yapılır.

## Site

Site statiktir; herhangi bir sunucu gerektirmez.
