"""İl haritası: Avrupa Komisyonu GISCO NUTS 2021, düzey 3 (81 il), 1:1 milyon.

Kullanım: python betikler/06b_harita_il_gisco.py [ciktı_klasörü] [indirilmiş_nuts3.geojson]
  ciktı_klasörü varsayılan: site-lab/veri  (harita_il.geojson ve harita_il.js yazılır)
Kod (TR613 gibi) veri.json'daki il kodlarıyla birebir aynıdır; ad veri.json'dan alınır.
Kaynak notu: © EuroGeographics, GISCO (Eurostat). İl sınırları artık buradan; bölge sınırları
betikler/06_harita_uret.R ile TRmaps'ten gelmeye devam eder.
"""
import json, os, sys, urllib.request

URL = "https://gisco-services.ec.europa.eu/distribution/v2/nuts/geojson/NUTS_RG_01M_2021_4326_LEVL_3.geojson"
kok = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
cikti = sys.argv[1] if len(sys.argv) > 1 else os.path.join(kok, "site-lab", "veri")
yerel = sys.argv[2] if len(sys.argv) > 2 else None

veri = json.load(open(os.path.join(cikti, "veri.json"), encoding="utf-8"))
ad = {y["kod"]: y["ad_tr"] for y in veri["yerler"] if y["duzey"] == "il"}

if yerel:
    ham = json.load(open(yerel, encoding="utf-8"))
else:
    with urllib.request.urlopen(URL) as r:
        ham = json.loads(r.read().decode("utf-8"))


def yuvarla(halka):
    """3 ondalık (~100 m); art arda tekrar eden noktaları at, halkayı kapalı tut."""
    out = []
    for x, y in halka:
        p = [round(x, 3), round(y, 3)]
        if not out or p != out[-1]:
            out.append(p)
    if out and out[0] != out[-1]:
        out.append(out[0])
    return out


def kes(g):
    ps = [g["coordinates"]] if g["type"] == "Polygon" else g["coordinates"]
    yeni = []
    for p in ps:
        halkalar = [yuvarla(r) for r in p]
        halkalar = [r for r in halkalar if len(r) >= 4]
        if halkalar:
            yeni.append(halkalar)
    return {"type": "MultiPolygon", "coordinates": yeni}


ozellikler = []
for f in ham["features"]:
    kod = f["properties"]["NUTS_ID"]
    if f["properties"]["CNTR_CODE"] != "TR":
        continue
    if kod not in ad:
        sys.exit("veri.json'da olmayan il kodu: " + kod)
    ozellikler.append({"type": "Feature", "properties": {"kod": kod, "ad": ad[kod]}, "geometry": kes(f["geometry"])})

eksik = set(ad) - {f["properties"]["kod"] for f in ozellikler}
if eksik or len(ozellikler) != 81:
    sys.exit("il sayısı/kodu uyuşmuyor: %d, eksik %s" % (len(ozellikler), sorted(eksik)))

ozellikler.sort(key=lambda f: f["properties"]["kod"])
metin = json.dumps({"type": "FeatureCollection", "name": "harita_il", "features": ozellikler},
                   ensure_ascii=False, separators=(",", ":"))
open(os.path.join(cikti, "harita_il.geojson"), "w", encoding="utf-8").write(metin)
open(os.path.join(cikti, "harita_il.js"), "w", encoding="utf-8").write("window.HARITA_IL=" + metin + ";")
print("81 il yazildi: %.0f KB" % (len(metin.encode("utf-8")) / 1024))
