"""Giris sayfasi kontrolleri. Kullanim: python -I betikler/kontrol_giris.py site-lab"""
import json, re, sys, glob, os

def _html(site):
    return sorted(glob.glob(os.path.join(site, "*.html")))

def _oku(p):
    with open(p, encoding="utf-8") as f:
        return f.read()

def kontrol_adlar(site):
    h = []
    for p in _html(site):
        if p.endswith(".test.html"):
            continue
        t = _oku(p)
        if re.search(r"Mekânsal Portal|Mekansal Portal|Enstitü Sosyal Portal", t):
            h.append(f"{os.path.basename(p)}: eski ad")
    return h

def kontrol_baglantilar(site):
    h = []
    for p in _html(site):
        for x in re.findall(r'href="([^"#:]+\.html)', _oku(p)):
            if not os.path.exists(os.path.join(site, x)):
                h.append(f"{os.path.basename(p)}: kırık bağlantı {x}")
    return h

def _veri(site):
    with open(os.path.join(site, "veri", "veri.json"), encoding="utf-8") as f:
        return json.load(f)

def kontrol_veri(site):
    h = []
    d = _veri(site)
    il = {y["kod"] for y in d["yerler"] if y["duzey"] == "il"}
    basliklar = {g["baslik"]: g for g in d["gostergeler"]}
    g = basliklar.get("Toplam doğurganlık hızı")
    if not g:
        h.append("TDH göstergesi yok")
    else:
        n = max((sum(1 for k, v in g["veri"].items() if k in il and v[i] is not None)
                 for i in range(len(g["yillar"]))), default=0)
        son = sum(1 for k, v in g["veri"].items() if k in il and v[-1] is not None)
        if son < 70:
            h.append(f"TDH il sayısı: {son}")
    gp = os.path.join(os.path.dirname(os.path.abspath(site)), "ayar", "giris.json")
    if os.path.exists(gp):
        with open(gp, encoding="utf-8") as f:
            c = json.load(f)
        for a in c.get("anahtar", []):
            if a["gosterge"] not in basliklar:
                h.append(f"giris.json: gösterge yok: {a['gosterge']}")
        if c.get("harita_gosterge") not in basliklar:
            h.append("giris.json: harita_gosterge yok")
    return h

def kontrol_giris(site):
    h = []
    p = os.path.join(site, "index.html")
    if not os.path.exists(p):
        return ["index.html yok"]
    t = _oku(p)
    for i in ["ust", "kahraman", "seri", "araclar", "konular", "bulgular", "alt"]:
        if f'id="{i}"' not in t:
            h.append(f"index.html: id yok: {i}")
    m = re.search(r'id="gSay">([^<]*)<', t)
    if m and re.search(r"\d", m.group(1)):
        h.append("index.html: gSay içinde elle yazılmış sayı")
    if "Enstitü Portal" not in t:
        h.append("index.html: ad yok")
    for eski in ["Araçlar</a>", "Bulgular</a>", "Veri ve yöntem</a>"]:
        if eski in t:
            h.append("index.html: kaldırılan menü bağlantısı duruyor: " + eski)
    for yeni in ['class="kare"', "Giriş yap", "Enstitü girişi", "Üye girişi", 'getElementById("girisDugme")']:
        if yeni not in t:
            h.append("index.html: yok: " + yeni)
    return h

if __name__ == "__main__":
    site = sys.argv[1] if len(sys.argv) > 1 else "site-lab"
    kontroller = [kontrol_adlar, kontrol_baglantilar]
    if os.path.exists(os.path.join(site, "veri", "veri.json")):
        kontroller.append(kontrol_veri)
    kontroller.append(kontrol_giris)
    hatalar = [x for k in kontroller for x in k(site)]
    for x in hatalar:
        print("HATA:", x)
    print(f"{len(hatalar)} hata")
    sys.exit(1 if hatalar else 0)
