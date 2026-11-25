#!/bin/sh
# veri.json / geojson -> window degiskenli .js dosyalari (fetch gerektirmez; file:// ve GitHub Pages'te calisir)
# Harita dosyalari yalniz geojson varsa yeniden uretilir (GitHub Actions'ta geojson yoktur, harita_*.js depodan gelir).
SITE="$(cd "$(dirname "$0")/.." && pwd)/${PORTAL_SITE:-docs}"
cd "$SITE/veri" || exit 1
printf 'window.VERI=' > veri.js;           cat veri.json >> veri.js;            printf ';' >> veri.js
[ -f harita_il.geojson ] && { printf 'window.HARITA_IL=' > harita_il.js; cat harita_il.geojson >> harita_il.js; printf ';' >> harita_il.js; }
[ -f harita_bolge.geojson ] && { printf 'window.HARITA_BOLGE=' > harita_bolge.js; cat harita_bolge.geojson >> harita_bolge.js; printf ';' >> harita_bolge.js; }
# giris.js yalniz PORTAL_SITE verildiginde (taslak) uretilir; canli site degismesin
[ -n "$PORTAL_SITE" ] && [ -f "$SITE/../ayar/giris.json" ] && { printf 'window.GIRIS=' > giris.js; cat "$SITE/../ayar/giris.json" >> giris.js; printf ';' >> giris.js; }
ls -la *.js
# Her paketlemede veri dosyalarinin adresine yeni surum numarasi yaz (tarayici onbellegi eski veriyi tutmasin)
SURUM=$(date +%Y%m%d%H%M%S)
for f in "$SITE"/*.html; do sed -i "s|\(veri/[a-z_]*\.js\)?v=[0-9]*|\1?v=$SURUM|g" "$f"; done
echo "surum: $SURUM"
