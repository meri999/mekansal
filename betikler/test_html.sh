#!/bin/sh
# Kullanim: sh betikler/test_html.sh <html dosyasi>  -> basliksiz Chrome ile calistirip <pre id=o> ciktisini basar
"/c/Program Files/Google/Chrome/Application/chrome.exe" --headless=new --disable-gpu --allow-file-access-from-files --virtual-time-budget=30000 --dump-dom "file:///$(cd "$(dirname "$1")" && pwd -W)/$(basename "$1")" 2>/dev/null | sed -n 's/.*<title>\(.*\)<\/title>.*/\1/p;/<pre id="o">/,/<\/pre>/p'
