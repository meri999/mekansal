# Tarama ilerleme cubugu. Calistirma:  powershell -File portal\betikler\ilerleme.ps1
# Tek seferlik gosterim:               powershell -File portal\betikler\ilerleme.ps1 -Bir
param([switch]$Bir)
$toplam = 492
$klasor = Join-Path $PSScriptRoot "..\veri"
$dolu_k = [string][char]0x2588; $bos_k = [string][char]0x2591; $kum = [string][char]0x231B
$t0 = Get-Date; $ilk = $null
do {
  $n = 0
  foreach ($f in Get-ChildItem $klasor -Filter "kesif_*.csv") {
    $n += [Math]::Max(0, (Get-Content $f.FullName -Encoding UTF8 | Measure-Object -Line).Lines - 1)
  }
  if ($null -eq $ilk) { $ilk = $n }
  $yuzde = [Math]::Min([double]1, [double]$n / $toplam)
  $uzun = 40; $dolu = [int][Math]::Floor($uzun * $yuzde)
  $cubuk = ($dolu_k * $dolu) + ($bos_k * ($uzun - $dolu))
  $gecen = (Get-Date) - $t0
  $kalan = "hesaplaniyor"
  if ($n -gt $ilk -and $gecen.TotalSeconds -gt 30) {
    $hiz = ($n - $ilk) / $gecen.TotalMinutes
    $kalan = ("{0:N0} dk" -f (($toplam - $n) / $hiz))
  }
  Write-Host ("`r{0} [{1}] %{2:N0}  {3}/{4} tablo  kalan ~ {5}   " -f $kum, $cubuk, ($yuzde * 100), $n, $toplam, $kalan) -NoNewline
  if ($Bir -or $n -ge $toplam) { break }
  Start-Sleep -Seconds 5
} while ($true)
Write-Host ""
