<#
  Builds the smart ai and copies it next to the original ai, to try it in the game.
  Then choose it in the launcher: Game options, AI = Smart.

    .\try_ai.ps1                         build and copy into the game folder (default: the repository root)
    .\try_ai.ps1 -GameDir C:\Games\FPCA  another game folder (where atomic_server.exe is)
    .\try_ai.ps1 -Restore                only for older versions of this script: put the original ai.dll back
                                         (they copied the smart ai over ai.dll and saved it as ai_original.dll)
#>
param(
  [string]$GameDir = "",
  [string]$Lazbuild = "$env:USERPROFILE\lazarus\4.8\lazbuild.exe",
  [switch]$Restore
)

$ErrorActionPreference = 'Stop'
$Root = Split-Path -Parent $PSScriptRoot
if (-not $GameDir) { $GameDir = $Root }
$GameDir = (Resolve-Path $GameDir).Path
$Ai = Join-Path $GameDir 'ai.dll'
$Backup = Join-Path $GameDir 'ai_original.dll'
$Target = Join-Path $GameDir 'ai_smart.dll'

if (Get-Process -Name atomic_server -ErrorAction SilentlyContinue) {
  Write-Warning "atomic_server is running and may keep the ai in use. Close the server first."
}

if ($Restore) {
  if (-not (Test-Path $Backup)) { throw "No backup found: $Backup" }
  Copy-Item $Backup $Ai -Force
  Remove-Item $Backup
  Write-Host "Original ai restored: $Ai"
  exit 0
}

if (-not (Test-Path $Lazbuild)) { throw "lazbuild not found: $Lazbuild (use -Lazbuild)" }
Write-Host "Building ai_smart ..."
& $Lazbuild -q (Join-Path $PSScriptRoot 'ai_smart.lpi')
if ($LASTEXITCODE -ne 0) { throw "Build failed (exit code $LASTEXITCODE)" }
$Built = Join-Path $Root 'ai_smart.dll'
if (-not (Test-Path $Built)) { throw "Not found after the build: $Built" }
if ($Built -ne $Target) { Copy-Item $Built $Target -Force }
Write-Host "Smart ai: $Target"
Write-Host "Choose it in the launcher: Game options, AI = Smart (the server log shows 'Atomic smart ai')."
if (Test-Path $Backup) {
  Write-Warning "ai.dll was replaced by an older version of this script, put the original back with: .\try_ai.ps1 -GameDir `"$GameDir`" -Restore"
}
