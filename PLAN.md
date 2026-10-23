# Plan: Spielspaß-Mods (Handicap, Geister, Unverwundbarkeit, Stun)

Alle Mods sind **optional, Default = aus**, serverseitig in `atomic_server.ini` konfiguriert.
Dieser Plan ist das Ergebnis der Repo-Analyse und der Entscheidungen (Fragen 1–16).

## 1. Ausgangslage (relevante Stellen)

| Thema | Ort |
|---|---|
| Server-Frameloop (alle 10 ms) | `TServer.CreateNewFrame` – `server/uatomic_server.pas:1765` |
| Treffer Flamme → Spieler, Kill-Statistik | `TAtomicField.HandlePlayerVsMap` – `units/uatomic_field.pas:1741` |
| Sterben (`Dying` → nach `AtomicDietimeout` `Alive := false`) | `CreateNewFrame`, `KillPlayer` (`uatomic_field.pas:2071`) |
| Blinken per `ColorIndex`-Override beim Senden | `TServer.UpdateAllClients` – `uatomic_server.pas:1930` |
| Krankheiten / Items | `TServer.HandlePlayerGetsPowerUp` – `uatomic_server.pas:1141` |
| Fliegende Bomben (`bmFly`), Landung, Bounce | `TAtomicField.HandleBombs` – `uatomic_field.pas:1442-1492` |
| Wurf (Grab) / Punch | `HandleActionPlayer` – `uatomic_field.pas:1208ff` / `1261ff` |
| Hurry (Endspiel) | `HurryHandling` (`uatomic_server.pas:2183`), `fHurryIndex` (privat im Feld) |
| Rundenende | `EndGameCheck` – `uatomic_server.pas:1992` |
| Netzprotokoll | `TAtomicInfo` wird als rohes Record gesendet; `ProtocollVersion = 13` (`uatomic_common.pas:142`) |
| Server-Start | Der **Client** startet `atomic_server` per `TGame.StartHost` (`client/ugame.pas:832`, Parameter `-p`, `-l`). Der **Launcher startet den Server nicht**, er schreibt nur die Client-ini (`fpc_atomic.ini`). |

### Ergebnisse der Detailprüfung (offene Punkte)

1. **Wurf landet auf einem Spieler → ja, ohne Abprall.** `FielWalkable` (`uatomic_field.pas:584`) prüft nur Stein, Item und andere Bomben, **keine Spieler**. Eine geworfene Bombe wird also unter dem Spieler abgelegt (`MoveDir := bmNone`). Der Stun-Haken sitzt genau in diesem Landezweig (`:1473-1476`). Landet die Bombe erst nach Bounce(s), greift der Haken bei der endgültigen Landung ebenfalls (derselbe Zweig).
2. **Punch und Wurf teilen sich `bmFly`.** Damit nur Wurf und Geisterwurf stunnen, braucht die Bombe ein Flag `StunOnLand: Boolean` (gesetzt bei Grab-Wurf und Geisterwurf, nicht bei Punch). Das Flag lebt nur im Server, kein Protokoll.
3. **Launcher:** Er startet nur den Client. Konsequenz: Die Server-ini liegt im Config-Verzeichnis (`GetAppConfigDir`, wie `stats.txt`) und gilt, egal wer den Server startet (Client-Host oder dediziert). Der Launcher bekommt später nur einen „Mods“-Dialog, der diese Datei bearbeitet, und einen Button zum Öffnen des Ordners gibt es schon.
4. **`Info.Alive`-Audit:** 23 Stellen in `uatomic_server.pas` und `uatomic_field.pas` (Rundeninit, Superbad-Tausch, PlayerLeaves, Sync-Timeout, Bewegen, Aktionen, Alive-Zähler, `EndGameCheck`, Statistik, `BlockedByPlayer`, `HandlePlayerVsMap`, `GetAiInfo`, Item-Repopulate) und 2 im Client (Rendern, Tot-Symbol).
   **Designentscheidung daraus:** Ein Geist behält **`Info.Alive = false`** und bekommt stattdessen ein neues Flag `Info.Ghost`. Dann schließen alle 23 bestehenden Stellen Geister automatisch aus (Rundenende, Alive-Zähler, KI-Info, Items, Anstecken, Superbad-Tausch). Neu gebaut werden nur: Geisterbewegung, Geisterwurf, Revive und das Rendern im Client (`Alive Or Ghost`).
5. **Bestehende Fehler** (nicht Teil der Mods, getrennt fixen):
   - `HandlePlayerGetsPowerUp`: `Case random(3)` erreicht den Zweig `3:` (`dSuperSlow`) nie (`uatomic_server.pas:1175`).
   - `RepopulatePlayersCollectedPowerUps`: Bomben-Check prüft zweimal `x`, nie `y` (`uatomic_field.pas:2126`).
6. **AI-Spieler** haben `UID = AIPlayer` (-1) und einen `UserName`; ein Handicap pro Name wirkt also auch für sie. Das `libai`-Interface (`uai_types.pas`) bleibt unverändert. KI-Geister bleiben vorerst passiv (kein Wurf).

## 2. Entscheidungen

| Thema | Entscheidung |
|---|---|
| Konfiguration | Nur `atomic_server.ini` (Config-Verzeichnis). Alle Mods Default aus. |
| Handicap | Trefferpunkte pro Opfer, pro **Spielername** in `[Handicap]`. |
| Kills | Nur der tödliche Treffer zählt als Kill. Ein Treffer durch die eigene Bombe kostet trotzdem einen Trefferpunkt. |
| Endspiel | Ab Hurry-Beginn (`fHurryIndex >= 0`, 80 s Restzeit). Dann: Schutz weg, Geister weg, Treffer tödlich. Im Infinity-Modus nie. Hurry-Tod (Stein entsteht auf Spieler) ist immer tödlich. |
| Schutzzeiten | Die längere gilt, keine Addition. |
| Geisterrand | Äußerste Zellenreihe des 15×11-Feldes (Ring aus 48 Zellen). Flammen/Steine dort ignorieren Geister. |
| Geisterbombe | Fester Wurf 3 Felder senkrecht vom Rand ins Feld. Nach der Explosion darf er erneut werfen (max. 1 Bombe unterwegs). |
| Revive | Nur Treffer auf einen **Gegner** (Teamspiel), an der Startposition (bzw. freies Feld), 3 s Schutz. |
| Positive Krankheit | Ersetzt einen %-Anteil der `puDisease`-Items. Konstantes Blinken, Dauer 5–20 s zufällig, Ende unbekannt. |
| Stun | Wurf/Geisterwurf, der auf einem Spieler landet. Betrifft jeden Spieler auf dem Feld (auch Team, Werfer). Dauer 100–500 ms zufällig. Blockiert Bewegung **und** Aktionen. Unverwundbare sind immun. `raStandStill` + eigenes Blinken. |
| Protokoll | `ProtocollVersion` 13 → 14. Alte Clients verbinden nicht mehr. |

## 3. Konfiguration (`atomic_server.ini`)

```ini
[Mods]
; Mod 1: Handicap / Trefferpunkte
HitPoints=0                 ; 0 = aus, 1 = an
InvulnTimeMs=5000           ; Schutz nach einem (nicht tödlichen) Treffer
DefaultHitPoints=1          ; Trefferpunkte, wenn Spieler nicht in [Handicap] steht
; Mod 2: Geister
Ghosts=0
GhostReviveInvulnMs=3000
GhostFlameLen=2             ; Flammenlänge der Geisterbombe (neu, Default-Vorschlag)
; Mod 3: Positive Krankheit
PositiveDisease=0
PositiveDiseaseChance=15    ; % aller Krankheits-Items
PositiveMinMs=5000
PositiveMaxMs=20000
; Mod 4: Stun
StunOnBombLanding=0
StunMinMs=100
StunMaxMs=500

[Handicap]
; Spielername=Trefferpunkte bis zum Tod
Anna=2
```

Fehlende Schlüssel werden mit Defaults angelegt, damit man sie findet. Ungültige Werte werden mit Log-Warnung auf Defaults/Grenzen geklemmt (Min ≤ Max, Prozent 0..100).

Beim Einloggen eines Clients sendet der Server eine `SendSplashMessage` mit den aktiven Mods (kein Protokoll nötig).

## 4. Datenmodell

**`TPlayer` (nur Server)**: `InvulnTimer`, `HitPoints`, `MaxHitPoints`, `StunTimer`, `GhostPos` (Index auf dem Rand-Ring), `GhostBombActive`.
**`TAtomicInfo` (Protokoll 14)**: `Ghost: Boolean`, `HitPoints: uint8`, `Flags` (Bit: unverwundbar, gestunnt – optional für Client-Effekte).
**Bombe (nur Server)**: `StunOnLand: Boolean`.
**Neu in `uatomic_common.pas`**: `TModSettings`, Konstanten (Blinkperioden), Ini-Lader (in `uatomic_server.pas`, Muster wie `LoadStatistics`).
**`TAtomicField`**: öffentliche Property `HurryActive: Boolean` (= `fHurryIndex <> -1`).

## 5. Phasen

Jede Phase wird einzeln gebaut (`make server client`) und von Hand getestet.

### Phase 0 – Grundlage
- `TModSettings` + Ini-Lader mit Defaults, Splash-Message beim Login.
- `HurryActive` im Feld, Hilfsfunktion `ModsActive`.
- Die zwei bestehenden Fehler (Punkt 5) separat fixen.
- `[Debug]`-Abschnitt der ini (Cheat-Modus Teil A, siehe 5a): `RoundTimeSec`, `StartInvulnMs`, `StartItems`.
- **Noch kein Protokoll-Bump**, solange nichts Neues gesendet wird.

### Phase 1 – Unverwundbarkeit (gemeinsamer Kern)
- `InvulnTimer` zählt in `CreateNewFrame` herunter; bei `HurryActive` auf 0.
- `HandlePlayerVsMap`: Flamme ignorieren, solange `InvulnTimer > 0`.
- Blinken in `UpdateAllClients`: Eigenfarbe ↔ `WhiteColorIndex` (nicht mit Krankheitsbunt verwechselbar).
- Hurry-Tod bleibt unberührt.

### Phase 2 – Stun
- Bombe bekommt `StunOnLand` (Grab-Wurf, später Geisterwurf).
- Im Landezweig von `HandleBombs`: Für jeden lebenden, nicht fliegenden, nicht sterbenden, nicht unverwundbaren Spieler auf dem Landefeld `StunTimer := RandomRange(StunMin, StunMax)`.
- `CreateNewFrame` vor `HandleMovePlayer`: Bei `StunTimer > 0` `MoveState := msStill`, `Action := aaNone` (gilt auch für KI), Timer herunterzählen.
- Blinken: eigene Farbe (z. B. Schwarz/Grau), unterscheidbar vom weißen Schutz-Blinken.

### Phase 3 – Positive Krankheit
- Eigener Zufallszweig in `HandlePlayerGetsPowerUp` (`puDisease`) mit `PositiveDiseaseChance`: `InvulnTimer := max(InvulnTimer, RandomRange(Min, Max))`.
- Keine `TDisease` (sonst ansteckend/heilbar), kein `pssDiseased`.
- Blinken wie Phase 1.

### Phase 4 – Handicap / Trefferpunkte
- `HandleStartRound`: `HitPoints := Handicap[UserName]` oder Default.
- `HandlePlayerVsMap`: Bei Treffer und `HitPoints > 1`: `HitPoints--`, `InvulnTimer := InvulnTimeMs`, Sound, **kein** `KillPlayer`, **keine** Kill-Statistik. Beim letzten Punkt: normales Sterben mit Kill-Wertung.
- Selbsttreffer kostet ebenfalls einen Punkt.
- Protokoll-Bump 13 → 14 und Feld `HitPoints` in `TAtomicInfo`; Client zeigt die Trefferpunkte im Spielerstatus (neben `S:`/`K:`).
- Cheat-Modus Teil B (siehe 5a): `miDebugCommand`, Cheat-Tasten F4–F11, Anzeige „CHEATS AKTIV“, F4-Hilfe und Zustandszeile. Die Cheats für später folgende Mods (Stun, Geister, positive Krankheit) sind zunächst Stubs und werden in den jeweiligen Phasen angebunden.

### Phase 5 – Geister
- `CreateNewFrame`, Stelle `Alive := false`: Bei Mod an und `Not HurryActive` → `Info.Ghost := true`, Position auf nächstem Randfeld, `GhostBombActive := false`.
- Geisterbewegung: Eigener Ring-Bewegungscode (links/rechts/oben/unten verschieben `GhostPos` entlang des Rings) vor `HandleMovePlayer`.
- Geisterwurf: Aktion erzeugt eine `bmFly`-Bombe mit `StunOnLand`, Ziel 3 Felder senkrecht ins Feld, `FlameLen = GhostFlameLen`, Besitzer = Geist. `GhostBombActive` blockt einen zweiten Wurf bis zur Explosion.
- Revive: In `HandlePlayerVsMap` Treffer eines Lebenden durch Flamme eines Geistes (Team-/Gegnerprüfung): Geist wird lebendig (`Alive := true`, `Ghost := false`, `HitPoints` zurück), Startposition/freies Feld, `InvulnTimer := GhostReviveInvulnMs`. Kein Kill-Credit für den Geist.
- Bei `HurryActive`: alle Geister `Ghost := false`, bestehende Geisterbomben werden entschärft/ignorieren Revive.
- Items des Toten bleiben wie heute verteilt (`RepopulatePlayersCollectedPowerUps` läuft vorher).
- Client: Rendern bei `Alive Or Ghost` (halbtransparent), Tot-Symbol nur bei weder `Alive` noch `Ghost`.
- Rundenende prüft weiter nur `Alive` – Geister zählen nie als lebend.

### Phase 6 – Launcher, Doku, Release
- Launcher: Mods-Dialog (Checkboxen/Werte) liest/schreibt `atomic_server.ini` im Config-Verzeichnis.
- `MANUAL.md` und Changelog-Kopf in `uatomic_common.pas` ergänzen; Version und `ProtocollVersion` (14) erhöhen (Datei markiert die zwei Stellen mit ACHTUNG).
- Client-Splash zeigt aktive Mods.

## 5a. Cheat-Modus (Test-Hilfe)

Zweck: Mods sofort prüfen, ohne 80 s auf Hurry zu warten oder Situationen zu provozieren. Standardmäßig komplett aus.

### Teil A – Debug-Schalter in der Server-ini (Phase 0)

```ini
[Debug]
Cheats=0              ; Master-Schalter, Default aus
RoundTimeSec=0        ; >0 überschreibt die Rundenzeit (Hurry schon nach ~10 s testbar)
StartInvulnMs=0       ; Spieler starten bereits unverwundbar
StartItems=           ; z.B. "kick,punch,grab,bomb*3" für Wurf-/Stun-Tests
```

### Teil B – Cheat-Tasten im Spiel (Phase 4, zusammen mit Protokoll 14)

- Neue Nachricht `miDebugCommand` (Client → Server). Der Server führt sie nur aus, wenn `Cheats=1` **und** der Absender der Master ist; sonst Verwerfen + Log-Eintrag. Jeder ausgeführte Cheat wird geloggt.
- Der Server meldet `Cheats=1` beim Login (Splash-Message). Nur dann reagieren die Tasten im Client.
- Tasten (nur im laufenden Spiel):

| Taste | Wirkung |
|---|---|
| F5 | Positive Krankheit auf mich (Unverwundbarkeit 5–20 s) |
| F6 | Mich selbst treffen (Trefferpunkt abziehen bzw. sterben) |
| F7 | Mich zum Geist machen / zurück ins Spiel holen |
| F8 | Hurry sofort starten |
| F9 | Stun auf mich |
| F10 | Alle Items (Grab, Kick, Punch, Bomben) |
| F11 | Trefferpunkte auf Max setzen |
| F4 | Cheat-Hilfe ein-/ausblenden |

- Cheats prüfen bewusst **nicht** auf `HurryActive` o. Ä., damit Randfälle gezielt erzeugt werden können.

### Anzeige im Spiel (Text)

- **Dauerhaft, solange `Cheats=1`:** kleiner Hinweis oben im Spielfeld, z. B. `CHEATS AKTIV  –  F4: Hilfe`, damit nie unbemerkt mit Cheats gespielt wird (auch für Mitspieler sichtbar, da der Server den Status an alle meldet).
- **Mit F4 eine Hilfe-Liste** (Overlay mit den Tasten oben, gerendert mit dem vorhandenen `AtomicFont` wie die `S:/K:`-Anzeige).
- **Zustandsanzeige je Spieler** (nur bei Cheats): neben `S:`/`K:` eine Zeile mit `HP:n`, `INV` (Restzeit), `STUN`, `GHOST` – so sieht man sofort, ob der Mod wie erwartet gewirkt hat. Dafür sendet der Server bei aktivem Cheat-Modus die Restzeiten in `TAtomicInfo` (über die bereits vorhandenen Info-Felder/Flags; Restzeit als zusätzliches `uint16`).
- **Rückmeldung pro Befehl:** Der Server antwortet mit einer kurzen Text-Nachricht (`SendSplashMessage`), z. B. `Cheat: Hurry gestartet`.

## 6. Reihenfolge und Abhängigkeiten

0 → 1 → 2 → 3 → 4 → 5 → 6. Phase 1 ist Voraussetzung für 2, 3, 4 (Unverwundbarkeit), Phase 4 für 5 (Trefferpunkte beim Revive), Phase 2 für 5 (Geisterwurf stunnt).
Protokoll-Bump 14 erfolgt in Phase 4 und umfasst alle neuen Info-Felder sowie `miDebugCommand` in einem Schritt, damit nur einmal inkompatibel wird.
Der Cheat-Modus (5a) verkürzt die Tests aller Phasen; Teil A steht ab Phase 0 bereit, Teil B ab Phase 4. Aufwand Cheat-Modus: Teil A ca. 10–15 Min, Teil B ca. 30–45 Min.

## 7. Test (kein Test-Suite vorhanden)

- Build: `make server client` (siehe `CLAUDE.md`), jeweils auf `Linking … ` ohne `Error` achten.
- Handtest mit KI-Spielern: kurze Rundenzeit, Handicap für einen KI-Namen, Beobachten von Blinken, Stun, Geistern.
- Randfälle: Hurry beginnt während Schutz/Geist aktiv; Infinity-Modus; Teamspiel; Spieler verlässt während Geist; Stun gleichzeitig auf Trampolin/Loch; Geisterbombe fliegt auf Spieler im Hurry; zwei Spieler auf dem Landefeld; gleichzeitiger Treffer und Schutzende im selben Frame.
- Mods aus (Default): Spielverhalten muss identisch zum aktuellen Stand sein.

## 8. Restrisiken

- Neue Zustände (Schutz, Stun, Geist) greifen in Treffer-, Bewegungs- und Rundenende-Logik ein; der `Alive`-Audit (Punkt 4) und die Randfälle in Abschnitt 7 sind die Hauptabsicherung.
- Der Protokoll-Bump macht alte Clients inkompatibel (bewusste Entscheidung).
- `GhostFlameLen` und der Revive-Ort (freies Feld) sind Vorschläge ohne ausdrückliche Vorgabe und können noch angepasst werden.
