# Smart-KI

Eine zweite KI für FPC Atomic, neben der originalen KI von Corpsman in `ai/`, die unverändert bleibt.
Sie spielt klüger, bleibt für Menschen fair, und jede Fähigkeit lässt sich in `ai_smart.ini` abschalten oder nur mit einer Wahrscheinlichkeit nutzen.

Englisch: [README.md](README.md)

## Was anders ist als bei der originalen KI

| Originale KI | Smart-KI |
| --- | --- |
| Legt Bomben auf der Stelle, wenn keine Steine mehr in Reichweite sind | Legt eine Bombe nur, wenn sie Steine zerstört oder einen Gegner trifft |
| Greift Gegner nur im Hunter-Modus an | Greift in jedem Spielmodus an (`Attack`) |
| Die eigenen Flammen stoppen an Löchern, Laufbändern, Pfeilen und Trampolinen, auf diesen Karten läuft sie in die eigenen Flammen | Flammen werden wie im Spiel berechnet, immer |
| Jede Bombe gilt als „explodiert jetzt“ | Weiß, wann jede Bombe explodiert, auch bei Kettenreaktionen, und prüft, ob die Flucht schnell genug ist (`TimedDanger`) |
| Zündet Fernzünder-Bomben, sobald sie sicher steht, ein Gegner in den Flammen hat keine Chance | Wartet mindestens `MinReactionMs`, bevor sie auf einen Gegner zündet (`Trigger`) |
| Sammelt Powerups nur zufällig ein, verbrennt sie mit ihren Bomben | Läuft zu guten Powerups und verbrennt sie nicht (`Powerups`) |
| Nutzt nie Kick, Punch, Grab oder Spooger | Nutzt sie (`Kick`, `Punch`, `Grab`, `Spooger`) |
| Steht ohne Ziel herum | Geht auf den nächsten Gegner zu (`Approach`) |

## Ausprobieren

Der Server lädt immer `ai.dll` (Linux: `libai.so`) aus seinem Ordner. Die Skripte bauen die Smart-KI und kopieren sie an die Stelle des Originals. Das Original wird einmal als `ai_original.dll` (`libai_original.so`) gesichert.

```sh
# Windows (PowerShell), der Spielordner ist der mit atomic_server.exe
ai_smart\try_ai.ps1 -GameDir <Spielordner>
ai_smart\try_ai.ps1 -GameDir <Spielordner> -Restore     # Original zurück

# Linux
ai_smart/try_ai.sh --gamedir <Spielordner>
ai_smart/try_ai.sh --gamedir <Spielordner> --restore
```

Ohne `-GameDir` / `--gamedir` wird der Ordner des Repositorys genommen. Vorher den Server beenden, oder in der Server-Konsole die KI mit `u` entladen und danach mit `a` neu laden. Das Server-Log zeigt `Atomic smart ai ver. ...`, wenn die Smart-KI geladen ist.

## Wo die Einstellungen liegen

1. `ai_smart.ini` neben dem Server, falls es sie gibt (zum Testen mit anderen Einstellungen).
2. Sonst `ai_smart.ini` im Konfigurationsordner des Spiels, neben `atomic_server.ini`:
   - Windows: `%LOCALAPPDATA%\fpc_atomic\ai_smart.ini`
   - Linux: `~/.config/fpc_atomic/ai_smart.ini`

Gibt es keine, schreibt die KI sie mit den Standardwerten und einem Kommentar zu jedem Schlüssel.
Die Datei wird zu Beginn jeder Runde neu gelesen. Änderungen wirken also ab der nächsten Runde, ohne Neustart.

## Wie Chance funktioniert

Jede Fähigkeit hat eine `Chance` von 0 bis 100:

- `0` = nie, `100` = immer.
- Werte dazwischen werden einmal gewürfelt, und das Ergebnis gilt für `RerollMs` (Standard 3 s). Würde die KI in jedem Frame (10 ms) würfeln, wäre jeder Wert über 0 fast „immer“.

Ungültige Werte werden auf ihren Bereich begrenzt, fehlende Schlüssel bekommen den Standardwert.

## Alle Schlüssel

### [General]

| Schlüssel | Standard | Bedeutung |
| --- | --- | --- |
| `Seed` | 0 | 0 = zufällig. Jede andere Zahl ergibt in gleichen Situationen gleiche Entscheidungen, für wiederholbare Tests. |
| `LogDecisions` | 0 | 1 = jedes neue Ziel jedes KI-Spielers wird in `ai_smart.log` neben der INI geschrieben (z. B. `P1 t=4200 attack (6,3)`). |
| `RerollMs` | 3000 | Wie lange ein gewürfeltes Ergebnis gilt (100..60000 ms). |

### Fähigkeiten

| Abschnitt | Standard | Bedeutung |
| --- | --- | --- |
| `[Attack]` | 100 | Legt Bomben, um Gegner zu treffen, in jedem Spielmodus. Im Hunter-Modus greift sie immer an, wie die originale KI. |
| `[Powerups]` | 100 | Läuft gezielt zu guten Powerups und verbrennt sie nicht. Wie sehr sie welches will: `[PowerupWeights]`. |
| `[Approach]` | 100 | Ohne anderes Ziel geht sie auf den nächsten Gegner zu und hält etwa 3 Felder Abstand. |
| `[TimedDanger]` | 100 | Gefahrenkarte mit der Explosionszeit jeder Bombe: Sie läuft über Felder, die später explodieren, und prüft, ob die Flucht schnell genug ist. `0` = jede Bombe gilt sofort als gefährlich, wie bei der originalen KI. |
| `[Trigger]` | 100 | Fernzünder-Bomben: auf einen Gegner erst nach `MinReactionMs` zünden. `0` = erst nach `MaxWaitMs`, nie gezielt auf einen Gegner. |
| `[Kick]` | 70 | Braucht das Kick-Item. Kickt eine Bombe zu einem Gegner in derselben Linie, oder kickt eine Bombe weg, wenn es keine andere Flucht gibt. |
| `[Punch]` | 50 | Braucht das Punch-Item. Schlägt eine benachbarte Bombe 3 Felder weit zu einem Gegner. Nur ohne eigene Fernzünder-Bomben (dieselbe Taste zündet sie). |
| `[Grab]` | 50 | Braucht das Grab-Item. Legt eine Bombe und wirft sie 3 Felder weit auf einen Gegner. |
| `[Spooger]` | 50 | Braucht das Spooger-Item. Legt eine Bombenreihe zu einem Gegner in derselben Linie. |
| `[Traps]` | 50 | Bevorzugt Angriffsfelder, nach denen der Gegner weniger freie Felder um sich hat. |
| `[TeamSafety]` | 100 | Legt nie eine Bombe, die einen Mitspieler trifft (Teamplay, und Jäger untereinander). |
| `[OpenSpace]` | 100 | Von zwei etwa gleich guten Zielen nimmt sie das mit mehr freien Nachbarfeldern (weniger Sackgassen). |
| `[Infect]` | 50 | Ist sie krank, läuft sie zum nächsten Gegner und steckt ihn an. |
| `[HuntedFlees]` | 70 | Hunter-Modus: Der Gejagte hält Abstand zu den Jägern, solange er höchstens eine Bombe hat. |
| `[Anticipate]` | 100 | Felder in der Flammenlinie eines Gegners, der noch Bomben hat, sind als Ziel weniger attraktiv. |
| `[ChainAttack]` | 50 | Zählt auch Gegner, die über eine Kettenreaktion getroffen werden (die neue Bombe zündet eine andere). |
| `[Conveyors]` | 100 | Betritt kein Laufband, das sie in ein gefährliches Feld schiebt. |
| `[RoundEnd]` | 100 | Hurry (die letzten 80 s einer Runde mit Zeitlimit): Felder, die bald zugemauert werden, gelten als gefährlich. |

### Weitere Schlüssel der Fähigkeiten

| Schlüssel | Standard | Bedeutung |
| --- | --- | --- |
| `[TimedDanger] SafetyMs` | 250 | Zusätzlicher Abstand in ms zwischen ihr und einer Explosion. Größer = vorsichtiger. |
| `[Trigger] MinReactionMs` | 600 | So lange liegt eine Fernzünder-Bombe mindestens, bevor sie auf einen Gegner gezündet wird. Kleiner = schneller und unfairer, etwa 600 entspricht einer menschlichen Reaktion. |
| `[Trigger] MaxWaitMs` | 2000 | Spätestens nach dieser Zeit wird eine Fernzünder-Bombe gezündet, auch ohne Gegner (wie eine normale Bombe). |
| `[Anticipate] Cost` | 3 | Wie viel weniger attraktiv so ein Feld ist (0..10). |
| `[RoundEnd] HurryLookAheadMs` | 3000 | Wie weit sie im Voraus auf Felder achtet, die der Hurry zumauert. |

### [Reaction]

| Schlüssel | Standard | Bedeutung |
| --- | --- | --- |
| `DelayMs` | 0 | Sie entscheidet nur alle `DelayMs` neu und läuft dazwischen weiter. 0 = in jedem Frame. Größer = langsamer, menschlicher, leichter. |
| `MistakeChance` | 0 | Wahrscheinlichkeit in %, dass eine Entscheidung durch einen zufälligen Schritt ersetzt wird (nie in Flammen oder ein gefährliches Feld). |

### [PowerupWeights]

Wie sehr sie jedes gute Powerup will, 0 (ignorieren) bis 10. Standard: `ExtraBomb=5`, `LongerFlame=5`, `GoldFlame=6`, `Speed=3`, `Kick=4`, `Spooger=2`, `Punch=2`, `Grab=2`, `Trigger=3`, `Jelly=1`.

## Profile

Die Werte in die Abschnitte von `ai_smart.ini` übernehmen.

**Wie die originale KI** (mit den Fehlerbehebungen: keine Bomben ohne Grund, richtige Flammen): jede `Chance=0`.

**Fair** (Standard): die Werte aus den Tabellen oben.

**Stark**: jede `Chance=100`, `MinReactionMs=300`, `SafetyMs=150`.

**Leicht**: `Attack 30`, `Kick`/`Punch`/`Grab`/`Spooger 0`, `DelayMs=150`, `MistakeChance=5`.

## Ein Problem eingrenzen

1. Mit den Standardwerten spielen und auf das Gesamtbild achten.
2. Sieht etwas falsch aus, `LogDecisions=1` setzen. Das Log zeigt, welches Ziel jeder KI-Spieler gewählt hat.
3. Einzelne Fähigkeiten nacheinander auf `Chance=0` setzen, bis die verursachende gefunden ist. Die Änderung wirkt ab der nächsten Runde.
4. Mit einer Zahl als `Seed` führt dieselbe Situation zu denselben Entscheidungen.

## Dateien

| Datei | Inhalt |
| --- | --- |
| `ai_smart.lpr` | Die Bibliothek mit denselben Exporten wie die originale KI |
| `uatomicai_smart.pas` | Die KI |
| `usmartsettings.pas` | Lesen und Schreiben von `ai_smart.ini`, das Log |
| `try_ai.ps1`, `try_ai.sh` | Bauen und Einsetzen zum Ausprobieren |
