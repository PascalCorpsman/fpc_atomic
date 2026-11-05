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

## Die KI wählen

Die Smart-KI liegt neben der originalen KI (`ai_smart.dll`, Linux: `libai_smart.so`, der Launcher lädt sie mit den Updates herunter).

- **Launcher:** Spieloptionen, **KI** = Original oder Smart, **Schwierigkeit** 0..100 für die Smart-KI. Das Spiel gibt die Wahl an den Server weiter, den es hostet.
- **Eigener Server:** `atomic_server -p 5521 -ai ai_smart`. Ohne `-ai`, oder wenn die Datei fehlt, lädt der Server die originale KI.

Das Server-Log zeigt `Atomic smart ai ver. ...`, wenn die Smart-KI geladen ist.

**Für Entwickler:** `ai_smart\try_ai.ps1 -GameDir <Spielordner>` (Linux: `ai_smart/try_ai.sh --gamedir <Spielordner>`) baut die Smart-KI und kopiert sie in den Spielordner, neben das Original. Ohne den Parameter wird der Ordner des Repositorys genommen.

## Wo die Einstellungen liegen

1. `ai_smart.ini` neben dem Server, falls es sie gibt (zum Testen mit anderen Einstellungen).
2. Sonst `ai_smart.ini` im Konfigurationsordner des Spiels, neben `atomic_server.ini`:
   - Windows: `%LOCALAPPDATA%\fpc_atomic\ai_smart.ini`
   - Linux: `~/.config/fpc_atomic/ai_smart.ini`

Gibt es keine, schreibt die KI sie mit den Standardwerten und einem Kommentar zu jedem Schlüssel.
Die Datei wird zu Beginn jeder Runde neu gelesen. Änderungen wirken also ab der nächsten Runde, ohne Neustart.

## Schwierigkeit

`[General] Difficulty` von 0 bis 100 stellt die Stärke mit einer Zahl ein (Standard 50):

| | 0 (leicht) | 50 (Standard) | 100 (stark) |
| --- | --- | --- | --- |
| Reaktion (`DelayMs`) | 250 ms | 125 ms | 0 |
| Fehler (`MistakeChance`) | 10 % | 5 % | 0 |
| Fernzünder frühestens (`MinReactionMs`) | 1000 ms | 650 ms | 300 ms |
| Angriff | 30 % | 65 % | 100 % |
| Spooger, Punch, Grab | 0 | 35 % | 70 % |
| Kick | 0 | 45 % | 90 % |
| Fallen, Kettenreaktionen, Vorausahnen | 0 | 50 % | 100 % |

Überleben (Gefahrenkarte mit Zeit, Laufbänder, Hurry) und keine Treffer auf Mitspieler bleiben bei jeder Schwierigkeit an: Eine leichtere KI ist langsamer und macht Fehler, sprengt sich aber nicht absichtlich selbst in die Luft.

In der Spieler-Einstellung bekommen die Smart-KIs einen Namen nach der Schwierigkeit, mit der Nummer ihres Platzes (z. B. `Nitro 3`): 0 % Pipo, 11 % Bombo, 22 % Kiko, 33 % Zappy, 44 % Fuego, 56 % Nitro, 67 % Blazer, 78 % Blaze, 89 % Inferno, 100 % Armageddon (es zählt die nächste Stufe, 50 = Nitro). In der Statistik (`Playerstats.csv` im Konfigurationsordner) zählen alle Smart-KIs einer Stufe zusammen, z. B. als `Smart Nitro`. So sieht man, wie man gegen jede Stufe abschneidet.

In der Datei sind die Werte, die die Schwierigkeit setzt, auskommentiert (`;Chance=65`). Entfernt man das `;`, gilt der eigene Wert und überschreibt die Schwierigkeit. Eine Datei der Version 0.01 (ohne `Difficulty`) wird als `ai_smart.ini.bak` gesichert und neu geschrieben.

## Wie Chance funktioniert

Jede Fähigkeit hat eine `Chance` von 0 bis 100:

- `0` = nie, `100` = immer.
- Werte dazwischen werden einmal gewürfelt, und das Ergebnis gilt für `RerollMs` (Standard 3 s). Würde die KI in jedem Frame (10 ms) würfeln, wäre jeder Wert über 0 fast „immer“.

Ungültige Werte werden auf ihren Bereich begrenzt, fehlende Schlüssel bekommen den Standardwert.

## Alle Schlüssel

### [General]

| Schlüssel | Standard (Schwierigkeit 0 / 50 / 100) | Bedeutung |
| --- | --- | --- |
| `Difficulty` | 50 | 0 = leicht, 50 = normal, 100 = stark. Setzt die unten mit 0 / 50 / 100 angegebenen Werte. |
| `Seed` | 0 | 0 = zufällig. Jede andere Zahl ergibt in gleichen Situationen gleiche Entscheidungen, für wiederholbare Tests. |
| `LogDecisions` | 0 | 1 = jedes neue Ziel jedes KI-Spielers wird in `ai_smart.log` neben der INI geschrieben (z. B. `P1 t=4200 attack (6,3)`). |
| `RerollMs` | 3000 | Wie lange ein gewürfeltes Ergebnis gilt (100..60000 ms). |

### Fähigkeiten

| Abschnitt | Standard (Schwierigkeit 0 / 50 / 100) | Bedeutung |
| --- | --- | --- |
| `[Attack]` | 30 / 65 / 100 | Legt Bomben, um Gegner zu treffen, in jedem Spielmodus. Im Hunter-Modus greift sie immer an, wie die originale KI. |
| `[Powerups]` | 40 / 70 / 100 | Läuft gezielt zu guten Powerups und verbrennt sie nicht. Wie sehr sie welches will: `[PowerupWeights]`. |
| `[Approach]` | 30 / 65 / 100 | Ohne anderes Ziel geht sie auf den nächsten Gegner zu und hält etwa 3 Felder Abstand. |
| `[TimedDanger]` | 100 | Gefahrenkarte mit der Explosionszeit jeder Bombe: Sie läuft über Felder, die später explodieren, und prüft, ob die Flucht schnell genug ist. `0` = jede Bombe gilt sofort als gefährlich, wie bei der originalen KI. |
| `[Trigger]` | 100 | Fernzünder-Bomben: auf einen Gegner erst nach `MinReactionMs` zünden. `0` = erst nach `MaxWaitMs`, nie gezielt auf einen Gegner. |
| `[Kick]` | 0 / 45 / 90 | Braucht das Kick-Item. Kickt eine Bombe zu einem Gegner in derselben Linie, oder kickt eine Bombe weg, wenn es keine andere Flucht gibt. |
| `[Punch]` | 0 / 35 / 70 | Braucht das Punch-Item. Schlägt eine benachbarte Bombe 3 Felder weit zu einem Gegner. Nur ohne eigene Fernzünder-Bomben (dieselbe Taste zündet sie). |
| `[Grab]` | 0 / 35 / 70 | Braucht das Grab-Item. Legt eine Bombe und wirft sie 3 Felder weit auf einen Gegner. |
| `[Spooger]` | 0 / 35 / 70 | Braucht das Spooger-Item. Legt eine Bombenreihe zu einem Gegner in derselben Linie. |
| `[Traps]` | 0 / 50 / 100 | Bevorzugt Angriffsfelder, nach denen der Gegner weniger freie Felder um sich hat. |
| `[TeamSafety]` | 100 | Legt, kickt oder zündet nie eine Bombe, die einen Mitspieler trifft (Teamplay, und Jäger untereinander). Unter 100 achtet sie manchmal nicht darauf. |
| `[OpenSpace]` | 50 / 75 / 100 | Von zwei etwa gleich guten Zielen nimmt sie das mit mehr freien Nachbarfeldern (weniger Sackgassen). |
| `[Infect]` | 0 / 35 / 70 | Ist sie krank, läuft sie zum nächsten Gegner und steckt ihn an. |
| `[HuntedFlees]` | 30 / 65 / 100 | Hunter-Modus: Der Gejagte hält Abstand zu den Jägern, solange er höchstens eine Bombe hat. |
| `[Anticipate]` | 0 / 50 / 100 | Felder in der Flammenlinie eines Gegners, der noch Bomben hat, sind als Ziel weniger attraktiv. |
| `[ChainAttack]` | 0 / 50 / 100 | Zählt auch Gegner, die über eine Kettenreaktion getroffen werden (die neue Bombe zündet eine andere). |
| `[Conveyors]` | 100 | Betritt kein Laufband, das sie in ein gefährliches Feld schiebt. |
| `[RoundEnd]` | 100 | Hurry (die letzten 80 s einer Runde mit Zeitlimit): Felder, die bald zugemauert werden, gelten als gefährlich. |
| `[KickSnipe]` | 0 / 40 / 80 | Braucht Kick und Fernzünder. Legt eine Fernzünder-Bombe, kickt sie in eine freie Linie zu einem Gegner und zündet, wenn der Gegner in den Flammen steht. Eine gekickte Bombe hält vor Spielern an, ein Gegner in der Linie ist daher das beste Ziel. Mehrere Bomben reihen sich hintereinander auf (sie gehen zusammen hoch). Mit Jelly pendelt die Bombe zwischen dem nächsten Hindernis und der KI, die danach seitlich aus der Linie tritt. |
| `[Stun]` | 0 / 50 / 100 | Boxhandschuh-Mod, braucht das Punch-Item: betäubt einen Gegner direkt vor der KI (nur ohne eigene Fernzünder-Bomben, dieselbe Taste zündet sie). |
| `[BrickWalk]` | 30 / 65 / 100 | Mod „durch Blöcke laufen“: läuft über Steine, solange die Zeit reicht, auch zur Flucht, und bleibt nie in einem stehen, wenn sie endet. |
| `[Borders]` | 50 / 75 / 100 | Mod „offene Grenzen“: läuft über einen offenen Rand hinaus (kommt auf der anderen Seite herein), auch zur Flucht, und wirft Bomben darüber (Grab). Teleporter meidet sie wie Löcher. |
| `[FlameBeam]` | 100 | Flammenwerfer-Mod: meidet den Strahl eines feuernden Spielers wie Flammen, und die Linie vor einem Gegner mit Tankfüllung. |
| `[Invuln]` | 30 / 65 / 100 | Unverwundbarkeit (Mods): greift keine Gegner an, die gerade nicht getroffen werden können, und läuft durch Flammen, solange sie selbst unverwundbar ist. |
| `[WhiteBomb]` | 100 | Weiße-Bombe-Mod: flieht nicht vor einer weißen Bombe (keine Flammen) und legt eine eigene weiße Bombe sofort (sie macht nur die Bildschirme der Gegner weiß). |
| `[StartGlove]` | 0 / 50 / 100 | Mod „Handschuh beim Start“: wirft vor dem ersten Schritt eine Bombe in Steine oder auf einen Gegner. |
| `[NoCover]` | 100 / 50 / 0 | Flammenwerfer-Mod: Der Strahl endet an Bomben. Steht die KI untätig hinter einer Bombe in der Linie eines Gegners mit Flammenwerfer, tritt sie aus der Deckung, damit man sie treffen kann (eine starke KI bleibt in Deckung). |

### Weitere Schlüssel der Fähigkeiten

| Schlüssel | Standard (Schwierigkeit 0 / 50 / 100) | Bedeutung |
| --- | --- | --- |
| `[TimedDanger] SafetyMs` | 250 | Zusätzlicher Abstand in ms zwischen ihr und einer Explosion. Größer = vorsichtiger. |
| `[Trigger] MinReactionMs` | 1000 / 650 / 300 | So lange liegt eine Fernzünder-Bombe mindestens, bevor sie auf einen Gegner gezündet wird. Kleiner = schneller und unfairer, etwa 600 entspricht einer menschlichen Reaktion. |
| `[Trigger] MaxWaitMs` | 2000 | Spätestens nach dieser Zeit wird eine Fernzünder-Bombe gezündet, auch ohne Gegner (wie eine normale Bombe). |
| `[Anticipate] Cost` | 3 | Wie viel weniger attraktiv so ein Feld ist (0..10). |
| `[RoundEnd] HurryLookAheadMs` | 3000 | Wie weit sie im Voraus auf Felder achtet, die der Hurry zumauert. |
| `[FlameBeam] ReactionMs` | 2200 / 1400 / 600 | Die KI bemerkt einen Flammenstrahl erst, wenn er so lange feuert (im Strahl: wenn sie so lange getroffen wird), wie ein Mensch, der erst reagieren muss. Der Strahl tötet nach `FlameThrowerKillMs` (Mods, Standard 1500) im Strahl: Liegt der Wert in der Nähe, ist die KI zu treffen, kleiner = schwerer zu treffen. |
| `[FlameBeam] SlowPercent` | 50 / 25 / 0 | Im Strahl läuft die KI um so viel Prozent langsamer (sie setzt Schritte aus), wie ein Spieler, der gerade brennt. |
| `[FlameBeam] FreezeMs` | 1500 / 750 / 0 | Vom Strahl getroffen erschrickt die KI und bleibt erst so lange stehen (nicht, wenn gleich eine Bombe explodiert). |
| `[KickSnipe] HitChance` | 30 / 60 / 90 | Wahrscheinlichkeit in %, eine gekickte Bombe im richtigen Moment zu zünden, sonst 300..800 ms zu spät (der Gegner ist dann vielleicht weg). |
| `[KickSnipe] WaitMs` | 8000 | So lange wartet die KI mit ihren Fernzünder-Bomben auf einen Gegner (das Spiel macht nach 15 s normale Bomben daraus). |
| `[KickSnipe] EndgameBricks` | 10 | Endspiel: Liegen höchstens so viele Steine auf dem Feld, snipt die KI immer, wenn sie kann (außer bei `Chance=0`). |

### [Reaction]

| Schlüssel | Standard (Schwierigkeit 0 / 50 / 100) | Bedeutung |
| --- | --- | --- |
| `DelayMs` | 250 / 125 / 0 | Sie entscheidet nur alle `DelayMs` neu und läuft dazwischen weiter. 0 = in jedem Frame. Größer = langsamer, menschlicher, leichter. |
| `MistakeChance` | 10 / 5 / 0 | Wahrscheinlichkeit in %, dass eine Entscheidung durch einen zufälligen Schritt ersetzt wird (nie in Flammen oder ein gefährliches Feld). |

### [PowerupWeights]

Wie sehr sie jedes gute Powerup will, 0 (ignorieren) bis 10. Standard: `ExtraBomb=5`, `LongerFlame=5`, `GoldFlame=6`, `Speed=3`, `Kick=4`, `Spooger=2`, `Punch=2`, `Grab=2`, `Trigger=3`, `Jelly=1`.

## Profile

**Leicht**: `Difficulty=0`. **Normal**: `Difficulty=50` (Standard). **Stark**: `Difficulty=100`.

**Wie die originale KI** (mit den Fehlerbehebungen: keine Bomben ohne Grund, richtige Flammen): jede `Chance=0` setzen (das `;` entfernen).

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
| `try_ai.ps1`, `try_ai.sh` | Bauen und in einen Spielordner kopieren (für Entwickler) |
