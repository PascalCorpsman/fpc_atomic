# Spiel-Mods

[English version](MODS_MANUAL.md)

FPC Atomic hat einige optionale Spiel-Mods, die die Regeln des Originalspiels ändern. Alle Mods sind **standardmäßig aus**. Ohne Änderungen spielt sich das Spiel also genau wie das Original.

- [Mods einrichten](#mods-einrichten)
- [Stun bei Bombenlandung](#stun-bei-bombenlandung)
- [Boxhandschuh](#boxhandschuh)
- [Positive Krankheit](#positive-krankheit)
- [Durch Steine laufen](#durch-steine-laufen)
- [Weißer Bildschirm](#weißer-bildschirm)
- [Weiße Bombe](#weiße-bombe)
- [Flammenwerfer](#flammenwerfer)
- [Trefferpunkte (Handicap)](#trefferpunkte-handicap)
- [Geister](#geister)
- [Handschuh beim Start](#handschuh-beim-start)
- [Offene Grenzen](#offene-grenzen)
- [Teleporter](#teleporter)
- [Mod-Test-Modus](#mod-test-modus)
- [atomic_server.ini von Hand bearbeiten](#atomic_serverini-von-hand-bearbeiten)

## Mods einrichten

Die Mods sind eine Einstellung des **Servers**. Nur der PC, der das Spiel hostet, braucht sie. Alle anderen Spieler bekommen die Regeln vom Server und müssen nichts ändern.

Am einfachsten geht es im Launcher: **Mods...** anklicken, links die gewünschten Mods anhaken und rechts ihre Werte einstellen. **OK** speichert. Die Einstellungen gelten für das **nächste Spiel, das auf diesem PC gehostet wird** (der Server liest sie beim Start).

Gespeichert werden die Einstellungen in `atomic_server.ini` im Konfigurationsordner (Launcher: **Config folder**), siehe [atomic_server.ini von Hand bearbeiten](#atomic_serverini-von-hand-bearbeiten).

Die Oberfläche des Launchers ist englisch. Die Namen der Einstellungen stehen deshalb in den Tabellen so, wie sie im Dialog heißen.

### Allgemeine Regeln

- **Unverwundbare Spieler** blinken weiß (rot, wenn ihre eigene Farbe weiß ist). Flammen schaden ihnen nicht, und sie können nicht gestunnt werden.
- **Gestunnte Spieler** blinken schwarz (rot, wenn ihre eigene Farbe schwarz ist).
- **Der Hurry beendet jeden Schutz:** Wenn der Hurry beginnt (80 s vor Rundenende), endet jede Unverwundbarkeit, alle Geister verschwinden und jeder Treffer ist tödlich. In Runden mit unbegrenzter Zeit gibt es keinen Hurry.
- Überschneiden sich mehrere Schutzzeiten, zählt die längste. Sie werden nicht addiert.

## Stun bei Bombenlandung

Eine **geworfene** Bombe (Handschuh, siehe [erste Aktion](MANUAL.md#first-action-double-pressed)), die auf einem Spieler landet, stunnt diesen Spieler. Ein gestunnter Spieler kann sich weder bewegen noch eine Aktion ausführen. Geboxte Bomben stunnen nicht.

Der Stun trifft jeden auf dem Feld: Gegner, Teamkollegen und den Werfer selbst. Unverwundbare sind immun.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `StunOnBombLanding` | 0 (aus) | 0 / 1 |
| Min. stun time (ms) | `StunMinMs` | 100 | 0 .. 10000 |
| Max. stun time (ms) | `StunMaxMs` | 500 | 0 .. 10000, nicht kleiner als Min. |

Die Stun-Zeit ist zufällig zwischen Min. und Max.

## Boxhandschuh

Mit dem Box-Item ![](data/res/powpunch.png) stunnt die [zweite Aktion](MANUAL.md#second-action) einen **Gegner**, der auf dem Feld direkt vor dem Spieler steht (in Blickrichtung). Das geht auch im Stehen.

- Liegt auf demselben Feld eine Bombe, wird nur der Gegner gestunnt, die Bombe bleibt liegen. Ohne Gegner wird die Bombe wie gewohnt geboxt.
- Teamkollegen, Unverwundbare, fliegende und sterbende Spieler werden nicht gestunnt.
- Nach einem Schlag muss derselbe Spieler kurz warten, bevor er wieder schlagen kann. So kann niemand dauerhaft gestunnt werden.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `PunchStun` | 0 (aus) | 0 / 1 |
| Min. stun time (ms) | `PunchStunMinMs` | 300 | 0 .. 10000 |
| Max. stun time (ms) | `PunchStunMaxMs` | 800 | 0 .. 10000, nicht kleiner als Min. |
| Wait before next punch (ms) | `PunchStunCooldownMs` | 1500 | 0 .. 10000 |

## Positive Krankheit

Ein Teil der Krankheits-Items ![](data/res/powdisea.png) macht den Spieler für eine zufällige Zeit **unverwundbar** statt krank. Der Spieler sieht nicht, wann der Schutz endet.

Im Hurry ist ein Krankheits-Item immer eine normale Krankheit. Die schlimme Krankheit ![](data/res/powebola.png) bleibt unverändert.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `PositiveDisease` | 0 (aus) | 0 / 1 |
| Chance of a disease item (%) | `PositiveDiseaseChance` | 15 | 0 .. 100 |
| Min. invulnerable time (ms) | `PositiveMinMs` | 5000 | 0 .. 120000 |
| Max. invulnerable time (ms) | `PositiveMaxMs` | 20000 | 0 .. 120000, nicht kleiner als Min. |

## Durch Steine laufen

Ein Teil der Krankheits-Items ![](data/res/powdisea.png) lässt den Spieler für eine zufällige Zeit **durch zerstörbare Steine** laufen, statt ihn krank zu machen. Feste Blöcke bleiben fest. Solange der Effekt läuft, wird der Spieler durchsichtig gezeichnet.

- Endet die Zeit in einem Stein, kann der Spieler ihn nur noch verlassen. In weitere Steine kommt er nur, solange kein Nachbarfeld frei ist. So wird niemand eingesperrt.
- In einem Stein können keine Bomben gelegt werden.
- Explodiert der Stein, während der Spieler darin steht, wird er getroffen wie von einer Flamme.
- Items in einem Stein können erst aufgenommen werden, wenn der Stein weg ist.
- Im Hurry ist ein Krankheits-Item immer eine normale Krankheit, und ein laufender Effekt endet (aus einem Stein kommt man noch heraus).
- Ist die [positive Krankheit](#positive-krankheit) auch an, wird sie zuerst gewürfelt. Die Chance hier gilt für die übrigen Krankheits-Items.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `BrickWalk` | 0 (aus) | 0 / 1 |
| Chance of a disease item (%) | `BrickWalkChance` | 15 | 0 .. 100 |
| Min. time (ms) | `BrickWalkMinMs` | 5000 | 0 .. 120000 |
| Max. time (ms) | `BrickWalkMaxMs` | 10000 | 0 .. 120000, nicht kleiner als Min. |
| See-through (%) | `BrickWalkTransparency` | 50 | 0 .. 100, 0 = normal, 100 = unsichtbar |

Die Durchsichtigkeit entsteht, indem der Spieler nur in einem Teil der Bilder gezeichnet wird. Er kann deshalb etwas flackern.

## Weißer Bildschirm

Ein Teil der Krankheits-Items ![](data/res/powdisea.png) macht den Bildschirm des Spielers für eine Weile weiß, statt einer normalen Krankheit. Der Bildschirm bleibt eine einstellbare Zeit weiß und blendet dann zurück.

- Er wird nach den positiven Effekten gewürfelt ([positive Krankheit](#positive-krankheit), [durch Steine laufen](#durch-steine-laufen)).
- KI-Spieler sind nicht betroffen.
- Spielen zwei Spieler an einem PC, zeigt dieser PC keinen weißen Bildschirm, weil sich beide den Bildschirm teilen.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `FlashDisease` | 0 (aus) | 0 / 1 |
| Chance of a disease item (%) | `FlashDiseaseChance` | 15 | 0 .. 100 |
| Strength (%) | `FlashDiseaseIntensity` | 90 | 0 .. 100, 100 = man sieht nichts mehr |
| Duration (ms) | `FlashDiseaseMs` | 2500 | 0 .. 10000 |
| Fade back (ms) | `FlashDiseaseFadeMs` | 2000 | 0 .. 10000 |

## Weiße Bombe

Ein Teil der Items unter den Steinen sind **weiße Bomben**. Sie sehen aus wie ein weißes Extra-Bomben-Item. Nach dem Aufheben ist die nächste gelegte Bombe weiß:

- Sie explodiert nach der normalen Zeit, aber ohne Flammen. Sie zerstört nichts und tötet niemanden.
- Sie macht die Bildschirme **aller Gegner** auf dem ganzen Feld weiß, wie die Krankheit [weißer Bildschirm](#weißer-bildschirm). Teamkollegen und der Spieler, der sie gelegt hat, sind nicht betroffen.
- Sie ist eine Extra-Bombe. Sie zählt nicht zu den Bomben des Spielers, danach legt er wieder normale Bomben.
- Sie kann gekickt, geworfen und geboxt werden wie jede andere Bombe.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `WhiteBomb` | 0 (aus) | 0 / 1 |
| Share of the items (%) | `WhiteBombChance` | 10 | 0 .. 100 |
| Strength (%) | `WhiteBombIntensity` | 90 | 0 .. 100, 100 = man sieht nichts mehr |
| Duration (ms) | `WhiteBombMs` | 2500 | 0 .. 10000 |
| Fade back (ms) | `WhiteBombFadeMs` | 2000 | 0 .. 10000 |

## Flammenwerfer

Ein Teil der Items unter den Steinen sind **Flammenwerfer**. Sie sehen aus wie das Flammen-Item ![](data/res/powflame.png) mit rotem Rahmen. Solange der Spieler Tank hat, feuert er mit **gehaltener zweiter Aktion** einen Strahl in Blickrichtung.

- Der Strahl reicht eine einstellbare Zahl von Feldern weit. Er stoppt an Steinen und festen Blöcken, ohne sie zu zerstören, und zündet Bomben.
- Items im Strahl verbrennen sofort, wie bei einer Flamme. Das lässt sich abschalten, dann geht der Strahl über sie hinweg.
- Ein Gegner, der eine einstellbare Zeit am Stück im Strahl steht, wird getroffen wie von einer Flamme. Wer den Strahl verlässt, fängt wieder bei 0 an. Teamkollegen werden nicht verletzt.
- Der Tank leert sich nur beim Feuern. Ist er leer, ist der Flammenwerfer weg. Weitere Flammenwerfer-Items füllen den Tank auf.
- Solange der Spieler Tank hat, zündet und boxt die zweite Aktion keine Bomben.
- Beim Feuern läuft man normal schnell, der Strahl dreht sich mit.
- Im Mod-Test-Modus gibt F4 (alle Items) auch Tank.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `FlameThrower` | 0 (aus) | 0 / 1 |
| Share of the items (%) | `FlameThrowerChance` | 5 | 0 .. 100 |
| Fuel per item (ms) | `FlameThrowerFuelMs` | 5000 | 0 .. 60000 |
| Range (tiles) | `FlameThrowerRange` | 3 | 1 .. 14 |
| Time to kill (ms) | `FlameThrowerKillMs` | 1000 | 0 .. 10000 |
| Burn items in the beam | `FlameThrowerBurnItems` | 1 (an) | 0 / 1 |

## Trefferpunkte (Handicap)

Spieler überstehen mehrere Treffer. Jeder Treffer kostet einen Trefferpunkt (auch ein Treffer durch die eigene Bombe) und macht den Spieler kurz unverwundbar, damit dieselbe Flamme nicht zweimal trifft. Nur der letzte Treffer ist tödlich, und nur er zählt als Kill.

Die Trefferpunkte werden **pro Spielername** in einer Handicap-Tabelle eingestellt. Schwächere Spieler können so mehr Trefferpunkte bekommen als starke. Alle Spieler, die nicht in der Tabelle stehen, bekommen die Standard-Trefferpunkte. Zu Beginn jeder Runde werden die Trefferpunkte wieder aufgefüllt.

Im Hurry ist jeder Treffer tödlich, egal wie viele Trefferpunkte noch übrig sind.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `HitPoints` | 0 (aus) | 0 / 1 |
| Invulnerable after a hit (ms) | `InvulnTimeMs` | 5000 | 500 .. 60000 |
| Default hit points | `DefaultHitPoints` | 1 | 1 .. 255 |
| Show hit points above the players | `ShowHitPoints` | 1 (an) | 0 / 1 |
| Handicap-Tabelle | Abschnitt `[Handicap]` | leer | `Spielername=Trefferpunkte`, 1 .. 255 |

**Zwei Spieler an einem PC:** Der zweite Spieler an einem PC (Tastatur 2) hat denselben Spielernamen wie der erste. In der Handicap-Tabelle steht er als `Name~2`, z. B.

```ini
[Handicap]
Anna=3
Anna~2=1
```

Ist die Anzeige an, steht die Zahl nur über einem Spieler, solange er mehr als 1 Trefferpunkt hat.

## Geister

Ein Spieler, der stirbt, verlässt die Runde nicht. Er läuft als kleiner, flackernder **Geist** auf einem Rahmen rund um das Spielfeld weiter (außerhalb des Spielfelds).

- **Bewegen:** Die Richtungstasten bewegen den Geist am Rahmen entlang. Wenn er stehen bleibt, schaut er immer ins Feld.
- **Werfen:** Die [erste Aktion](MANUAL.md#first-action) wirft eine Geisterbombe gerade ins Feld: Sie fliegt 3 Felder vom Rand in die Zeile oder Spalte vor dem Geist. Es kann immer nur eine Geisterbombe unterwegs sein. Die Flammenlänge der Geisterbombe ist einstellbar.
- **Zurückkommen:** Trifft die Flamme einer Geisterbombe einen **Gegner** (Teamkollegen zählen nicht), kommt der Geist an seiner Startposition ins Spiel zurück (oder auf dem nächsten freien Feld). Er hat dann die Fähigkeiten aus dem Schema (seine Items wurden beim Tod verteilt), volle Trefferpunkte und ist kurz unverwundbar.
- Für eine Geisterbombe bekommt der Geist keinen Kill. Der getroffene Spieler stirbt (oder verliert einen Trefferpunkt) wie gewohnt.
- Ist [Stun bei Bombenlandung](#stun-bei-bombenlandung) an, stunnen Geisterbomben wie geworfene Bomben.
- **Grenzen:** Ein Spieler kann pro Runde nur begrenzt oft Geist werden. Ein Geist, der innerhalb einer einstellbaren Zeit keinen Gegner trifft, verschwindet. In den letzten 5 s davor flackert er schneller.
- Mit dem Hurry verschwinden alle Geister.
- Die Runde endet wie gewohnt, wenn nur noch ein Spieler (oder Team) lebt, Geister zählen nicht. Geister spielen also erst ab 3 Spielern eine Rolle.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `Ghosts` | 0 (aus) | 0 / 1 |
| Flame length of the ghost bomb | `GhostFlameLen` | 2 | 1 .. 99 |
| Invulnerable after return (ms) | `GhostReviveInvulnMs` | 3000 | 0 .. 60000 |
| Max. times ghost per round | `GhostMaxCount` | 1 | 0 .. 99, 0 = unbegrenzt |
| Max. time as ghost (ms) | `GhostMaxTimeMs` | 0 | 0 .. 600000, 0 = unbegrenzt |
| Flicker time (ms) | `GhostFlickerMs` | 200 | 0 .. 5000, 0 = kein Flackern |

Die Flackerzeit ist die Länge eines Takts: Ein Geist ist 3 Takte sichtbar und 1 Takt unsichtbar.

## Handschuh beim Start

Manchmal beginnt eine Runde in einer kleinen Nische zwischen Steinen, und eine versehentlich gelegte Bombe sperrt den Spieler ein. Mit diesem Mod kann jeder Spieler die Bombe unter sich werfen wie mit dem Handschuh ![](data/res/powgrab.png) (die [erste Aktion](MANUAL.md#first-action-double-pressed) zweimal drücken), **bis zum ersten Schritt** der Runde.

- Eine Richtungstaste gegen eine Wand dreht den Spieler nur, ohne Schritt. So lässt sich der Wurf zielen.
- Spieler mit Spooger ![](data/res/powspoog.png) spoogen stattdessen wie gewohnt.
- Ein Geist, der ins Spiel zurückkommt, bekommt den Handschuh nicht noch einmal.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `GloveAtStart` | 0 (aus) | 0 / 1 |

## Offene Grenzen

Spieler können über den Rand des Spielfelds laufen und kommen auf der Gegenseite heraus, oben <-> unten und / oder links <-> rechts.

- Die Grenzen öffnen sich eine einstellbare Zeit nach Rundenbeginn. So kann niemand sofort aus seiner Startecke fliehen.
- Gelbe Pfeile auf dem Rahmen um das Feld zeigen, welche Grenzen offen sind. In den letzten 3 s vor dem Öffnen blinken sie.
- Wer auf der Mitte einer Randkachel steht und weiter nach außen läuft, steht sofort auf der Gegenseite und läuft dort weiter.
- Ist das Feld gegenüber belegt (Stein, fester Block, Bombe), wirkt der Rand wie eine Wand.
- Nur Spieler gehen über den Rand. Gekickte Bomben und Flammen bleiben wie gewohnt am Rand stehen.
- Mit dem Hurry schließen sich die Grenzen wieder.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `OpenBorders` | 0 (aus) | 0 / 1 |
| Top <-> bottom | `OpenBordersTopBottom` | 1 (an) | 0 / 1 |
| Left <-> right | `OpenBordersLeftRight` | 1 (an) | 0 / 1 |
| Open after (s) | `OpenBordersActiveSec` | 30 | 0 .. 600, 0 = ab Rundenbeginn |

## Teleporter

Teleporter in der Mitte der Ränder bringen den Spieler zum Teleporter am gegenüberliegenden Rand, oben <-> unten und / oder links <-> rechts. Sie funktionieren wie die Löcher mancher Karten und sehen auch so aus, mit einem türkisen Ring darum.

- Die Teleporter erscheinen eine einstellbare Zeit nach Rundenbeginn. Vorher sind es normale Felder.
- Die Teleporter-Felder und die Felder daneben (zur Feldmitte hin) bleiben von Anfang an frei von Steinen.
- Auf einen Teleporter kann keine Bombe gelegt werden, rollende Bomben bleiben davor stehen.
- Im Hurry sind die Teleporter weg.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `Teleporters` | 0 (aus) | 0 / 1 |
| Top <-> bottom | `TeleportersTopBottom` | 1 (an) | 0 / 1 |
| Left <-> right | `TeleportersLeftRight` | 1 (an) | 0 / 1 |
| Appear after (s) | `TeleportersActiveSec` | 60 | 0 .. 600, 0 = ab Rundenbeginn |

## Mod-Test-Modus

Mit dem Mod-Test-Modus lassen sich die Mods ausprobieren, ohne im Spiel auf die passende Situation zu warten. **Er ist nur zum Testen gedacht:** Solange er an ist, sehen alle Spieler unten im Bild den Hinweis `MOD TEST MODE - F3: HELP` und über den Spielern die Restzeiten von Unverwundbarkeit (`I`), Stun (`S`) und „durch Steine laufen“ (`B`).

Nur der Spieler, der das Spiel hostet (der Master), kann während einer Runde die Test-Tasten benutzen:

| Taste | Wirkung |
|---|---|
| F1 | Die nächste Bombe ist eine weiße Bombe |
| F2 | Weißer Bildschirm |
| F3 | Hilfe mit allen Test-Tasten ein- / ausblenden |
| F4 | Alle Items (Bomben, Kick, Boxen, Handschuh, ..., mit Flammenwerfer-Mod auch Tank) |
| F5 | Positive Krankheit (unverwundbar) |
| F6 | Mich treffen (kostet einen Trefferpunkt oder tötet) |
| F7 | Mich zum Geist machen / zurückholen |
| F8 | Hurry sofort starten |
| F9 | Mich stunnen |
| F11 | Meine Trefferpunkte auffüllen |
| F12 | Durch Steine laufen |

Mit gehaltener **Shift**-Taste wirkt die Taste auf den zweiten Spieler an diesem PC. F10 wird nicht benutzt, weil Windows damit das Fenstermenü öffnet.

Die Test-Tasten ignorieren die Grenzen der Mods (z. B. geht F7 öfter als `GhostMaxCount`). So lassen sich Randfälle testen.

Diese Einstellungen wirken nur, solange der Mod-Test-Modus an ist:

| Einstellung (Dialog) | ini-Schlüssel (Abschnitt `[Debug]`) | Default | Bereich |
|---|---|---|---|
| Mod test mode an / aus | `ModTestMode` | 0 (aus) | 0 / 1 |
| Start items | `StartItems` | leer | z. B. `kick,punch,grab,bomb*3` |
| Round time (s, 0 = normal) | `RoundTimeSec` | 0 | 0 .. 3600, 0 = Zeit aus den Spieleinstellungen |
| Invulnerable at start (ms) | `StartInvulnMs` | 0 | 0 .. 600000 |
| Ghosts count as alive | `GhostsKeepRound` | 0 (aus) | 0 / 1 |

- **Start items:** Jeder Spieler bekommt diese Items zu Beginn jeder Runde. Kommagetrennt, `*n` nimmt ein Item n-mal. Mögliche Items: `bomb`, `flame`, `goldflame`, `speed`, `kick`, `punch`, `grab`, `spooger`, `jelly`, `trigger`. `trigger` gehört hinter `bomb`, es gilt für die Bomben davor.
- **Round time:** Der Hurry beginnt bei 80 s Restzeit, `90` startet den Hurry also nach 10 s.
- **Ghosts count as alive:** Die Runde endet nicht, solange noch Geister übrig sind. So lassen sich Geister auch mit nur 2 Spielern testen.

## atomic_server.ini von Hand bearbeiten

Die Datei liegt im Konfigurationsordner von FPC Atomic:

- Windows: `%LOCALAPPDATA%\fpc_atomic\atomic_server.ini`
- Linux: `~/.config/fpc_atomic/atomic_server.ini`

Beispiel:

```ini
[Mods]
; Kommentare nur in einer eigenen Zeile
StunOnBombLanding=1
StunMinMs=100
StunMaxMs=500
Ghosts=1
GhostMaxCount=1

[Debug]
ModTestMode=0

[Handicap]
Anna=3
Anna~2=1
```

Bitte beachten:

- **Keine Kommentare hinter Werten.** Ein Kommentar muss in einer eigenen Zeile stehen und mit `;` beginnen. `HitPoints=0 ; aus` wird als „an“ gelesen, Zahlen mit Kommentar dahinter fallen auf ihren Default zurück.
- `1` heißt an, `0` heißt aus.
- Steht ein Schlüssel zweimal da, zählt der erste.
- Fehlende Schlüssel werden beim Serverstart mit ihrem Default angelegt. So findet man alle Schlüssel in der Datei.
- Ungültige Werte werden auf den nächsten gültigen Wert gesetzt. Das Server-Log (Konfigurationsordner, `log/server.log`) zeigt für jeden eine Warnung.
- Der Server liest die Datei nur beim Start. Änderungen gelten ab dem nächsten gehosteten Spiel.
- Der Dialog **Mods...** im Launcher schreibt den Abschnitt `[Handicap]` komplett neu. Kommentare in diesem Abschnitt gehen verloren, Kommentare in allen anderen Abschnitten bleiben erhalten.
