# Spieloptionen und Spielmodi

[English version](GAMEOPTIONS_MANUAL.md)

Diese Anleitung erklärt die Spieloptionen (Launcher: **Spieloptionen...**, im Spiel: das Optionen-Menü) und die Spielmodi (Launcher: **Spielmodus...**). Die optionalen Regeländerungen stehen in der [Anleitung der Mods](MODS_MANUAL_DE.md).

- [Teil 1: Spieloptionen](#teil-1-spieloptionen)
- [Teil 2: Spielmodi](#teil-2-spielmodi)
  - [Klassisch](#klassisch)
  - [Hunter](#hunter)
- [FAQ](#faq)

## Teil 1: Spieloptionen

Die Spieloptionen stehen in `fpc_atomic.cfg` im Konfigordner (Launcher: **Konfigordner**). Der Dialog **Spieloptionen...** im Launcher und das Optionen-Menü im Spiel ändern dieselben Werte. Änderungen im Launcher gelten ab dem nächsten Start des Spiels.

Die meisten Optionen sind eine Einstellung des **Hosts**: Wer ein Spiel hostet, mit dessen Teamspiel, Schema, Karte, Spielzeit, Siegen und Förderband-Tempo spielt der Server. Die anderen Spieler müssen nichts ändern.

| Option | Default | Bedeutung |
|---|---|---|
| Teamspiel | aus | Die Spieler spielen in zwei Teams, Rot und Weiß. Jeder Spieler wählt sein Team im Spieler-Setup. Ein Team gewinnt die Runde, wenn nur noch Spieler dieses Teams leben, und jeder Spieler des Siegerteams bekommt den Sieg, auch wenn er gestorben ist. |
| Zufällige Startpositionen | an | Die Spieler starten auf zufälligen Startpositionen des Schemas. Aus: Spieler 1 startet immer auf Startposition 1, Spieler 2 auf Startposition 2 usw. |
| Getrennte Netzspieler werden zur KI | aus | Verliert ein Netzwerkspieler während eines Matches die Verbindung, übernimmt eine KI seinen Platz. Aus: Der Spieler scheidet aus. Solange sich die Spieler noch verbinden, scheidet ein verlorener Spieler immer einfach aus. |
| Schema | BASIC.SCH | Das Schema legt die Steine, die Startpositionen und die Items unter den Steinen fest (Dateien in `data/schemes`). |
| Karte | Jedes Spiel zufällig | Die Grafik und der Sound des Spielfelds (Dateien in `data/maps`). Die Vorschau im Launcher zeigt die Karte mit allen Steinen des Schemas. Im Spiel fehlt je nach Steindichte des Schemas zufällig ein Teil der Steine. |
| Spielzeit | 2:30 | Dauer einer Runde: Unendlich oder 1:30 bis 10:00 in 30-s-Schritten. 85 s vor dem Ende erscheint „Hurry“, 80 s vor dem Ende beginnt der Hurry (siehe unten). Ist die Zeit um und hat niemand gewonnen, ist die Runde unentschieden. Unendlich = keine Zeitgrenze und kein Hurry. |
| Siege für den Matchgewinn | 3 | Wie viele Runden ein Spieler (oder ein Team) gewinnen muss, um das Match zu gewinnen, 1 bis 99. |
| Tempo der Förderbänder | Mittel | Geschwindigkeit der Förderbänder auf Karten, die welche haben: langsam, mittel oder schnell. |
| Netzwerk-Port | 5521 | TCP-Port des Servers, wenn du ein Spiel hostest. Spieler, die über das Internet beitreten, brauchen diesen Port (siehe [MANUAL_DE.md](MANUAL_DE.md)). |
| Lautstärke (%) | 100 % | Die Lautstärke. Im Spiel lässt sie sich auch mit `+` und `-` in 5-%-Schritten ändern. |
| Soundeffekte abspielen | an | Explosionen, Items usw. Die Musik wird getrennt geschaltet, mit „Musik abspielen“ im Launcher oder mit `m` im Spiel. |

Karte und Siege kann der Host auch im Spiel vor dem Start einer Runde ändern.

### Der Hurry

80 s vor dem Ende der Runde beginnt der Hurry: Alle 0,5 s fällt ein fester Block auf das Feld, vom Rand spiralförmig zur Mitte. Ein Spieler unter einem fallenden Block stirbt, eine Bombe darunter explodiert vorher. Mit den [Mods](MODS_MANUAL_DE.md#allgemeine-regeln) beendet der Hurry außerdem jede Unverwundbarkeit, alle Geister verschwinden und jeder Treffer ist tödlich. In Runden mit unendlicher Zeit gibt es keinen Hurry.

## Teil 2: Spielmodi

Der Spielmodus ist wie die Mods eine Einstellung des Hosts. Er steht in `atomic_mods.ini` (Abschnitte `[GameMode]` und `[Hunter]`) und geht zusammen mit den Mods an den Server, wenn der Host beitritt. Die anderen Spieler müssen nichts ändern.

Im Launcher auf **Spielmodus...** klicken: Die Liste links zeigt die Modi, der markierte ist der aktive Modus. Es ist immer genau ein Modus aktiv. Rechts stehen die Einstellungen des Modus. **OK** speichert, **Standard** setzt den Spielmodus auf Klassisch und die Hunter-Einstellungen auf ihre Standardwerte zurück (die Mods bleiben, wie sie sind). Das Hauptfenster zeigt den aktiven Modus neben dem Knopf.

### Klassisch

Das normale Spiel: Wer als Letzter lebt, gewinnt die Runde, oder die Runde ist unentschieden, wenn die Zeit um ist. [Teamspiel](#teil-1-spieloptionen) wird in den Spieloptionen eingeschaltet und nur in diesem Modus gespielt.

### Hunter

Ein Spieler ist der **Gejagte**. Er bekommt (fast) alle Items, alle anderen Spieler jagen ihn. Wer den Gejagten tötet, wird der neue Gejagte und bekommt die Items, der bisherige Gejagte fällt auf normal zurück.

**Ablauf einer Runde**

1. Zu Rundenbeginn wird ein Spieler der Gejagte (zufällig oder der Führende, siehe Einstellungen).
2. Der Gejagte bekommt die Startwerte des Schemas plus die **Items des Gejagten**. Er blinkt und ist kurz unverwundbar.
3. Tötet ein Jäger den Gejagten (mit einer Bombe oder einem Flammenwerfer), wird dieser Jäger der neue Gejagte.
4. Stirbt der Gejagte **ohne Killer** (eigene Bombe, Hurry, Geisterbombe), entscheidet die Einstellung „Gejagter stirbt ohne Killer“, wer weitermacht.
5. Mit **Respawn** kommen tote Spieler nach einer Wartezeit auf einem zufälligen freien Feld weit weg vom Gejagten zurück und sind kurz unverwundbar. Die Runde endet, wenn die Spielzeit abgelaufen ist.
6. Den Rundensieger bestimmt die Einstellung „Rundensieger“. Der Sieger bekommt einen Sieg für das Match. Bei Gleichstand ist die Runde unentschieden.

**Einstellungen**

| Einstellung (Dialog) | ini-Schlüssel (`[Hunter]`) | Default | Bedeutung |
|---|---|---|---|
| Spielmodus | `Mode` (`[GameMode]`) | `Classic` | `Classic` oder `Hunter` |
| Items des Gejagten | `HuntedItems` | `bomb*4,flame*4,speed*2,kick,punch,grab,jelly` | Items, die der Gejagte zusätzlich zum Schema bekommt, im selben Format wie die Start-Items des [Mod-Test-Modus](MODS_MANUAL_DE.md#mod-test-modus): mit Komma getrennt, `*n` = n-mal. Möglich: bomb, flame, goldflame, speed, kick, punch, grab, spooger, jelly, trigger. Grab und Spooger schließen sich aus. |
| Gejagter bei Rundenbeginn | `HuntedStart` | 0 | 0 = ein zufälliger Spieler, 1 = der Führende (meiste gewonnene Runden im Match, bei Gleichstand zufällig) |
| Gejagter stirbt ohne Killer | `HuntedLost` | 0 | 0 = ein zufälliger anderer lebender Spieler wird der Gejagte, 1 = er bleibt der Gejagte (nur mit Respawn, ohne Respawn wie 0) |
| Rundensieger | `HunterScoring` | 0 | 0 = längste Zeit als Gejagter, 1 = am häufigsten den Gejagten getötet, 2 = wer am Rundenende der Gejagte ist |
| Der bisherige Gejagte hat | `HuntedLoseItems` | 0 | 0 = seine Items von bevor er der Gejagte wurde, 1 = keine Items (nur die Startwerte des Schemas) |
| Unverwundbar als neuer Gejagter (ms) | `HuntedInvulnMs` | 1500 | 0 .. 60000, 0 = gar nicht |
| Respawn | `HunterRespawn` | 1 (an) | Tote Spieler kommen zurück, die Runde endet nur durch Zeitablauf, es gibt keinen Hurry |
| Respawn nach (ms) | `HunterRespawnMs` | 3000 | 0 .. 60000 |
| Unverwundbar nach Respawn (ms) | `HunterRespawnInvulnMs` | 2000 | 0 .. 60000 |
| Der Gejagte wird nicht krank | `HuntedNoDiseases` | 1 (an) | Krankheiten, Slow und Tauschen wirken nicht auf den Gejagten, er wird nicht angesteckt |
| Goldene Farbe | `HuntedGold` | 1 (an) | Der Gejagte wird in Gold gezeichnet |
| Aura | `HuntedAura` | 1 (an) | Eine pulsierende Aura um den Gejagten |
| Gejagter sieht seine Aura | `HuntedAuraSelf` | 0 (aus) | Aus: Nur die Jäger sehen die Aura. Zwei Spieler an einem PC sehen sie immer, denn einer von beiden könnte ein Jäger sein. |

**Rundensieger (Wertung)**

- **Längste Zeit als Gejagter:** Die Zeit zählt, solange der Gejagte lebt. Gut für lange Runden mit Respawn: Jeder will die Items holen und behalten.
- **Am häufigsten den Gejagten getötet:** Zählt, wie oft ein Spieler den Gejagten erwischt hat. Belohnt das Jagen, nicht das Verstecken.
- **Gejagter am Rundenende:** Nur der letzte Moment zählt. Spannend am Schluss, die Zeit davor ist das Aufwärmen.

**Respawn und Hurry**

Mit Respawn scheidet niemand aus, die Runde kann also nur durch Zeitablauf enden. Deshalb gibt es mit Respawn **keinen Hurry**: Er würde nur Spieler zerquetschen, die ohnehin wiederkommen. Bei der Spielzeit „Unendlich“ dauert die Runde 120 s, sonst würde sie nie enden.

Ohne Respawn gelten die normalen Regeln: Die Runde endet, wenn nur noch ein Spieler lebt (oder die Zeit um ist), der Hurry kommt wie gewohnt und die [Geister-Mod](MODS_MANUAL_DE.md#geister) wirkt.

**Ein neuer Gejagter, der bisherige Gejagte**

- Ein neuer Gejagter blinkt und ist „Unverwundbar als neuer Gejagter“ lang unverwundbar, damit ihn nicht die Bombe erwischt, durch die er der Gejagte wurde.
- Bomben, die er schon gelegt hat, werden berücksichtigt: Mit den neuen Items kann er mehr Bomben gleichzeitig legen, die Bomben auf dem Feld werden davon abgezogen.
- Der bisherige Gejagte (falls er noch lebt) bekommt seine Items von vorher oder nur die Werte des Schemas, siehe „Der bisherige Gejagte hat“.
- Die Items des Gejagten werden bei seinem Tod nicht auf dem Feld verteilt, nur wirklich aufgesammelte Items werden wie gewohnt verteilt.

**Darstellung**

- Der Gejagte wird in **Gold** gezeichnet und hat eine pulsierende **Aura** (beides abschaltbar). Blinkt der Gejagte (unverwundbar oder gestunnt), wird statt Gold das Blinken gezeigt.
- Die Kopfzeile zeigt `H:` mit den Sekunden, die ein Spieler in dieser Runde der Gejagte war, neben dem Score (`S:`) und den Kills (`K:`).

**Teamspiel und Mods**

- **Teamspiel:** Im Teamspiel wird der Hunter-Modus ignoriert, es wird ein normales Teamspiel gespielt. Der Dialog Spielmodus zeigt einen Hinweis, wenn Teamspiel an ist.
- **Geister:** nur ohne Respawn. Mit Respawn kommen Tote ohnehin zurück, der Dialog zeigt einen Hinweis, wenn beides an ist.
- **Trefferpunkte:** gelten wie gewohnt. Der Gejagte wechselt erst, wenn der Gejagte wirklich stirbt, nicht schon, wenn er einen Trefferpunkt verliert.
- Alle anderen Mods wirken wie gewohnt. Es gelten die [allgemeinen Regeln](MODS_MANUAL_DE.md#allgemeine-regeln) der Mods (Unverwundbarkeit, Hurry).

**KI**

- Ein KI-Spieler kann Jäger und Gejagter sein. Die mitgelieferte KI jagt den Gejagten: Sie läuft zu ihm und legt Bomben in seine Linie. Als Gejagte greift sie alle an.
- Nicht jede KI kennt die Spielmodi. Unterstützt die geladene KI den Hunter-Modus nicht (eine ältere oder fremde `ai.dll` / `libai.so`) oder ist gar keine KI geladen, lassen sich KI-Plätze im Spieler-Setup nicht wählen, das Spiel zeigt stattdessen eine Meldung. Vorhandene KI-Plätze werden beim Wechsel zum Karten-Setup gemeldet.
- Aus demselben Grund funktioniert „Getrennte Netzspieler werden zur KI“ nur, wenn die KI den Modus kann. Sonst scheidet der verlorene Spieler aus.

## FAQ

**Warum kann ich keine KI wählen?**
Die geladene KI unterstützt den aktiven Spielmodus nicht, oder es ist gar keine KI geladen. Auf Klassisch umschalten oder die KI verwenden, die zu dieser Version des Spiels gehört (`ai.dll` / `libai.so` neben dem Server).

**Warum gibt es keinen Hurry?**
Im Hunter-Modus mit Respawn gibt es keinen Hurry: Tote kommen ohnehin zurück, die Runde endet, wenn die Zeit um ist. Respawn ausschalten, dann kommt der Hurry wieder. Im Modus Klassisch gibt es bei der Spielzeit „Unendlich“ keinen Hurry.

**Welche Items soll der Gejagte bekommen?**
Der Standard (`bomb*4,flame*4,speed*2,kick,punch,grab,jelly`) macht den Gejagten stark, aber nicht unantastbar. Mit `goldflame` oder `trigger` wird er deutlich gefährlicher, mit weniger Items wird die Jagd kürzer. Auf kleinen Feldern sind weniger Bomben besser. Für eine faire Jagd mit Anfängern `bomb*2,flame*2,speed` probieren.

**Der Gejagte stirbt durch seine eigene Bombe, wer ist der Nächste?**
Das regelt „Gejagter stirbt ohne Killer“: ein zufälliger anderer Spieler, oder mit Respawn kann er der Gejagte bleiben.

**Ich habe Teamspiel eingeschaltet, aber der Hunter-Modus wird nicht gespielt.**
Das ist so gewollt: Teamspiel und Hunter-Modus passen nicht zusammen, im Teamspiel wird ein normales Teamspiel gespielt.
