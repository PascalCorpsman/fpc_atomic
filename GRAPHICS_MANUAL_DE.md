# Grafiksätze und Grafik-Generator

[English version](GRAPHICS_MANUAL.md)

FPC Atomic braucht Grafiken und Töne. Bisher kamen sie nur von der Atomic Bomberman CD (`data/`, erzeugt vom CD-Extraktor). Jetzt kann FPC Atomic auch **eigene Grafiken und Töne berechnen**: Figur, Bomben, Flammen, Powerups, alle 11 Karten, die Menübilder, die Schemes und alle Spielgeräusche. Ein solcher **Grafiksatz** entsteht in wenigen Sekunden. Mit der CD bleibt alles wie bisher, die Originalgrafiken sind immer als „Original“ wählbar.

- [Ohne CD spielen](#ohne-cd-spielen)
- [Grafiksatz wählen](#grafiksatz-wählen)
- [Töne](#töne)
- [Der Grafik-Generator](#der-grafik-generator)
- [Die Stile](#die-stile)
- [Wo die Sätze liegen](#wo-die-sätze-liegen)
- [Kommandozeile](#kommandozeile)

## Ohne CD spielen

Beim allerersten Start ohne CD-Daten und ohne Grafiksatz fragt der Launcher einmal: **Willkommen bei FPC Atomic – woher sollen die Grafiken und Sounds des Spiels kommen?**

- **Eigene erzeugen (empfohlen)**: Ein Fenster zeigt **vier Stile** nebeneinander, jeweils mit derselben Figur (**Neu würfeln** zeigt vier neue Varianten). Ein Klick auf einen Stil erzeugt diesen Satz in etwa 15 Sekunden, benannt nach dem Stil. Die Ordner unter `assets/` heißen immer englisch (`Flat`, `Soft`, `Pixel`, `Retro`), Launcher, Spieloptionen und Grafik-Generator zeigen den Namen des Stils in der aktuellen Sprache des Launchers (auf Deutsch Flach, Weich, Pixel, Retro) und wechseln ihn mit der Sprache. Der Launcher wählt ihn aus (Sounds „Automatisch“), du kannst sofort spielen. Die anderen drei Stile entstehen **im Hintergrund** mit demselben Seed (gleiche Figur und Karten, anderes Aussehen); der Status im Launcher zeigt, wie viele noch laufen, sie erscheinen unter dem Bild, sobald sie fertig sind. Schließt du den Launcher vorher, wartet er unsichtbar, bis sie fertig sind. **Abbrechen** im Stil-Fenster erzeugt nichts und zeigt die Frage wieder.
- **Von der CD übernehmen**: öffnet den CD-Extraktor (dafür braucht es die Atomic Bomberman CD).
- **Später**: Es passiert nichts. Das Bild im Launcher zeigt dann „Noch keine Grafiken“, ein Klick darauf öffnet die Frage wieder.

Von selbst kommt die Frage nur einmal. **Einrichtung...** im Launcher (Gruppe Installation, links neben **CD-Extraktor...**; fett, solange Spieldaten fehlen) oder `atomic_launcher --setup` öffnen sie jederzeit, z.B. um die vier Stile mit einem neuen Seed noch einmal zu erzeugen. Sätze, die eine frühere Einrichtung erzeugt hat, werden dabei **ersetzt** (Flach, Weich, Pixel, Retro bekommen das neue Aussehen). Sätze, die im Grafik-Generator gespeichert wurden oder anders entstanden sind, werden nie überschrieben: Der neue Stil heißt dann z.B. „Flach 2“ (Ordner `Flat2`). Alles andere bleibt möglich: Der CD-Extraktor ist immer da, nach dem Auslesen der CD ist „Original“ einfach eine weitere Wahl, und die erzeugten Sätze bleiben.

Fehlen die CD-Daten, erzeugt ein Klick auf **Starten** zuerst einen Grafiksatz (ein Stil nach Wahl, danach startet der Launcher das Spiel).

**Die CD-Daten sind schon da?** Dann kommt die Frage nicht von selbst. Stattdessen zeigt der Launcher einmal einen kleinen Hinweis über dem Bild: **Neu: eigene Grafiken...**. Ein Klick darauf öffnet dieselbe Stil-Auswahl wie die Einrichtung, erzeugt die vier Stile und lässt „Original“ gewählt; eine kurze Meldung sagt dann, dass man mit ◀ ▶ unter dem Bild umschaltet. Das **×** daneben blendet den Hinweis für immer aus. Er erscheint nicht, wenn es schon Grafiksätze gibt oder die Einrichtung schon lief.

Wird das Spiel (`fpc_atomic`) direkt gestartet, ohne Launcher, nimmt es den Grafiksatz und die Sounds, die im Launcher gewählt sind (dieselbe Einstellungsdatei). Gibt es weder CD-Daten noch einen brauchbaren Satz, meldet es „Es sind noch keine Grafiken eingerichtet“ und bietet **Einrichtung öffnen** (startet den Launcher mit der Frage) oder **Beenden** an.

Spielgeräusche und Menü-Musik bringt der Satz mit (siehe [Töne](#töne)). Die Schemes (Aufstellungen der Steine) bringt der Grafiksatz mit, sie werden beim ersten Mal nach `data/schemes` kopiert. Sind dort schon Schemes von der CD, bleiben sie unverändert.

## Grafiksatz wählen

Der aktuelle Satz steht im Launcher **unter dem Bild links**: `◀  Name  ▶`. Die Pfeile schalten zwischen „Original“ und allen erzeugten Sätzen um, das Bild zeigt eine Vorschau des Satzes. Dieselbe Auswahl gibt es im Launcher unter **Spieloptionen...** als **Grafiksatz**.

- Fehlt einem Satz eine Datei, nimmt das Spiel sie aus dem Original (falls vorhanden).
- **Netzwerkspiel:** Jeder Spieler wählt seinen Satz selbst, auch unterschiedliche Sätze spielen zusammen. Die Karten sind trotzdem dieselben, nur ihr Aussehen unterscheidet sich.

## Töne

Jeder Satz enthält eigene Spielgeräusche: Explosionen, Kicks, Abpraller, Teleporter und kurze Sprüche wie „Erwischt!“ oder „Beeil dich!“. Sie sind keine Aufnahmen, sondern werden wie die Grafiken berechnet (eine Interpretation der Original-Geräusche, beschrieben in `asset_generator/sounds.ini`). Dazu kommt ein eigenes Musikstück für die Menüs (`asset_generator/music.ini`).

Die Sprüche gibt es auf Englisch, Deutsch, Französisch und Spanisch (in jeder Sprache eigene kurze Sprüche, z.B. „Je t'ai eu !“, „¡Te pillé!“), jeder Satz enthält alle. Das Spiel und **Probehören** im Launcher spielen die Sprüche automatisch in der Sprache des Launchers, es genügt, die Sprache im Launcher umzustellen. Ein Satz einer älteren Version ohne französische oder spanische Sprüche spielt die englischen. Die Sprache im Bereich **Töne** des Grafik-Generators wählt nur, was die Vorschau spielt, und die Vorgabe für andere Programme, die nur eine Sprache kennen (die einfachen Dateien in `sounds/`, die Sprachen liegen in `sounds/en/`, `sounds/de/`, `sounds/fr/`, `sounds/es/`).

Der Klang passt zum Stil: Flach klingt klar und knackig, Weich warm mit etwas Hall, Pixel nach 16-Bit-Konsole mit Echo, Retro rau nach 8-Bit. Der Seed verändert Tonhöhe, Länge und Klangfarbe, zwei Sätze im selben Stil klingen also unterschiedlich.

Im Launcher stehen die Töne unter dem Bild links in einer zweiten Zeile: `◀  Sounds: Name  ▶`. **Probehören** spielt eine Explosion, einen Spruch und den Anfang der Menü-Musik der gewählten Quelle, auch wenn Musik oder Sounds ausgeschaltet sind.

Welche Töne das Spiel spielt, stellt man dort oder unter **Spieloptionen...** bei **Sounds** ein:

- **Automatisch** (Standard): die Töne des gewählten Grafiksatzes. Bei „Original“ (oder einem Satz ohne Töne) die Töne von der CD. Wer den Grafiksatz wechselt, hört also auch dessen Töne. Passt es neben die Pfeile, steht die tatsächliche Quelle dabei, z.B. `Sounds: Auto (Name)`, sonst zeigt sie der Tooltip.
- **Original (CD)**: immer die Töne von der CD, auch zu einem erzeugten Grafiksatz.
- **ein Satz**: die Töne dieses Satzes, unabhängig vom Grafiksatz und auch wenn die CD-Töne da sind.

Fehlt der gewählten Quelle ein Ton, kommt er von der CD. Gibt es weder CD-Töne noch Töne in der gewählten Quelle, bleibt das Spiel stumm, der Launcher zeigt dann „(ohne Sounds)“.

Ein- und ausschalten lassen sich die Töne im Launcher unter **Spieleinstellungen**: **Musik abspielen** (Menü und Spiel) und **Sounds abspielen** (die Spielgeräusche, dieselbe Einstellung wie **Soundeffekte abspielen** in den Spieloptionen).

## Der Grafik-Generator

**Grafiken...** im Launcher (oder ein Doppelklick auf das Bild links) öffnet den Grafik-Generator mit dem gewählten Satz. Bei „Original“ beginnt er einen neuen Satz.

- **Oben:** Name des Satzes, **Stil**, **Seed** mit **Würfeln** und **Eigene Figur pro Spieler**. Der Seed ist die Zahl, aus der alles berechnet wird: gleicher Seed, gleicher Stil und gleiche Einstellungen ergeben immer genau dieselben Grafiken.
- **Links:** die Bereiche, also Figur (mit dem Gejagten und, falls eingeschaltet, Spieler 1 bis 10), Bomben, Flammen, Powerups, Spielfeld-Objekte, die 11 Karten, Menübilder und das Bild für den Launcher.
- **Mitte:** die Vorschau des Bereichs. Die Figur läuft in der gewählten **Spielerfarbe**, Animation und Richtung sind wählbar (Laufen, mit Bombe, Treten, Tod, Idle, ...). Bei den Karten zeigt eine Szene Steine, die Figur, eine tickende Bombe und eine Explosion.
- **Töne** und **Musik** sind eigene Bereiche: Schieberegler für Tonhöhe, Länge, Helligkeit und Rauschen bzw. Tonart, Tempo und Klangfarbe, dazu eine Liste aller Töne zum Anhören (▶) und **Musik abspielen**.
- **Rechts:** die Einstellungen des Bereichs als Schieberegler (Größe, Proportionen, Farbton, ...), jeweils mit **Zurück**. **Neuer Seed** würfelt nur diesen Bereich neu, **Seed sperren** hält ihn fest, wenn oben neu gewürfelt wird.
- **Speichern** schreibt den Satz, **Als Satz speichern...** legt einen neuen an.

Beim Speichern wird der Satz komplett neu erzeugt. Erst wenn alles fertig ist, ersetzt er den alten Satz. Bricht man ab, bleibt der alte Satz unverändert. Ist eine Datei des Satzes gerade von einem anderen Programm geöffnet, kommt eine Meldung, und der alte Satz bleibt ebenfalls erhalten. Ein laufendes Spiel sieht die neuen Grafiken erst beim nächsten Start. Eigene Dateien sollte man nicht in einen Satz-Ordner legen, sie gehen beim nächsten Speichern verloren.

## Die Stile

| Stil | Aussehen |
|---|---|
| Flach mit Kontur | Cartoon-Look: kräftige Farben, dicke dunkle Kontur, ein harter Schatten |
| Weich schattiert | gemalt wie Knete: weiche Verläufe, Glanzlichter, keine schwarze Kontur |
| Pixel-Art | 16-Bit-Konsole: grobe Pixel, Schattierung in Stufen mit Dithering |
| Retro (DOS-Look) | sehr grobe Pixel, wenige satte Farben, große Köpfe |

Die Figur trägt in jedem Stil die Spielerfarbe, ebenso Bomben und Flammen.

## Wo die Sätze liegen

Jeder Satz ist ein Ordner unter `assets/` neben dem Spiel, z. B. `assets/Generated/`. Darin liegen dieselben Unterordner wie in `data/` (`atomic/`, `maps/`, `res/`, `schemes/`), dazu `profile.ini` (Name, Seed, Stil und alle geänderten Einstellungen) und `preview.png` (das Bild im Launcher). Einen Satz löscht man, indem man seinen Ordner löscht.

In der `profile.ini` steht auch, wer den Satz erzeugt hat: `Origin=setup` bei den Sätzen der Einrichtung im Launcher (eine neue Einrichtung darf sie ersetzen), `Origin=editor`, sobald der Satz im Grafik-Generator gespeichert wurde (die Einrichtung ersetzt ihn nie). Die Kommandozeilen-Version behält den Wert, den sie vorfindet.

Wer einen Satz weitergeben will, gibt einfach die `profile.ini` weiter: Daraus erzeugt der Generator denselben Satz wieder.

## Kommandozeile

Grafik-Generator mit Oberfläche (`asset_generator_gui`):

- `--new`: neuer Satz mit zufälligem Seed
- `--set <Ordner>`: Satz `assets/<Ordner>` öffnen
- `--auto --set <Ordner>`: nur die Stil-Auswahl und der Fortschritt, wie beim Start ohne CD; mit `--style flat|soft|pixel|retro` ohne Auswahl
- `--auto --setup --folders <flat,soft,pixel,retro> --names <flat,soft,pixel,retro>`: die Stil-Auswahl der Einrichtung, der gewählte Stil wird in seinen eigenen Ordner erzeugt (ein Ordner und Name pro Stil, durch Kommas getrennt). Der Ordner steht danach in `assets/last_set.txt`, die anderen Stile erzeugt der Launcher mit `asset_generator --out assets/<Ordner> --profile assets/<erster Satz>/profile.ini --style <Stil> --name <Name>` (gleicher Seed)

Ohne Oberfläche (`asset_generator`):

```
asset_generator --out assets/MeinSatz --name "Mein Satz" --seed random --style retro
```

- `--out <Ordner>`: Ziel, eine vorhandene `profile.ini` dort wird gelesen
- `--profile <Datei>`, `--seed <Zahl|random>`, `--style <flat|soft|pixel|retro>`, `--name <Text>`
- `--per-player`: eigene Figur pro Spieler
- `--language en|de|fr|es`: Vorgabe-Sprache der Sprüche (der Satz enthält immer alle)
- `--only <a,b,...>`: nur diese Bereiche, `--list` zeigt alle
- `--params`: alle Einstellungen mit ihren Werten ausgeben

Das Spiel selbst nimmt den Satz mit `-assets <Ordner>` und die Töne mit `-sounds <Ordner>` (der Launcher übergibt beides automatisch). Ohne diese Parameter nimmt es die Einstellungen des Launchers (`[Launcher] AssetSet` und `SoundSet`). Der Ordnername `auto` ist für die Tonauswahl reserviert.

Der Launcher öffnet die Einrichtung mit `--setup` (auch `-setup`).
