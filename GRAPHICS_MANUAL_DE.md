# Grafiksätze und Grafik-Generator

[English version](GRAPHICS_MANUAL.md)

FPC Atomic braucht Grafiken. Bisher kamen sie nur von der Atomic Bomberman CD (`data/`, erzeugt vom CD-Extraktor). Jetzt kann FPC Atomic auch **eigene Grafiken berechnen**: Figur, Bomben, Flammen, Powerups, alle 11 Karten, die Menübilder und die Schemes. Ein solcher **Grafiksatz** entsteht in wenigen Sekunden. Mit der CD bleibt alles wie bisher, die Originalgrafiken sind immer als „Original“ wählbar.

- [Ohne CD spielen](#ohne-cd-spielen)
- [Grafiksatz wählen](#grafiksatz-wählen)
- [Der Grafik-Generator](#der-grafik-generator)
- [Die Stile](#die-stile)
- [Wo die Sätze liegen](#wo-die-sätze-liegen)
- [Kommandozeile](#kommandozeile)

## Ohne CD spielen

Fehlen die CD-Daten, zeigt der Launcher den Button **Grafiken erzeugen**. Auch ein Klick auf **Starten** erzeugt zuerst einen Grafiksatz:

1. Ein Fenster zeigt **vier Stile** nebeneinander, jeweils mit derselben Figur. **Neu würfeln** zeigt vier neue Varianten.
2. Ein Klick auf einen Stil erzeugt den ganzen Satz, ein Fortschrittsbalken zeigt den Stand.
3. Danach wählt der Launcher den neuen Satz aus und startet das Spiel.

**Abbrechen** erzeugt nichts, das Spiel startet dann nicht.

Ohne CD gibt es (noch) **keine Sounds**, das Spiel läuft stumm. Die Schemes (Aufstellungen der Steine) bringt der Grafiksatz mit, sie werden beim ersten Mal nach `data/schemes` kopiert. Sind dort schon Schemes von der CD, bleiben sie unverändert.

## Grafiksatz wählen

Der aktuelle Satz steht im Launcher **unter dem Bild links**: `◀  Name  ▶`. Die Pfeile schalten zwischen „Original“ und allen erzeugten Sätzen um, das Bild zeigt eine Vorschau des Satzes. Dieselbe Auswahl gibt es im Launcher unter **Spieloptionen...** als **Grafiksatz**.

- Fehlt einem Satz eine Datei, nimmt das Spiel sie aus dem Original (falls vorhanden).
- **Netzwerkspiel:** Jeder Spieler wählt seinen Satz selbst, auch unterschiedliche Sätze spielen zusammen. Die Karten sind trotzdem dieselben, nur ihr Aussehen unterscheidet sich.

## Der Grafik-Generator

**Grafiken...** im Launcher (oder ein Doppelklick auf das Bild links) öffnet den Grafik-Generator mit dem gewählten Satz. Bei „Original“ beginnt er einen neuen Satz.

- **Oben:** Name des Satzes, **Stil**, **Seed** mit **Würfeln** und **Eigene Figur pro Spieler**. Der Seed ist die Zahl, aus der alles berechnet wird: gleicher Seed, gleicher Stil und gleiche Einstellungen ergeben immer genau dieselben Grafiken.
- **Links:** die Bereiche, also Figur (mit dem Gejagten und, falls eingeschaltet, Spieler 1 bis 10), Bomben, Flammen, Powerups, Spielfeld-Objekte, die 11 Karten, Menübilder und das Bild für den Launcher.
- **Mitte:** die Vorschau des Bereichs. Die Figur läuft in der gewählten **Spielerfarbe**, Animation und Richtung sind wählbar (Laufen, mit Bombe, Treten, Tod, Idle, ...). Bei den Karten zeigt eine Szene Steine, die Figur, eine tickende Bombe und eine Explosion.
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

Wer einen Satz weitergeben will, gibt einfach die `profile.ini` weiter: Daraus erzeugt der Generator denselben Satz wieder.

## Kommandozeile

Grafik-Generator mit Oberfläche (`asset_generator_gui`):

- `--new`: neuer Satz mit zufälligem Seed
- `--set <Ordner>`: Satz `assets/<Ordner>` öffnen
- `--auto --set <Ordner>`: nur die Stil-Auswahl und der Fortschritt, wie beim Start ohne CD; mit `--style flat|soft|pixel|retro` ohne Auswahl

Ohne Oberfläche (`asset_generator`):

```
asset_generator --out assets/MeinSatz --name "Mein Satz" --seed random --style retro
```

- `--out <Ordner>`: Ziel, eine vorhandene `profile.ini` dort wird gelesen
- `--profile <Datei>`, `--seed <Zahl|random>`, `--style <flat|soft|pixel|retro>`, `--name <Text>`
- `--per-player`: eigene Figur pro Spieler
- `--only <a,b,...>`: nur diese Bereiche, `--list` zeigt alle
- `--params`: alle Einstellungen mit ihren Werten ausgeben

Das Spiel selbst nimmt den Satz mit `-assets <Ordner>` (der Launcher übergibt ihn automatisch).
