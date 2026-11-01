# Steuerung: Tastatur, Gamecontroller und Joystick

[English version](CONTROLS_MANUAL.md)

Diese Anleitung erklärt, wie man die Steuerung für die beiden Spieler an einem PC einrichtet (Launcher: **Steuerung...**, im Spiel: Optionen-Menü, **Define keyboard layouts**). Welche Taste im Spiel was macht, steht in der [Anleitung](MANUAL_DE.md#tasten-im-spiel).

- [Der Dialog Steuerung konfigurieren](#der-dialog-steuerung-konfigurieren)
- [Gamecontroller oder Joystick?](#gamecontroller-oder-joystick)
- [Einen Controller einrichten](#einen-controller-einrichten)
- [Die Optionen](#die-optionen)
- [Controller während des Spiels ein- und ausstecken](#controller-während-des-spiels-ein--und-ausstecken)
- [FAQ](#faq)

## Der Dialog Steuerung konfigurieren

Der Dialog hat eine Spalte für **Spieler 1** und eine für **Spieler 2**. Für jeden Spieler wählt man oben, womit er spielt:

- **Tastatur**: Darunter stehen die sechs Tasten (Hoch, Runter, Links, Rechts, erste und zweite Aktion). Mit **Ändern** drückt man die neue Taste. Ist eine Taste doppelt belegt (auch zwischen den beiden Spielern), wird sie rot angezeigt.
- **Controller**: Ist schon ein Controller eingerichtet, steht hier sein Name. Ist noch keiner eingerichtet, startet beim Anklicken gleich der [Assistent](#einen-controller-einrichten). Bricht man ihn ab, bleibt es bei der Tastatur.

Bei einem Controller zeigt der Dialog:

| Anzeige | Bedeutung |
|---|---|
| Status: verbunden (grün) | Der Controller ist angeschlossen und wird im Spiel benutzt. |
| Status: nicht verbunden (rot) | Der Controller ist gerade nicht angeschlossen. Sobald er eingesteckt wird, geht er wieder, auch während des Spiels. |
| Status: bitte neu einrichten (rot) | Der Gamecontroller wurde mit einer älteren Version eingerichtet, deren Tastenzuordnung falsch sein kann. Einfach **Controller einrichten...** klicken. |
| Derselbe Controller wie Spieler X (rot) | Beide Spieler benutzen denselben Controller. Das ist erlaubt, ein Controller bewegt dann aber beide Spieler gleichzeitig. |
| Erste / Zweite Aktion, Richtungen | Welche Tasten und Achsen eingerichtet sind. |

Tastenkürzel im Dialog:

| Kürzel | Wirkung |
|---|---|
| Alt+1 / Alt+2 | springt zu Spieler 1 bzw. Spieler 2 |
| Alt + unterstrichener Buchstabe | löst den Button / die Option aus, die Buchstaben werden je nach Sprache und Controllername automatisch vergeben |
| Pfeiltasten | wechseln zwischen Tastatur und Controller |
| Esc | Abbrechen |

**Standard** setzt beide Spieler auf die Standard-Tastatur zurück.

## Gamecontroller oder Joystick?

SDL2 kennt die meisten Controller (Xbox, PlayStation, Switch und viele andere) als **Gamecontroller**. Dann weiß SDL, welche Taste A, B, LB, ... ist, wo das Steuerkreuz und der linke Stick sind, und wie die Ruhestellung aussieht.

| | Gamecontroller (empfohlen) | Joystick |
|---|---|---|
| Für | alle Controller, die SDL kennt | ältere oder exotische Geräte, die SDL nicht als Gamecontroller kennt |
| Einrichten | 2 Schritte: erste und zweite Aktion | 5 Schritte: Ruhestellung, beide Aktionen, Hoch, Links |
| Richtungen | Steuerkreuz, optional der linke Analogstick | die eingerichteten Achsen, optional der Hat (Steuerkreuz) |
| Tastennamen | A, B, X, Y, LB, RB, ... | Taste 1, Taste 2, ... |

Wenn das Gerät als Gamecontroller angeboten wird, sollte man das nehmen. Joystick ist für Geräte gedacht, bei denen das nicht geht, oder wenn man die Achsen selbst wählen will.

## Einen Controller einrichten

Der Assistent startet mit **Controller einrichten...** oder beim ersten Wechsel auf Controller. Er führt Schritt für Schritt: Oben steht, was zu tun ist, darunter eine Checkliste mit allen Schritten und den Ergebnissen (✓ erledigt, ▶ aktuell, ○ offen). Die aktuelle Zeile blinkt, solange der Assistent auf den Controller wartet.

1. **Gerät wählen**: Den Controller auswählen und festlegen, ob er als Gamecontroller oder als Joystick benutzt wird. Gleiche Controller werden durchnummeriert (#2, #3). Wurde gerade ein Controller eingesteckt, hilft **Geräte aktualisieren**.
2. **Ruhestellung** (nur Joystick): Alle Sticks und Tasten loslassen, dann **Weiter**.
3. **Erste Aktion**: Die Taste drücken, mit der man Bomben legt. Der Assistent geht dann von selbst weiter.
4. **Zweite Aktion**: Eine andere Taste drücken.
5. **Hoch** (nur Joystick): Den Stick nach oben drücken und eine halbe Sekunde halten. Nimmt man hier das Steuerkreuz (Hat), wird nur mit dem Steuerkreuz gesteuert und der nächste Schritt entfällt.
6. **Links** (nur Joystick): Den Stick nach links drücken und kurz halten.
7. **Fertig**: Mit **Fertig** oder der eben eingerichteten ersten Aktion übernehmen.

**< Zurück** geht immer genau einen Schritt zurück, nichts anderes geht dabei verloren. Im ersten Schritt kommt man so zurück zur Geräteauswahl. **Abbrechen** (oder Esc) lässt die bisherige Einstellung unverändert.

Die **Live-Anzeige** unten ist nur zum Testen: Sie zeigt die Achsen, die gedrückten Tasten und die Richtung, die das Spiel erkennen würde.

## Die Optionen

Die Optionen gelten pro Spieler und lassen sich im Dialog jederzeit ändern, ohne den Controller neu einzurichten. Was für den Gerätetyp nicht greift, ist ausgegraut.

| Option | Default | Gilt für | Bedeutung |
|---|---|---|---|
| Analogstick verwenden | an | Gamecontroller | Zusätzlich zum Steuerkreuz steuert der linke Analogstick. |
| Hat (Steuerkreuz) verwenden | an | Joystick | Zusätzlich zu den Achsen steuert der erste Hat (das Steuerkreuz vieler Joysticks). |
| Hysterese (kein Flackern) | an | Stick und Achsen | siehe unten |
| Diagonale: stärkere Achse gewinnt | an | Stick und Achsen | siehe unten |

### Hysterese

Ein Stick zählt in einer Richtung als gedrückt, wenn er mindestens zu 25 % ausgelenkt ist. Hält man ihn genau an dieser Grenze, zittert der Wert darüber und darunter: Die Figur bekommt dann sehr schnell abwechselnd "gedrückt" und "losgelassen", sie stockt, und jedes Mal geht eine Nachricht an den Server.

Mit Hysterese wird eine Richtung bei 25 % gedrückt, aber erst wieder losgelassen, wenn der Stick unter etwa 14 % zurück ist. Dazwischen bleibt der letzte Zustand, die Figur läuft gleichmäßig. Für das Steuerkreuz spielt die Option keine Rolle. Ausschalten muss man sie eigentlich nur, wenn man möchte, dass die Figur schon beim kleinsten Zurücknehmen des Sticks stehen bleibt.

### Diagonale: stärkere Achse gewinnt

Die Figur kann immer nur in eine Richtung laufen. Hält man den Stick schräg, sind aber zwei Richtungen "gedrückt", z. B. Runter und Rechts.

- **Aus** (das alte Verhalten): Es gewinnt die Richtung, die zuletzt über die 25 %-Grenze gegangen ist. Wer beim Abbiegen den Stick schon etwas früh zur Seite drückt, biegt damit sofort ab, die Figur bleibt vor der Kreuzung an der Wand stehen.
- **An**: Es zählt nur die Achse, die weiter ausgelenkt ist. Man läuft also weiter, bis der Stick mehr zur Seite als nach unten zeigt, und biegt erst dann ab. Damit die Richtung bei genau 45° nicht hin und her springt, behält die bisherige Achse die Richtung, bis die andere 25 % stärker ist.

Für das Steuerkreuz und den Hat gilt die Option nicht, dort gibt es keine "stärkere" Richtung. Ausgegraut ist sie, wenn kein Stick bzw. keine Achse benutzt wird.

## Controller während des Spiels ein- und ausstecken

Wird ein Controller abgezogen (oder geht ein Funk-Controller in den Standby), lässt die Figur sofort alle Tasten los und bleibt stehen. Wird er wieder eingesteckt, geht er ohne Neustart weiter. Erkannt wird der Controller dabei über seinen Namen und bei gleichen Controllern über deren Reihenfolge.

## FAQ

**Der Status sagt "bitte neu einrichten", obwohl alles ging.**
Ältere Versionen haben bei Gamecontrollern die rohen Tastennummern gespeichert. Bei Xbox-Controllern passten A, B, X und Y zufällig, die Schultertasten aber nicht (LB wurde z. B. zu BACK). Einmal **Controller einrichten...**, dann stimmt es.

**Mein Controller taucht nicht auf.**
Controller einstecken und im Assistenten **Geräte aktualisieren** klicken. Fehlt die SDL2-Bibliothek, zeigt der Dialog "SDL2 nicht verfügbar", dann muss `SDL2.dll` (Windows) bzw. `libSDL2` (Linux) installiert werden.

**Zwei gleiche Controller: welcher ist Spieler 1?**
Die Reihenfolge, in der das Betriebssystem sie meldet. Im Assistenten sind sie durchnummeriert (#1, #2), im Dialog steht die Nummer hinter dem Namen.

**Das Steuerkreuz wird bei "Erste Aktion" nicht angenommen.**
Bei einem Gamecontroller kann das Steuerkreuz nicht als Aktionstaste verwendet werden, es ist nur für die Richtungen da.

**Wo wird das gespeichert?**
In `fpc_atomic.cfg` im Konfigordner (Launcher: **Konfigordner**) (Schlüssel `UseSDL`, `SDL_...`, für Spieler 2 mit einer `2` am Ende).
