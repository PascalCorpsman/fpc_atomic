# Spiel-Mods

[English version](MODS_MANUAL.md)

FPC Atomic hat einige optionale Spiel-Mods, die die Regeln des Originalspiels ändern. Alle Mods sind **standardmäßig aus**. Ohne Änderungen spielt sich das Spiel also genau wie das Original.

Welche Mods zu welcher Spielerrunde passen, beantwortet die [Mods-FAQ](MODS_FAQ_DE.md).

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
- [Items & Krankheiten](#items--krankheiten)
- [Mod-Test-Modus](#mod-test-modus)
- [atomic_mods.ini von Hand bearbeiten](#atomic_modsini-von-hand-bearbeiten)

## Mods einrichten

Die Mods sind eine Einstellung des **Hosts**, also des ersten Spielers, der dem Spiel beitritt. Beim Beitreten schickt das Spiel die Mods an den Server, auch an einen [dedizierten Server](MANUAL_DE.md#server-befehle) auf einem anderen PC. Alle anderen Spieler bekommen die Regeln vom Server und müssen nichts ändern. Ihre eigenen Mod-Einstellungen werden nicht genutzt.

Am einfachsten geht es im Launcher: **Mods...** anklicken, links die gewünschten Mods anhaken und rechts ihre Werte einstellen. **OK** speichert. Die Einstellungen gelten für das **nächste Spiel, dem dieser PC als erster Spieler beitritt**.

Gespeichert werden die Einstellungen in `atomic_mods.ini` im Konfigurationsordner (Launcher: **Konfigordner**), siehe [atomic_mods.ini von Hand bearbeiten](#atomic_modsini-von-hand-bearbeiten).

Haben alle Spieler den Server verlassen (oder ist das Match vorbei), schaltet der Server alle Mods wieder aus. Der nächste Host bringt seine eigenen Mods mit.

Die Namen der Einstellungen stehen in den Tabellen so, wie sie im Launcher auf Deutsch heißen (umschalten mit dem Link unten rechts im Launcher).

### Allgemeine Regeln

- **Unverwundbare Spieler** blinken weiß (rot, wenn ihre eigene Farbe weiß ist). Flammen schaden ihnen nicht, und sie können nicht gestunnt werden.
- **Gestunnte Spieler** blinken schwarz (rot, wenn ihre eigene Farbe schwarz ist).
- **Der Hurry beendet jeden Schutz:** Wenn der Hurry beginnt (80 s vor Rundenende), endet jede Unverwundbarkeit, alle Geister verschwinden und jeder Treffer ist tödlich. In Runden mit unbegrenzter Zeit gibt es keinen Hurry.
- Überschneiden sich mehrere Schutzzeiten, zählt die längste. Sie werden nicht addiert.

## Stun bei Bombenlandung

Eine **geworfene** Bombe (Handschuh, siehe [erste Aktion](MANUAL_DE.md#erste-aktion-doppelt-gedrückt)), die auf einem Spieler landet, stunnt diesen Spieler. Ein gestunnter Spieler kann sich weder bewegen noch eine Aktion ausführen. Geboxte Bomben stunnen nicht.

Der Stun trifft jeden auf dem Feld: Gegner, Teamkollegen und den Werfer selbst. Unverwundbare sind immun.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `StunOnBombLanding` | 0 (aus) | 0 / 1 |
| Min. Stun-Zeit (ms) | `StunMinMs` | 100 | 0 .. 10000 |
| Max. Stun-Zeit (ms) | `StunMaxMs` | 500 | 0 .. 10000, nicht kleiner als Min. |

Die Stun-Zeit ist zufällig zwischen Min. und Max.

## Boxhandschuh

Mit dem Box-Item ![](data/res/powpunch.png) stunnt die [zweite Aktion](MANUAL_DE.md#zweite-aktion) einen **Gegner**, der auf dem Feld direkt vor dem Spieler steht (in Blickrichtung). Das geht auch im Stehen.

- Liegt auf demselben Feld eine Bombe, wird nur der Gegner gestunnt, die Bombe bleibt liegen. Ohne Gegner wird die Bombe wie gewohnt geboxt.
- Teamkollegen, Unverwundbare, fliegende und sterbende Spieler werden nicht gestunnt.
- Nach einem Schlag muss derselbe Spieler kurz warten, bevor er wieder schlagen kann. So kann niemand dauerhaft gestunnt werden.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `PunchStun` | 0 (aus) | 0 / 1 |
| Min. Stun-Zeit (ms) | `PunchStunMinMs` | 300 | 0 .. 10000 |
| Max. Stun-Zeit (ms) | `PunchStunMaxMs` | 800 | 0 .. 10000, nicht kleiner als Min. |
| Pause bis zum nächsten Schlag (ms) | `PunchStunCooldownMs` | 1500 | 0 .. 10000 |

## Positive Krankheit

Ein Teil der Krankheits-Items ![](data/res/powdisea.png) macht den Spieler für eine zufällige Zeit **unverwundbar** statt krank. Der Spieler sieht nicht, wann der Schutz endet.

Im Hurry ist ein Krankheits-Item immer eine normale Krankheit. Die schlimme Krankheit ![](data/res/powebola.png) bleibt unverändert.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `PositiveDisease` | 0 (aus) | 0 / 1 |
| Chance pro Krankheits-Item (%) | `PositiveDiseaseChance` | 15 | 0 .. 100 |
| Min. Unverwundbarkeit (ms) | `PositiveMinMs` | 5000 | 0 .. 120000 |
| Max. Unverwundbarkeit (ms) | `PositiveMaxMs` | 20000 | 0 .. 120000, nicht kleiner als Min. |

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
| Chance pro Krankheits-Item (%) | `BrickWalkChance` | 15 | 0 .. 100 |
| Min. Zeit (ms) | `BrickWalkMinMs` | 5000 | 0 .. 120000 |
| Max. Zeit (ms) | `BrickWalkMaxMs` | 10000 | 0 .. 120000, nicht kleiner als Min. |
| Durchsichtig (%) | `BrickWalkTransparency` | 50 | 0 .. 100, 0 = normal, 100 = unsichtbar |

Die Durchsichtigkeit entsteht, indem der Spieler nur in einem Teil der Bilder gezeichnet wird. Er kann deshalb etwas flackern.

## Weißer Bildschirm

Ein Teil der Krankheits-Items ![](data/res/powdisea.png) macht den Bildschirm des Spielers für eine Weile weiß, statt einer normalen Krankheit. Der Bildschirm bleibt eine einstellbare Zeit weiß und blendet dann zurück.

- Er wird nach den positiven Effekten gewürfelt ([positive Krankheit](#positive-krankheit), [durch Steine laufen](#durch-steine-laufen)).
- KI-Spieler sind nicht betroffen.
- Spielen zwei Spieler an einem PC, zeigt dieser PC keinen weißen Bildschirm, weil sich beide den Bildschirm teilen.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `FlashDisease` | 0 (aus) | 0 / 1 |
| Chance pro Krankheits-Item (%) | `FlashDiseaseChance` | 15 | 0 .. 100 |
| Stärke (%) | `FlashDiseaseIntensity` | 90 | 0 .. 100, 100 = man sieht nichts mehr |
| Dauer (ms) | `FlashDiseaseMs` | 2500 | 0 .. 10000 |
| Ausblenden (ms) | `FlashDiseaseFadeMs` | 2000 | 0 .. 10000 |

## Weiße Bombe

Ein Teil der Items unter den Steinen sind **weiße Bomben**. Sie sehen aus wie ein weißes Extra-Bomben-Item. Nach dem Aufheben ist die nächste gelegte Bombe eine weiße Bombe (grau gezeichnet, damit man sie nicht mit den Bomben des weißen Spielers verwechselt):

- Sie explodiert nach der normalen Zeit, aber ohne Flammen. Sie zerstört nichts und tötet niemanden.
- Sie macht die Bildschirme **aller Gegner** auf dem ganzen Feld weiß, wie die Krankheit [weißer Bildschirm](#weißer-bildschirm). Teamkollegen und der Spieler, der sie gelegt hat, sind nicht betroffen.
- Sie ist eine Extra-Bombe. Sie zählt nicht zu den Bomben des Spielers, danach legt er wieder normale Bomben.
- Sie kann gekickt, geworfen und geboxt werden wie jede andere Bombe.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `WhiteBomb` | 0 (aus) | 0 / 1 |
| Anteil der Items (%) | `WhiteBombChance` | 10 | 0 .. 100 |
| Stärke (%) | `WhiteBombIntensity` | 90 | 0 .. 100, 100 = man sieht nichts mehr |
| Dauer (ms) | `WhiteBombMs` | 2500 | 0 .. 10000 |
| Ausblenden (ms) | `WhiteBombFadeMs` | 2000 | 0 .. 10000 |

## Flammenwerfer

Ein Teil der Items unter den Steinen sind **Flammenwerfer**. Sie sehen aus wie ein rötliches Flammen-Item ![](data/res/powflame.png) mit rotem Rahmen. Solange der Spieler Tank hat, feuert er mit **gehaltener zweiter Aktion** einen Strahl in Blickrichtung.

- Der Strahl reicht eine einstellbare Zahl von Feldern weit. Er stoppt an Steinen und festen Blöcken, ohne sie zu zerstören, und an Bomben.
- Eine Bombe, die eine einstellbare Zeit am Stück im Strahl liegt, wird gezündet (Default: 1 Sekunde). Das gilt auch für Trigger-Bomben. Ein Blindgänger explodiert nicht, er verschwindet einfach, und sein Besitzer bekommt die Bombe zurück. Der Kill zählt für den Besitzer der Bombe. Das Zünden lässt sich abschalten, dann ist eine Bombe ein Schutzschild gegen den Strahl.
- Items im Strahl verbrennen sofort, wie bei einer Flamme. Das lässt sich abschalten, dann geht der Strahl über sie hinweg.
- Ein Gegner, der eine einstellbare Zeit am Stück im Strahl steht, wird getroffen wie von einer Flamme. Wer den Strahl verlässt, fängt wieder bei 0 an. Teamkollegen werden nur mit eingeschaltetem Friendly Fire verletzt, den Schützen selbst trifft der Strahl nie.
- Der Tank leert sich nur beim Feuern. Ist er leer, ist der Flammenwerfer weg. Weitere Flammenwerfer-Items füllen den Tank auf.
- Solange der Spieler Tank hat, zündet und boxt die zweite Aktion keine Bomben.
- Beim Feuern läuft man normal schnell, der Strahl dreht sich mit.
- Lässt man die Taste los, während das Spielfenster nicht aktiv ist (z. B. Alt+Tab), bekommt das Spiel das nicht mit: Der Strahl feuert weiter, bis man die Taste im Spiel erneut drückt und loslässt.
- Im Mod-Test-Modus gibt F4 (alle Items) auch Tank.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `FlameThrower` | 0 (aus) | 0 / 1 |
| Anteil der Items (%) | `FlameThrowerChance` | 5 | 0 .. 100 |
| Treibstoff pro Item (ms) | `FlameThrowerFuelMs` | 5000 | 0 .. 60000 |
| Reichweite (Felder) | `FlameThrowerRange` | 3 | 1 .. 14 |
| Zeit bis zum Treffer (ms) | `FlameThrowerKillMs` | 2000 | 0 .. 10000 |
| Teamkollegen verletzen (Friendly Fire) | `FlameThrowerFriendlyFire` | 0 (aus) | 0 / 1 |
| Items im Strahl verbrennen | `FlameThrowerBurnItems` | 1 (an) | 0 / 1 |
| Bomben im Strahl zünden | `FlameThrowerIgniteBombs` | 1 (an) | 0 / 1 |
| Zeit bis zur Zündung (ms) | `FlameThrowerIgniteMs` | 1000 | 0 .. 10000 (0 = sofort) |

## Trefferpunkte (Handicap)

Spieler überstehen mehrere Treffer. Jeder Treffer kostet einen Trefferpunkt (auch ein Treffer durch die eigene Bombe) und macht den Spieler kurz unverwundbar, damit dieselbe Flamme nicht zweimal trifft. Nur der letzte Treffer ist tödlich, und nur er zählt als Kill.

Die Trefferpunkte werden **pro Spielername** in einer Handicap-Tabelle eingestellt. Schwächere Spieler können so mehr Trefferpunkte bekommen als starke. Alle Spieler, die nicht in der Tabelle stehen, bekommen die Standard-Trefferpunkte. Zu Beginn jeder Runde werden die Trefferpunkte wieder aufgefüllt.

Im Hurry ist jeder Treffer tödlich, egal wie viele Trefferpunkte noch übrig sind.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `HitPoints` | 0 (aus) | 0 / 1 |
| Unverwundbar nach Treffer (ms) | `InvulnTimeMs` | 5000 | 500 .. 60000 |
| Standard-Trefferpunkte | `DefaultHitPoints` | 1 | 1 .. 255 |
| Trefferpunkte über den Spielern anzeigen | `ShowHitPoints` | 1 (an) | 0 / 1 |
| Handicap-Tabelle | Abschnitt `[Handicap]` | leer | `Spielername=Trefferpunkte`, 1 .. 255 |

**Zwei Spieler an einem PC:** Der zweite Spieler an einem PC (Tastatur 2) hat denselben Spielernamen wie der erste. In der Handicap-Tabelle steht er als `Name~2`, z. B.

```ini
[Handicap]
Anna=3
Anna~2=1
```

Ist die Anzeige an, steht die Zahl nur über einem Spieler, solange er mehr als 1 Trefferpunkt hat.

KI-Spieler haben keinen Spielernamen und bekommen deshalb immer die Standard-Trefferpunkte.

## Geister

Ein Spieler, der stirbt, verlässt die Runde nicht. Er läuft als kleiner, flackernder **Geist** auf einem Rahmen rund um das Spielfeld weiter (außerhalb des Spielfelds).

- **Bewegen:** Die Richtungstasten bewegen den Geist am Rahmen entlang. Wenn er stehen bleibt, schaut er immer ins Feld.
- **Werfen:** Die [erste Aktion](MANUAL_DE.md#erste-aktion) wirft eine Geisterbombe gerade ins Feld: Sie fliegt 3 Felder vom Rand in die Zeile oder Spalte vor dem Geist. Es kann immer nur eine Geisterbombe unterwegs sein. Die Flammenlänge der Geisterbombe ist einstellbar.
- **Zurückkommen:** Trifft die Flamme einer Geisterbombe einen **Gegner** (Teamkollegen zählen nicht), kommt der Geist an seiner Startposition ins Spiel zurück (oder auf dem nächsten freien Feld). Er hat dann die Fähigkeiten aus dem Schema (seine Items wurden beim Tod verteilt), volle Trefferpunkte und ist kurz unverwundbar.
- Für eine Geisterbombe bekommt der Geist keinen Kill. Der getroffene Spieler stirbt (oder verliert einen Trefferpunkt) wie gewohnt.
- Ist [Stun bei Bombenlandung](#stun-bei-bombenlandung) an, stunnen Geisterbomben wie geworfene Bomben.
- **Grenzen:** Ein Spieler kann pro Runde nur begrenzt oft Geist werden. Ein Geist, der innerhalb einer einstellbaren Zeit keinen Gegner trifft, verschwindet. In den letzten 5 s davor flackert er schneller.
- Mit dem Hurry verschwinden alle Geister.
- Die Runde endet wie gewohnt, wenn nur noch ein Spieler (oder Team) lebt, Geister zählen nicht. Geister spielen also erst ab 3 Spielern eine Rolle.
- Im [Spielmodus Hunter](GAMEOPTIONS_MANUAL_DE.md#hunter) wirken Geister nur ohne Respawn, mit Respawn kommen Tote ohnehin zurück.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `Ghosts` | 0 (aus) | 0 / 1 |
| Flammenlänge der Geisterbombe | `GhostFlameLen` | 2 | 1 .. 99 |
| Unverwundbar nach Rückkehr (ms) | `GhostReviveInvulnMs` | 3000 | 0 .. 60000 |
| Max. Anzahl Geist pro Runde | `GhostMaxCount` | 1 | 0 .. 99, 0 = unbegrenzt |
| Max. Zeit als Geist (ms) | `GhostMaxTimeMs` | 0 | 0 .. 600000, 0 = unbegrenzt |
| Flackerzeit (ms) | `GhostFlickerMs` | 200 | 0 .. 5000, 0 = kein Flackern |

Die Flackerzeit ist die Länge eines Takts: Ein Geist ist 3 Takte sichtbar und 1 Takt unsichtbar.

## Handschuh beim Start

Manchmal beginnt eine Runde in einer kleinen Nische zwischen Steinen, und eine versehentlich gelegte Bombe sperrt den Spieler ein. Mit diesem Mod kann jeder Spieler die Bombe unter sich werfen wie mit dem Handschuh ![](data/res/powgrab.png) (die [erste Aktion](MANUAL_DE.md#erste-aktion-doppelt-gedrückt) zweimal drücken), **bis zum ersten Schritt** der Runde.

- Eine Richtungstaste gegen eine Wand dreht den Spieler nur, ohne Schritt. So lässt sich der Wurf zielen.
- Spieler mit Spooger ![](data/res/powspoog.png) spoogen stattdessen wie gewohnt.
- Ein Geist, der ins Spiel zurückkommt, bekommt den Handschuh nicht noch einmal.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `GloveAtStart` | 0 (aus) | 0 / 1 |

## Offene Grenzen

Spieler können über den Rand des Spielfelds laufen und kommen auf der Gegenseite heraus, oben <-> unten und / oder links <-> rechts.

- Die Grenzen öffnen sich eine einstellbare Zeit nach Rundenbeginn. So kann niemand sofort aus seiner Startecke fliehen.
- Gelbe Pfeile auf dem Rahmen um das Feld zeigen, welche Grenzen offen sind. In den letzten 3 s vor dem Öffnen blinken sie, 10 s nach dem Öffnen verschwinden sie wieder, damit sie nicht stören (die Grenzen bleiben offen).
- Wer auf der Mitte einer Randkachel steht und weiter nach außen läuft, steht sofort auf der Gegenseite und läuft dort weiter.
- Ist das Feld gegenüber belegt (Stein, fester Block, Bombe), wirkt der Rand wie eine Wand.
- Nur Spieler gehen über den Rand. Gekickte Bomben und Flammen bleiben wie gewohnt am Rand stehen.
- Mit dem Hurry schließen sich die Grenzen wieder.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `OpenBorders` | 0 (aus) | 0 / 1 |
| Oben <-> unten | `OpenBordersTopBottom` | 1 (an) | 0 / 1 |
| Links <-> rechts | `OpenBordersLeftRight` | 1 (an) | 0 / 1 |
| Offen nach (s) | `OpenBordersActiveSec` | 30 | 0 .. 600, 0 = ab Rundenbeginn |

## Teleporter

Teleporter in der Mitte der Ränder bringen den Spieler zum Teleporter am gegenüberliegenden Rand, oben <-> unten und / oder links <-> rechts. Sie funktionieren wie die Löcher mancher Karten und sehen auch so aus, mit einem türkisen Ring darum.

- Die Teleporter erscheinen eine einstellbare Zeit nach Rundenbeginn. Vorher sind es normale Felder.
- Die Teleporter-Felder bleiben von Anfang an frei, und auf den Feldern daneben liegen keine Steine (feste Blöcke bleiben).
- Auf einen Teleporter kann keine Bombe gelegt werden, rollende Bomben bleiben davor stehen.
- Im Hurry sind die Teleporter weg.

| Einstellung (Dialog) | ini-Schlüssel | Default | Bereich |
|---|---|---|---|
| Mod an / aus | `Teleporters` | 0 (aus) | 0 / 1 |
| Oben <-> unten | `TeleportersTopBottom` | 1 (an) | 0 / 1 |
| Links <-> rechts | `TeleportersLeftRight` | 1 (an) | 0 / 1 |
| Erscheinen nach (s) | `TeleportersActiveSec` | 60 | 0 .. 600, 0 = ab Rundenbeginn |

## Items & Krankheiten

Stellt ein, **ob und wie oft** welches Item und welche Krankheit vorkommt. Ohne den Mod hat jeder Stein für jedes der 13 Items und für „nichts“ die gleiche Chance (je etwa 7 %).

- **Steine mit Item (%)**: Anteil der zerstörbaren Steine, unter denen ein Item liegt. 93 ist wie ohne den Mod.
- **Gewicht eines Items**: Der Anteil eines Items ist sein Gewicht geteilt durch die Summe aller Gewichte. 0 = das Item kommt nie. Stehen alle Gewichte auf 10, kommen die Items so oft wie ohne den Mod. Der Dialog zeigt neben jedem Item den Anteil an den Steinen.
- **Gewicht einer Krankheit** ![](data/res/powdisea.png): bestimmt, welche normale Krankheit ein Krankheits-Item gibt. Sind alle vier 0, gibt es kein Krankheits-Item.
- **Super schlechte Krankheit** ![](data/res/powebola.png): Anteil der super schlechten Krankheits-Items, die die Plätze mit einem anderen Spieler tauschen, statt dass der Spieler dauernd Bomben legt (10 % wie ohne den Mod).

Bitte beachten:

- Items, die die Karte (Scheme) verbietet, kommen nicht vor. Ihr Gewicht fällt weg, der Anteil der Steine mit Item bleibt also gleich.
- [Positive Krankheit](#positive-krankheit), [Durch Steine laufen](#durch-steine-laufen) und [Weißer Bildschirm](#weißer-bildschirm) werden wie bisher zuerst gewürfelt. Die Gewichte bestimmen nur die normale Krankheit danach.
- [Weiße Bombe](#weiße-bombe) und [Flammenwerfer](#flammenwerfer) ersetzen wie bisher danach einen Teil dieser Items.
- Die Items eines toten Spielers werden wie bisher wieder verteilt, egal was die Gewichte sagen.

| Einstellung | ini-Schlüssel | Standard | Bereich |
|---|---|---|---|
| Mod an / aus | `ItemConfig` | 0 (aus) | 0 / 1 |
| Steine mit Item (%) | `ItemChance` | 93 | 0 .. 100 |
| Extra Bombe ![](data/res/powbomb.png) | `ItemWeightExtraBomb` | 10 | 0 .. 100 |
| Längere Flamme ![](data/res/powflame.png) | `ItemWeightFlameLength` | 10 | 0 .. 100 |
| Goldene Flamme ![](data/res/powgold.png) | `ItemWeightGoldFlame` | 10 | 0 .. 100 |
| Schneller ![](data/res/powskate.png) | `ItemWeightSpeed` | 10 | 0 .. 100 |
| Kicken ![](data/res/powkick.png) | `ItemWeightKick` | 10 | 0 .. 100 |
| Schlagen (Handschuh) ![](data/res/powpunch.png) | `ItemWeightPunch` | 10 | 0 .. 100 |
| Greifen ![](data/res/powgrab.png) | `ItemWeightGrab` | 10 | 0 .. 100 |
| Spooger ![](data/res/powspoog.png) | `ItemWeightSpooger` | 10 | 0 .. 100 |
| Fernzünder ![](data/res/powtrig.png) | `ItemWeightTrigger` | 10 | 0 .. 100 |
| Gummibomben ![](data/res/powjelly.png) | `ItemWeightJelly` | 10 | 0 .. 100 |
| Krankheit ![](data/res/powdisea.png) | `ItemWeightDisease` | 10 | 0 .. 100 |
| Super schlechte Krankheit ![](data/res/powebola.png) | `ItemWeightSuperBadDisease` | 10 | 0 .. 100 |
| Langsam ![](data/res/powslow.png) | `ItemWeightSlow` | 10 | 0 .. 100 |
| Vertauschte Tasten | `DiseaseWeightInverted` | 10 | 0 .. 100 |
| Keine Bomben | `DiseaseWeightNoBombs` | 10 | 0 .. 100 |
| Blindgänger | `DiseaseWeightDudBombs` | 10 | 0 .. 100 |
| Super langsam | `DiseaseWeightSuperSlow` | 10 | 0 .. 100 |
| Super schlecht: Plätze tauschen (%) | `SuperBadSwapChance` | 10 | 0 .. 100 |

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
| Einfg | Je 2 Items der Mods auf freie Felder legen: weiße Bombe, Flammenwerfer, Box-Item, Krankheit |

Mit gehaltener **Shift**-Taste wirkt die Taste auf den zweiten Spieler an diesem PC. F10 wird nicht benutzt, weil Windows damit das Fenstermenü öffnet.

Die Test-Tasten ignorieren die Grenzen der Mods (z. B. geht F7 öfter als `GhostMaxCount`). So lassen sich Randfälle testen.

Diese Einstellungen wirken nur, solange der Mod-Test-Modus an ist:

| Einstellung (Dialog) | ini-Schlüssel (Abschnitt `[Debug]`) | Default | Bereich |
|---|---|---|---|
| Mod-Test-Modus an / aus | `ModTestMode` | 0 (aus) | 0 / 1 |
| Start-Items | `StartItems` | leer | z. B. `kick,punch,grab,bomb*3` |
| Rundenzeit (s, 0 = normal) | `RoundTimeSec` | 0 | 0 .. 3600, 0 = Zeit aus den Spieleinstellungen |
| Unverwundbar beim Start (ms) | `StartInvulnMs` | 0 | 0 .. 600000 |
| Geister zählen als lebendig | `GhostsKeepRound` | 0 (aus) | 0 / 1 |

- **Start-Items:** Jeder Spieler bekommt diese Items zu Beginn jeder Runde. Kommagetrennt, `*n` nimmt ein Item n-mal. Mögliche Items: `bomb`, `flame`, `goldflame`, `speed`, `kick`, `punch`, `grab`, `spooger`, `jelly`, `trigger`. `trigger` gehört hinter `bomb`, es gilt für die Bomben davor.
- **Rundenzeit:** Der Hurry beginnt bei 80 s Restzeit, `90` startet den Hurry also nach 10 s.
- **Geister zählen als lebendig:** Die Runde endet nicht, solange noch Geister übrig sind. So lassen sich Geister auch mit nur 2 Spielern testen.

## atomic_mods.ini von Hand bearbeiten

Die Datei liegt im Konfigurationsordner von FPC Atomic:

- Windows: `%LOCALAPPDATA%\fpc_atomic\atomic_mods.ini`
- Linux: `~/.config/fpc_atomic/atomic_mods.ini`

Ältere Versionen haben die Mods in `atomic_server.ini` gespeichert, der Server hat sie von dort gelesen. Gibt es `atomic_mods.ini` noch nicht, übernehmen Spiel und Launcher diese Datei einmalig, danach wird `atomic_server.ini` nicht mehr genutzt.

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
- Fehlende Schlüssel werden mit ihrem Default angelegt, sobald Spiel oder Launcher die Datei lesen. So findet man alle Schlüssel in der Datei.
- Ungültige Werte werden auf den nächsten gültigen Wert gesetzt. Das Spiel-Log (Konfigurationsordner, `log/client.log`) zeigt für jeden eine Warnung. Der Server prüft die empfangenen Werte noch einmal.
- Das Spiel liest die Datei jedes Mal, wenn es einem Server als erster Spieler beitritt. Änderungen gelten ab dem nächsten Spiel, ein Neustart ist nicht nötig.
- Der Dialog **Mods...** im Launcher schreibt den Abschnitt `[Handicap]` komplett neu. Kommentare in diesem Abschnitt gehen verloren, Kommentare in allen anderen Abschnitten bleiben erhalten.
