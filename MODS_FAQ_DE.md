# Mods-FAQ

[English version](MODS_FAQ.md)

Fragen und Antworten zu den [Spiel-Mods](MODS_MANUAL_DE.md).

- [Welche Mods geben Anfängern eine Chance gegen spielstarke Gegner?](#welche-mods-geben-anfängern-eine-chance-gegen-spielstarke-gegner)

## Welche Mods geben Anfängern eine Chance gegen spielstarke Gegner?

Im Originalspiel gewinnt ein starker Spieler gegen Anfänger fast jede Runde. Einige Mods gleichen das aus. Am wichtigsten ist das [Handicap](#trefferpunkte-handicap), weil es als einziger Mod die Spieler unterschiedlich behandelt. Die anderen Mods helfen Anfängern, weil sie typische Anfängerfehler abfangen oder etwas Glück ins Spiel bringen.

### Empfohlen

#### Trefferpunkte (Handicap)

Mit [Trefferpunkten (Handicap)](MODS_MANUAL_DE.md#trefferpunkte-handicap) übersteht jeder Spieler eine Zahl von Treffern, eingestellt pro Spielername. Das ist der direkteste Weg, das Spiel auszugleichen:

- `Standard-Trefferpunkte` für die starken Spieler auf 1 lassen und Anfängern in der Handicap-Tabelle 2 oder 3 geben.
- Nach einem Treffer ist der Spieler eine Weile unverwundbar (`Unverwundbar nach Treffer`, Default 5 s). Das reicht, um sich in Sicherheit zu bringen, also bei 5 s oder mehr lassen.
- **Trefferpunkte über den Spielern anzeigen** anlassen, damit alle sehen, wer noch Extraleben hat.
- Die Tabelle nach ein paar Runden anpassen: Gewinnt ein Anfänger zu oft, einen Trefferpunkt wegnehmen.

Im Hurry ist jeder Treffer tödlich. Mit längerer Spielzeit oder unbegrenzter Zeit (Launcher: **Spiel...**) zählen die Trefferpunkte über einen größeren Teil der Runde, mit unbegrenzter Zeit gibt es gar keinen Hurry.

#### Handschuh beim Start

Ein typischer Anfängerfehler: In der Startecke eine Bombe legen und sich damit selbst einsperren. Mit dem [Handschuh beim Start](MODS_MANUAL_DE.md#handschuh-beim-start) kann jeder Spieler diese Bombe bis zum ersten Schritt wegwerfen. Starken Spielern passiert das selten, der Mod hilft also vor allem Anfängern.

#### Geister (ab 3 Spielern)

Anfänger sterben meist zuerst und schauen dann nur noch zu. Mit [Geistern](MODS_MANUAL_DE.md#geister) spielen sie am Rand des Felds weiter und können in die Runde zurückkommen:

- `Max. Anzahl Geist pro Runde` 1 oder 2: Ein Spieler kann ein- oder zweimal pro Runde zurückkommen.
- `Max. Zeit als Geist` 0: Der Geist hat so viel Zeit, wie er braucht.
- `Flammenlänge der Geisterbombe` 3 statt 2: Die Geisterbombe trifft leichter.

Auch starke Spieler können Geist werden, aber weil sie seltener sterben, haben sie weniger davon.

#### Positive Krankheit

Anfänger sammeln alles ein, auch die Krankheits-Items, starke Spieler weichen ihnen aus. Mit der [positiven Krankheit](MODS_MANUAL_DE.md#positive-krankheit) macht ein Teil der Krankheits-Items unverwundbar. So wird Alles-Einsammeln weniger bestraft, und das Glück beim Schutz gleicht das Spiel etwas aus. Mit einer Chance von 30 bis 50 % merkt man den Effekt deutlich.

#### Durch Steine laufen

Wirkt ähnlich wie die positive Krankheit: [Durch Steine laufen](MODS_MANUAL_DE.md#durch-steine-laufen) macht aus manchen Krankheits-Items einen Fluchtweg. Das hilft Spielern, die eingekesselt sind, und das passiert Anfängern öfter.

### Eher nicht, oder mit Vorsicht

Diese Mods belohnen genaues Timing und Zielen, davon profitieren starke Spieler mehr:

- **[Boxhandschuh](MODS_MANUAL_DE.md#boxhandschuh):** Gegner stunnen, dann eine Bombe daneben legen. Starke Spieler nutzen das als Kombo.
- **[Stun bei Bombenlandung](MODS_MANUAL_DE.md#stun-bei-bombenlandung):** Genaue Würfe brauchen Übung. Der Stun trifft auch den Werfer, Anfänger stunnen sich oft selbst.
- **[Offene Grenzen](MODS_MANUAL_DE.md#offene-grenzen) und [Teleporter](MODS_MANUAL_DE.md#teleporter):** mehr Wege zum Fliehen und zum Verfolgen. Starke Spieler nutzen sie gezielt, Anfänger verlieren den Überblick.
- **[Flammenwerfer](MODS_MANUAL_DE.md#flammenwerfer):** leicht zu bedienen, aber starke Spieler weichen dem Strahl aus und treffen öfter. Ist er an, macht ihn eine längere **Zeit bis zum Treffer** (z. B. 3000 ms) weniger gefährlich.

[Weißer Bildschirm](MODS_MANUAL_DE.md#weißer-bildschirm) und [weiße Bombe](MODS_MANUAL_DE.md#weiße-bombe) sind neutral: Sie bringen Chaos für alle, aber ein blinder Anfänger ist genauso verloren wie ein blinder Profi. Gut für den Spaß, nicht für den Ausgleich.

### Außerhalb der Mods

Auch einige Optionen des Spiels (Launcher: **Spiel...**) helfen:

- **Teamspiel:** Einen Anfänger und einen starken Spieler in ein Team stecken. Zusammen mit dem Handicap funktioniert das sehr gut.
- **Weniger Siege bis zum Matchgewinn:** Je weniger Siege nötig sind, desto mehr zählt eine Glücksrunde.
- **Längere oder unbegrenzte Spielzeit:** weniger Druck durch den Hurry, und die Trefferpunkte bleiben länger nützlich (siehe oben).

### Beispiel

Eine Einstellung für einen Abend mit zwei Anfängern (Anna, Ben) und einigen starken Spielern, in `atomic_server.ini` (oder dieselben Werte im Launcher unter **Mods...**):

```ini
[Mods]
HitPoints=1
DefaultHitPoints=1
InvulnTimeMs=5000
ShowHitPoints=1
GloveAtStart=1
Ghosts=1
GhostMaxCount=2
GhostMaxTimeMs=0
GhostFlameLen=3
PositiveDisease=1
PositiveDiseaseChance=40

[Handicap]
Anna=3
Ben=2
```

Die Spielernamen in `[Handicap]` müssen genau so geschrieben sein wie im Spiel. Ein zweiter Spieler am selben PC heißt `Name~2`, siehe [Trefferpunkte (Handicap)](MODS_MANUAL_DE.md#trefferpunkte-handicap).
