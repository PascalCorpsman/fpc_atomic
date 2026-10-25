(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of FPC_Atomic                                            *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit ulanguage;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms;

Type
  TLanguage = (lgEnglish, lgGerman);

Const
  LanguageCodes: Array[TLanguage] Of String = ('en', 'de'); // So steht die Sprache in fpc_atomic.cfg ([Launcher] Language)

Var
  Language: TLanguage = lgEnglish; // Die Sprache des Launchers, die .lfm Dateien und der Code sind englisch

(*
 * Übersetzt einen Text in die eingestellte Sprache, unbekannte Texte kommen
 * unverändert zurück. Geht in beide Richtungen, ein schon übersetzter Text
 * wird also auch wieder englisch.
 *)
Function Tr(Const Text: String): String;

(*
 * Übersetzt alle Texte eines Formulars in die eingestellte Sprache: Caption,
 * Hint und TextHint aller Komponenten, die Einträge von Listen und Comboboxen
 * und die Kopfzeilen von Tabellen. Kann beliebig oft aufgerufen werden.
 *)
Procedure TranslateForm(Const Form: TCustomForm);

Implementation

Uses TypInfo, StdCtrls, CheckLst, Grids;

Type
  TTranslation = Record
    En, De: String;
  End;

Const
  (*
   * Englisch -> Deutsch. Jeder deutsche Text darf nur einmal vorkommen und kein
   * englischer sein, sonst ist der Weg zurück ins Englische nicht eindeutig.
   *)
  Translations: Array[0..251] Of TTranslation = (
    // Hauptfenster (unit1)
    (En: ' Player '; De: ' Spieler '),
    (En: '&Player name'; De: 'S&pielername'),
    (En: 'Choose the keys or a gamepad for the 1st and 2nd player on this PC.';
     De: 'Tasten oder Gamepad für den 1. und 2. Spieler an diesem PC wählen.'),
    (En: '&Key assignment...'; De: '&Tastenbelegung...'),
    (En: ' Display && sound '; De: ' Anzeige && Ton '),
    (En: 'Start the game in fullscreen mode.'; De: 'Das Spiel im Vollbild starten.'),
    (En: '&Fullscreen'; De: '&Vollbild'),
    (En: 'Play the music. The sound effects can be switched off in Game...';
     De: 'Die Musik abspielen. Die Soundeffekte lassen sich unter Spiel... abschalten.'),
    (En: 'Play mu&sic'; De: 'Musik a&bspielen'),
    (En: 'Stretch the game to fill the whole window. Off: keep the aspect ratio with black bars.';
     De: 'Das Spiel auf das ganze Fenster strecken. Aus: Seitenverhältnis behalten, mit schwarzen Rändern.'),
    (En: 'Fill &window'; De: '&Fenster füllen'),
    (En: 'Show the frames per second in the game.'; De: 'Die Bilder pro Sekunde im Spiel anzeigen.'),
    (En: 'S&how FPS'; De: 'FPS an&zeigen'),
    (En: ' Internet game (optional) '; De: ' Internetspiel (optional) '),
    (En: 'Join a game over the internet: the game connects to the router IP and port below. Not needed in the ' +
      'local network.';
     De: 'Einem Spiel über das Internet beitreten: Das Spiel verbindet sich mit der Router-IP und dem Port ' +
       'unten. Im lokalen Netz nicht nötig.'),
    (En: '&Join through a router'; De: 'Über einen &Router beitreten'),
    (En: 'Router &IP'; De: 'Router-&IP'),
    (En: 'IP address of the router of the player who hosts the game.'; De: 'IP-Adresse des Routers des Spielers, der das Spiel hostet.'),
    (En: 'Po&rt'; De: 'P&ort'),
    (En: 'Port the router forwards to the server of the host (default 5521).';
     De: 'Port, den der Router an den Server des Hosts weiterleitet (Standard 5521).'),
    (En: 'Start the game.'#13#10#13#10 +
      'Ctrl + click (or Ctrl+Alt+L):'#13#10'host a network game directly.'#13#10#13#10 +
      'Shift + click (or Shift+Alt+L):'#13#10'join a network game directly.';
     De: 'Das Spiel starten (Alt+L).'#13#10#13#10 +
       'Strg + Klick (oder Strg+Alt+L):'#13#10'direkt ein Netzwerkspiel hosten.'#13#10#13#10 +
       'Shift + Klick (oder Shift+Alt+L):'#13#10'direkt einem Netzwerkspiel beitreten.'),
    (En: '&Launch FPC Atomic'; De: 'FPC Atomic starten'),
    (En: 'Installed game version. "up to date" is only shown after "Updates...".';
     De: 'Installierte Spielversion. "aktuell" erscheint erst nach "Updates...".'),
    (En: 'Do not close the launcher when the game starts, it comes back when the game is closed.';
     De: 'Den Launcher beim Spielstart nicht schließen, er erscheint wieder, wenn das Spiel beendet wird.'),
    (En: 'Show the launcher again &after the game'; De: 'Launcher nach dem Spiel wieder &anzeigen'),
    (En: ' Tools '; De: ' Werkzeuge '),
    (En: 'Optional game mods like stun, ghosts or hit points. They apply when a game is hosted on this PC.';
     De: 'Optionale Spiel-Mods wie Stun, Geister oder Trefferpunkte. Sie gelten, wenn auf diesem PC ein Spiel ' +
       'gehostet wird.'),
    (En: 'The options of the game (team play, scheme, play time, ...), the same as in the options of the game.';
     De: 'Die Optionen des Spiels (Teamspiel, Schema, Spielzeit, ...), die selben wie in den Optionen im Spiel.'),
    (En: '&Game...'; De: '&Spiel...'),
    (En: 'Check online for a new version and download it.'; De: 'Online nach einer neuen Version suchen und sie herunterladen.'),
    (En: 'Extract the game data from the original Atomic Bomberman CD.';
     De: 'Die Spieldaten von der originalen Atomic Bomberman CD auslesen.'),
    (En: 'CD e&xtractor...'; De: 'CD-E&xtraktor...'),
    (En: 'Open the folder with the settings, the logs and atomic_server.ini.';
     De: 'Den Ordner mit den Einstellungen, den Logs und atomic_server.ini öffnen.'),
    (En: '&Config folder'; De: '&Konfigordner'),
    (En: 'Open the game manual (MANUAL.md).'; De: 'Die Spielanleitung öffnen (MANUAL_DE.md).'),
    (En: 'Manual'; De: 'Anleitung'),
    (En: 'Cl&ose'; De: 'Schli&eßen'),
    (En: '✓ valid name'; De: '✓ gültiger Name'),
    (En: '✗ a-z 0-9 _ space, max. 19'; De: '✗ a-z 0-9 _ Leerz., max. 19'),
    (En: '✓ Game data found'; De: '✓ Spieldaten gefunden'),
    (En: '✗ Game data missing, run CD extractor'; De: '✗ Spieldaten fehlen, CD-Extraktor starten'),
    (En: 'Game ver. unknown'; De: 'Spielversion unbekannt'),
    (En: 'Game ver. '; De: 'Spielversion '),
    (En: ', up to date'; De: ', aktuell'),
    (En: 'is not a valid nodename, (need to be less than 20 chars and no spezial chars!';
     De: 'ist kein gültiger Spielername (weniger als 20 Zeichen und keine Sonderzeichen)!'),
    (En: 'Error, the following files are missing:'; De: 'Fehler, die folgenden Dateien fehlen:'),
    (En: 'Please re run cd_data_extractor'; De: 'Bitte den cd_data_extractor noch einmal ausführen'),
    (En: 'Error, installation not complete, please run "Check for updates"';
     De: 'Fehler, die Installation ist nicht vollständig, bitte "Updates..." ausführen'),
    (En: 'In the launcher location there is a folder named "cd_data_extractor", this will crash the update process';
     De: 'Neben dem Launcher liegt ein Ordner "cd_data_extractor", damit stürzt das Update ab'),
    (En: 'Do you want to rename that folder ?'; De: 'Soll der Ordner umbenannt werden?'),
    (En: 'Warning'; De: 'Warnung'),
    (En: 'Error, could not rename.'; De: 'Fehler, umbenennen nicht möglich.'),
    (En: 'Your version of fpc_atomic is up to date.'; De: 'Deine Version von fpc_atomic ist aktuell.'),
    // Tastenbelegung (ukeyboarddialog)
    (En: 'Keyboard settings'; De: 'Tastenbelegung'),
    (En: 'Keyboard 0'; De: 'Tastatur 0'),
    (En: 'Keyboard 1'; De: 'Tastatur 1'),
    (En: 'Key Up:'; De: 'Hoch:'),
    (En: 'Key Down:'; De: 'Runter:'),
    (En: 'Key Left:'; De: 'Links:'),
    (En: 'Key Right:'; De: 'Rechts:'),
    (En: 'Key Primary:'; De: 'Erste Aktion:'),
    (En: 'Key Secondary:'; De: 'Zweite Aktion:'),
    (En: 'Grab'; De: 'Ändern'),
    (En: 'Use joystick'; De: 'Joystick'),
    (En: 'Use keyboard'; De: 'Tastatur'),
    (En: 'default'; De: 'Standard'),
    (En: 'Cancel'; De: 'Abbrechen'),
    // Updates (unit3)
    (En: 'Update overview'; De: 'Update-Übersicht'),
    (En: 'Update launcher'; De: 'Launcher aktualisieren'),
    (En: 'Files that should be updated:'; De: 'Dateien, die aktualisiert werden sollten:'),
    (En: 'Additional files that can be downloaded:'; De: 'Zusätzliche Dateien, die heruntergeladen werden können:'),
    (En: 'Download and update files'; De: 'Dateien herunterladen und aktualisieren'),
    (En: 'All'; De: 'Alle'),
    (En: 'None'; De: 'Keine'),
    (En: 'Last release message:'; De: 'Nachricht zum letzten Release:'),
    (En: '%d files to download'; De: '%d Dateien zum Herunterladen'),
    (En: 'Nothing for download selected.'; De: 'Nichts zum Herunterladen ausgewählt.'),
    (En: 'Zip files are merged without checks, do you really want to continue ?';
     De: 'Zip Dateien werden ohne Prüfung eingespielt, wirklich fortfahren?'),
    (En: 'Need to download '; De: 'Herunterzuladen sind '),
    (En: ' continue?'; De: ', fortfahren?'),
    (En: 'Error, '; De: 'Fehler, '),
    (En: ' not found, without updater launcher can not be updated.';
     De: ' nicht gefunden, ohne Updater kann der Launcher nicht aktualisiert werden.'),
    // Mods (unit4)
    (En: 'Check a mod to switch it on, click its name to see its settings.';
     De: 'Einen Mod anhaken schaltet ihn ein, ein Klick auf den Namen zeigt seine Einstellungen.'),
    (En: 'Stun on bomb landing'; De: 'Stun bei Bombenlandung'),
    (En: 'Boxing glove'; De: 'Boxhandschuh'),
    (En: 'Positive disease'; De: 'Positive Krankheit'),
    (En: 'Walk through bricks'; De: 'Durch Steine laufen'),
    (En: 'White screen'; De: 'Weißer Bildschirm'),
    (En: 'White bomb'; De: 'Weiße Bombe'),
    (En: 'Flamethrower'; De: 'Flammenwerfer'),
    (En: 'Hit points'; De: 'Trefferpunkte'),
    (En: 'Ghosts'; De: 'Geister'),
    (En: 'Glove at start'; De: 'Handschuh beim Start'),
    (En: 'Open borders'; De: 'Offene Grenzen'),
    (En: 'Teleporters'; De: 'Teleporter'),
    (En: 'Mod test mode'; De: 'Mod-Test-Modus'),
    (En: 'On: a thrown bomb (glove or ghost) that lands on a player stuns everyone on that tile.';
     De: 'An: Eine geworfene Bombe (Handschuh oder Geist), die auf einem Spieler landet, stunnt alle auf diesem Feld.'),
    (En: 'A thrown bomb (glove or ghost) that lands on a player stuns everyone on that tile. Punched bombs do not stun.';
     De: 'Eine geworfene Bombe (Handschuh, Geist), die auf einem Spieler landet, stunnt alle auf dem Feld. ' +
       'Geboxte nicht.'),
    (En: 'Min. stun time (ms)'; De: 'Min. Stun-Zeit (ms)'),
    (En: 'Shortest stun time. The stun time is random between min. and max.';
     De: 'Kürzeste Stun-Zeit. Die Stun-Zeit ist zufällig zwischen Min. und Max.'),
    (En: 'Max. stun time (ms)'; De: 'Max. Stun-Zeit (ms)'),
    (En: 'Longest stun time. The stun time is random between min. and max.';
     De: 'Längste Stun-Zeit. Die Stun-Zeit ist zufällig zwischen Min. und Max.'),
    (En: 'On: with the punch item the second action stuns an opponent standing in front of the player.';
     De: 'An: Mit dem Box-Item stunnt die zweite Aktion einen Gegner, der vor dem Spieler steht.'),
    (En: 'With the punch item the second action stuns an opponent standing on the tile in front of the player.';
     De: 'Mit dem Box-Item stunnt die zweite Aktion einen Gegner, der auf dem Feld vor dem Spieler steht.'),
    (En: 'Wait before next punch (ms)'; De: 'Pause bis zum nächsten Schlag (ms)'),
    (En: 'After a punch the same player cannot punch again for this time, so nobody can be stunned all the time.';
     De: 'Nach einem Schlag kann der selbe Spieler so lange nicht wieder schlagen, damit niemand dauernd gestunnt ist.'),
    (En: 'Works also while standing still. A bomb on the same tile stays where it is. Team mates and ' +
      'invulnerable players are not stunned.';
     De: 'Geht auch im Stehen. Eine Bombe auf dem selben Feld bleibt liegen. Teamkollegen und unverwundbare ' +
       'Spieler werden nicht gestunnt.'),
    (En: 'On: some disease items make the player invulnerable for a while instead of sick.';
     De: 'An: Manche Krankheits-Items machen den Spieler eine Weile unverwundbar statt krank.'),
    (En: 'Some disease items make the player invulnerable for a random time instead of sick (not during hurry).';
     De: 'Manche Krankheits-Items machen den Spieler eine zufällige Zeit unverwundbar statt krank (nicht im Hurry).'),
    (En: 'Chance of a disease item (%)'; De: 'Chance pro Krankheits-Item (%)'),
    (En: 'Share of the disease items that make the player invulnerable instead of sick.';
     De: 'Anteil der Krankheits-Items, die unverwundbar statt krank machen.'),
    (En: 'Min. invulnerable time (ms)'; De: 'Min. Unverwundbarkeit (ms)'),
    (En: 'Shortest invulnerable time. The time is random between min. and max.';
     De: 'Kürzeste Zeit der Unverwundbarkeit. Die Zeit ist zufällig zwischen Min. und Max.'),
    (En: 'Max. invulnerable time (ms)'; De: 'Max. Unverwundbarkeit (ms)'),
    (En: 'Longest invulnerable time. The time is random between min. and max.';
     De: 'Längste Zeit der Unverwundbarkeit. Die Zeit ist zufällig zwischen Min. und Max.'),
    (En: 'On: some disease items let the player walk through destructible bricks for a while.';
     De: 'An: Mit manchen Krankheits-Items läuft der Spieler eine Weile durch zerstörbare Steine.'),
    (En: 'Some disease items let the player walk through destructible bricks for a random time (not during hurry).';
     De: 'Mit manchen Krankheits-Items läuft der Spieler eine zufällige Zeit durch zerstörbare Steine (nicht im Hurry).'),
    (En: 'Share of the disease items that let the player walk through bricks. Rolled after the positive disease.';
     De: 'Anteil der Krankheits-Items, mit denen der Spieler durch Steine läuft. Wird nach der positiven ' +
       'Krankheit ausgewürfelt.'),
    (En: 'Min. time (ms)'; De: 'Min. Zeit (ms)'),
    (En: 'Shortest time. The time is random between min. and max.'; De: 'Kürzeste Zeit. Die Zeit ist zufällig zwischen Min. und Max.'),
    (En: 'Max. time (ms)'; De: 'Max. Zeit (ms)'),
    (En: 'Longest time. The time is random between min. and max.'; De: 'Längste Zeit. Die Zeit ist zufällig zwischen Min. und Max.'),
    (En: 'See-through (%)'; De: 'Durchsichtig (%)'),
    (En: 'How see-through the player is drawn: 0 = normal, 100 = invisible.';
     De: 'Wie durchsichtig der Spieler gezeichnet wird: 0 = normal, 100 = unsichtbar.'),
    (En: 'If the time ends inside a brick, the player can only walk out of it (through further bricks only ' +
      'while there is no free tile next to the player). No bombs can be placed inside a brick, and the ' +
      'player is hit when the brick explodes.';
     De: 'Endet die Zeit in einem Stein, kann der Spieler nur noch hinauslaufen (durch weitere Steine nur, ' +
       'solange neben ihm kein freies Feld ist). In einem Stein kann er keine Bomben legen, und er wird ' +
       'getroffen, wenn der Stein explodiert.'),
    (En: 'On: some disease items turn the screen of the player white for a while.';
     De: 'An: Manche Krankheits-Items machen den Bildschirm des Spielers eine Weile weiß.'),
    (En: 'Some disease items turn the screen of the player white for a while.';
     De: 'Manche Krankheits-Items machen den Bildschirm des Spielers eine Weile weiß.'),
    (En: 'Share of the disease items that turn the screen white. Rolled after the positive effects.';
     De: 'Anteil der Krankheits-Items, die den Bildschirm weiß machen. Wird nach den positiven Effekten ausgewürfelt.'),
    (En: 'Strength (%)'; De: 'Stärke (%)'),
    (En: 'How white the screen gets: 100 = nothing can be seen any more.';
     De: 'Wie weiß der Bildschirm wird: 100 = man sieht nichts mehr.'),
    (En: 'Duration (ms)'; De: 'Dauer (ms)'),
    (En: 'How long the screen stays white.'; De: 'Wie lange der Bildschirm weiß bleibt.'),
    (En: 'Fade back (ms)'; De: 'Ausblenden (ms)'),
    (En: 'How long it takes until the screen is normal again.'; De: 'Wie lange es dauert, bis der Bildschirm wieder normal ist.'),
    (En: 'AI players are not affected. If two players play on one PC, that PC shows no white screen, because ' +
      'both share the screen.';
     De: 'KI-Spieler sind nicht betroffen. Spielen zwei Spieler an einem PC, zeigt dieser PC keinen weißen ' +
       'Bildschirm, weil sich beide den Bildschirm teilen.'),
    (En: 'On: some items below the bricks are white bombs that turn the screens of all opponents white.';
     De: 'An: Manche Items unter den Steinen sind weiße Bomben, die die Bildschirme aller Gegner weiß machen.'),
    (En: 'A white bomb item: the next bomb explodes without flames and turns the screens of all opponents white.';
     De: 'Weiße Bombe: Die nächste Bombe explodiert ohne Flammen und macht die Bildschirme aller Gegner weiß.'),
    (En: 'Share of the items (%)'; De: 'Anteil der Items (%)'),
    (En: 'Share of the items below the bricks that become a white bomb at the start of a round.';
     De: 'Anteil der Items unter den Steinen, die zu Rundenbeginn eine weiße Bombe werden.'),
    (En: 'How white the screens get: 100 = nothing can be seen any more.';
     De: 'Wie weiß die Bildschirme werden: 100 = man sieht nichts mehr.'),
    (En: 'How long the screens stay white.'; De: 'Wie lange die Bildschirme weiß bleiben.'),
    (En: 'How long it takes until the screens are normal again.'; De: 'Wie lange es dauert, bis die Bildschirme wieder normal sind.'),
    (En: 'Team mates and the player who placed it are not affected. The white bomb is an extra bomb, ' +
      'afterwards the player places normal bombs again.';
     De: 'Teamkollegen und wer sie gelegt hat sind nicht betroffen. Die weiße Bombe ist eine Extrabombe, ' +
       'danach legt der Spieler wieder normale Bomben.'),
    (En: 'On: some items below the bricks are flamethrowers, hold the second action to fire.';
     De: 'An: Manche Items unter den Steinen sind Flammenwerfer, zum Feuern die zweite Aktion gedrückt halten.'),
    (En: 'A flamethrower item: hold the second action to fire in the direction you look.';
     De: 'Flammenwerfer: Die zweite Aktion gedrückt halten, um in Blickrichtung zu feuern.'),
    (En: 'Share of the items below the bricks that become a flamethrower at the start of a round.';
     De: 'Anteil der Items unter den Steinen, die zu Rundenbeginn ein Flammenwerfer werden.'),
    (En: 'Fuel per item (ms)'; De: 'Treibstoff pro Item (ms)'),
    (En: 'How long a player can fire with one item. Only used up while firing, more items add fuel.';
     De: 'Wie lange ein Spieler mit einem Item feuern kann. Wird nur beim Feuern verbraucht, weitere Items ' +
       'geben mehr Treibstoff.'),
    (En: 'Range (tiles)'; De: 'Reichweite (Felder)'),
    (En: 'How far the beam reaches. It stops at bricks and solid blocks.';
     De: 'Wie weit der Strahl reicht. Er endet an Steinen und festen Blöcken.'),
    (En: 'Time to kill (ms)'; De: 'Zeit bis zum Treffer (ms)'),
    (En: 'How long an opponent has to stay in the beam without a break until being hit.';
     De: 'Wie lange ein Gegner ohne Unterbrechung im Strahl stehen muss, bis er getroffen wird.'),
    (En: 'On: items in the beam burn at once, like with a flame. Off: the beam goes over them.';
     De: 'An: Items im Strahl verbrennen sofort, wie bei einer Flamme. Aus: Der Strahl geht über sie hinweg.'),
    (En: 'Burn items in the beam'; De: 'Items im Strahl verbrennen'),
    (En: 'The beam stops at bricks and ignites bombs. Team mates are not hurt. While the player has fuel, the ' +
      'second action does not trigger or punch bombs.';
     De: 'Der Strahl endet an Steinen und zündet Bomben. Teamkollegen werden nicht verletzt. Solange der ' +
       'Spieler Treibstoff hat, zündet oder boxt die zweite Aktion keine Bomben.'),
    (En: 'On: players survive several hits, set per player name in the handicap table.';
     De: 'An: Spieler überleben mehrere Treffer, einstellbar pro Spielername in der Handicap-Tabelle.'),
    (En: 'Players survive several hits, set per player name. During hurry every hit is lethal.';
     De: 'Spieler überleben mehrere Treffer, einstellbar pro Spielername. Im Hurry ist jeder Treffer tödlich.'),
    (En: 'Invulnerable after a hit (ms)'; De: 'Unverwundbar nach Treffer (ms)'),
    (En: 'After a hit that is not lethal the player is invulnerable for this time, so the same flame does not hit twice.';
     De: 'Nach einem nicht tödlichen Treffer ist der Spieler so lange unverwundbar, damit die selbe Flamme ' +
       'nicht zweimal trifft.'),
    (En: 'Default hit points'; De: 'Standard-Trefferpunkte'),
    (En: 'Hit points of all players that are not in the handicap table.';
     De: 'Trefferpunkte aller Spieler, die nicht in der Handicap-Tabelle stehen.'),
    (En: 'Show the remaining hit points above each player (only if more than 1).';
     De: 'Die restlichen Trefferpunkte über jedem Spieler anzeigen (nur wenn mehr als 1).'),
    (En: 'Show hit points above the players'; De: 'Trefferpunkte über den Spielern anzeigen'),
    (En: 'Handicap per player:'; De: 'Handicap pro Spieler:'),
    (En: 'Hit points per player name, e.g. more for weaker players. Name~2 is the second player on the same PC.';
     De: 'Trefferpunkte pro Spielername, z.B. mehr für schwächere Spieler. Name~2 ist der zweite Spieler am selben PC.'),
    (En: 'Add a line.'; De: 'Eine Zeile hinzufügen.'),
    (En: 'Delete the selected line.'; De: 'Die gewählte Zeile löschen.'),
    (En: 'Name~2 = second player on the same PC.'; De: 'Name~2 = zweiter Spieler am selben PC.'),
    (En: 'Player name'; De: 'Spielername'),
    (En: 'On: dead players walk around the field as ghosts and can throw bombs to get back into the game.';
     De: 'An: Tote Spieler laufen als Geister über das Feld und können mit Bomben zurück ins Spiel kommen.'),
    (En: 'Dead players walk around the field as ghosts until hurry. A ghost bomb that hits an opponent brings ' +
      'the ghost back.';
     De: 'Tote Spieler laufen bis zum Hurry als Geister herum. Trifft ihre Bombe einen Gegner, kommen sie zurück.'),
    (En: 'Flame length of the ghost bomb'; De: 'Flammenlänge der Geisterbombe'),
    (En: 'Flame length of the bomb a ghost throws.'; De: 'Flammenlänge der Bombe, die ein Geist wirft.'),
    (En: 'Invulnerable after return (ms)'; De: 'Unverwundbar nach Rückkehr (ms)'),
    (En: 'A ghost that comes back is invulnerable for this time.'; De: 'Ein zurückgekehrter Geist ist so lange unverwundbar.'),
    (En: 'Max. times ghost per round'; De: 'Max. Anzahl Geist pro Runde'),
    (En: 'How often a player can become a ghost per round, 0 = unlimited.';
     De: 'Wie oft ein Spieler pro Runde zum Geist werden kann, 0 = unbegrenzt.'),
    (En: 'Max. time as ghost (ms)'; De: 'Max. Zeit als Geist (ms)'),
    (En: 'A ghost that hits no opponent within this time vanishes, 0 = unlimited.';
     De: 'Ein Geist, der in dieser Zeit keinen Gegner trifft, verschwindet, 0 = unbegrenzt.'),
    (En: 'Flicker time (ms)'; De: 'Flackerzeit (ms)'),
    (En: 'Ghosts are visible for 3 ticks and invisible for 1 tick, this is the length of a tick. 0 = no flicker.';
     De: 'Geister sind 3 Takte sichtbar und 1 Takt unsichtbar, das ist die Länge eines Takts. 0 = kein Flackern.'),
    (En: '0 = unlimited, flicker time 0 = no flicker. In the last 5 s before a ghost vanishes it flickers faster.';
     De: '0 = unbegrenzt, Flackerzeit 0 = kein Flackern. In den letzten 5 s vor dem Verschwinden flackert ein ' +
       'Geist schneller.'),
    (En: 'On: until the first step of a round every player can throw the bomb below the player like with the glove.';
     De: 'An: Bis zum ersten Schritt einer Runde kann jeder Spieler die Bombe unter sich werfen wie mit dem Handschuh.'),
    (En: 'Until the first step of a round every player can throw the bomb below the player like with the glove ' +
      '(press the first action twice). Pressing against a wall only turns, so you can aim before throwing.';
     De: 'Bis zum ersten Schritt einer Runde kann jeder Spieler die Bombe unter sich werfen wie mit dem ' +
       'Handschuh (erste Aktion zweimal drücken). Gegen eine Wand drücken dreht nur, so kann man vor dem ' +
       'Wurf zielen.'),
    (En: 'On: players can walk over the border and come out on the opposite side.';
     De: 'An: Spieler können über den Rand laufen und kommen auf der gegenüberliegenden Seite heraus.'),
    (En: 'Players can walk over the border of the field and come out on the opposite side.';
     De: 'Spieler können über den Rand des Felds laufen und kommen auf der Gegenseite heraus.'),
    (En: 'Players walk from the top border to the bottom border and back.'; De: 'Spieler laufen vom oberen zum unteren Rand und zurück.'),
    (En: 'Top <-> bottom'; De: 'Oben <-> unten'),
    (En: 'Players walk from the left border to the right border and back.'; De: 'Spieler laufen vom linken zum rechten Rand und zurück.'),
    (En: 'Left <-> right'; De: 'Links <-> rechts'),
    (En: 'Open after (s)'; De: 'Offen nach (s)'),
    (En: 'Seconds after the start of a round until the borders open, so nobody flees from the start corner at ' +
      'once. 0 = from the start.';
     De: 'Sekunden nach Rundenbeginn, bis die Ränder offen sind, damit niemand sofort aus der Startecke ' +
       'flieht. 0 = ab Start.'),
    (En: 'Arrows on the frame show the open borders, they blink 3 s before the borders open. If the tile on ' +
      'the opposite side is blocked (brick, bomb), the border works like a wall. Only players go over the ' +
      'border.';
     De: 'Pfeile am Rahmen zeigen die offenen Ränder, 3 s vor dem Öffnen blinken sie. Ist das Feld auf der ' +
       'Gegenseite belegt (Stein, Bombe), wirkt der Rand wie eine Wand. Nur Spieler gehen über den Rand.'),
    (En: 'On: teleporters in the middle of the borders bring the player to the opposite border.';
     De: 'An: Teleporter in der Mitte der Ränder bringen den Spieler zum gegenüberliegenden Rand.'),
    (En: 'Teleporters in the middle of the borders bring the player to the opposite border (not during hurry).';
     De: 'Teleporter in der Mitte der Ränder bringen den Spieler zum gegenüberliegenden Rand (nicht im Hurry).'),
    (En: 'Teleporters in the middle of the top and the bottom border.'; De: 'Teleporter in der Mitte des oberen und des unteren Rands.'),
    (En: 'Teleporters in the middle of the left and the right border.'; De: 'Teleporter in der Mitte des linken und des rechten Rands.'),
    (En: 'Appear after (s)'; De: 'Erscheinen nach (s)'),
    (En: 'Seconds after the start of a round until the teleporters appear and work, 0 = from the start.';
     De: 'Sekunden nach Rundenbeginn, bis die Teleporter erscheinen und funktionieren, 0 = ab Start.'),
    (En: 'The teleporter tiles and the tiles next to them stay free of bricks from the start. No bombs can be ' +
      'placed on a teleporter, rolling bombs stop in front of it.';
     De: 'Die Teleporterfelder und die Felder daneben bleiben von Anfang an frei von Steinen. Auf einem ' +
       'Teleporter kann man keine Bomben legen, rollende Bomben halten davor an.'),
    (En: 'On: test keys F1..F12 and Ins for the host and some test settings. Shows a hint to all players.';
     De: 'An: Testtasten F1..F12 und Einfg für den Host und einige Testeinstellungen. Zeigt allen Spielern ' +
       'einen Hinweis.'),
    (En: 'Only for testing, shows a hint to all players. In game F3 shows the test keys (only for the master).';
     De: 'Nur zum Testen, zeigt allen Spielern einen Hinweis. Im Spiel zeigt F3 die Testtasten (nur für den Master).'),
    (En: 'Start items'; De: 'Start-Items'),
    (En: 'Items every player gets at round start, comma separated, *n = n times. Possible: bomb, flame, ' +
      'goldflame, speed, kick, punch, grab, spooger, jelly, trigger.';
     De: 'Items, die jeder Spieler zu Rundenbeginn bekommt, mit Komma getrennt, *n = n mal. Möglich: bomb, ' +
       'flame, goldflame, speed, kick, punch, grab, spooger, jelly, trigger.'),
    (En: 'e.g. kick,punch,grab,bomb*3'; De: 'z.B. kick,punch,grab,bomb*3'),
    (En: 'Round time (s, 0 = normal)'; De: 'Rundenzeit (s, 0 = normal)'),
    (En: 'Round time in seconds, hurry starts at 80 s remaining. 0 = time from the game settings.';
     De: 'Rundenzeit in Sekunden, der Hurry beginnt 80 s vor Schluss. 0 = Zeit aus den Spieleinstellungen.'),
    (En: 'Invulnerable at start (ms)'; De: 'Unverwundbar beim Start (ms)'),
    (En: 'All players are invulnerable for this time at round start.'; De: 'Alle Spieler sind zu Rundenbeginn so lange unverwundbar.'),
    (En: 'Ghosts count as alive, so the round does not end when only one player is left. Allows testing ghosts ' +
      'with 2 players.';
     De: 'Geister zählen als lebendig, die Runde endet also nicht, wenn nur noch ein Spieler übrig ist. So ' +
       'lassen sich Geister mit 2 Spielern testen.'),
    (En: 'Ghosts count as alive (test ghosts with 2 players)'; De: 'Geister zählen als lebendig (Geister mit 2 Spielern testen)'),
    (En: 'Changes apply to the next game started on this PC.';
     De: 'Änderungen gelten für das nächste Spiel, das auf diesem PC gestartet wird.'),
    (En: 'Open the mods manual (MODS_MANUAL.md, with a link to the German version).';
     De: 'Die Anleitung der Mods öffnen (MODS_MANUAL_DE.md).'),
    (En: 'Mods manual'; De: 'Mods-Anleitung'),
    (En: 'Reset all mods to their default values (all off). Saved only with OK.';
     De: 'Alle Mods auf die Standardwerte zurücksetzen (alle aus). Gespeichert wird erst mit OK.'),
    (En: '&Defaults'; De: '&Standard'),
    (En: 'Save the settings to atomic_server.ini. They apply to the next game hosted on this PC.';
     De: 'Die Einstellungen in atomic_server.ini speichern. Sie gelten für das nächste Spiel, das auf diesem ' +
       'PC gehostet wird.'),
    (En: 'Close without saving.'; De: 'Schließen, ohne zu speichern.'),
    (En: 'Error, could not write:'; De: 'Fehler, kann nicht schreiben:'),
    (En: 'Error, max. stun time is smaller than min. stun time.'; De: 'Fehler, die max. Stun-Zeit ist kleiner als die min. Stun-Zeit.'),
    (En: 'Error, max. time is smaller than min. time.'; De: 'Fehler, die max. Zeit ist kleiner als die min. Zeit.'),
    (En: 'Error, invalid handicap in line %d:'; De: 'Fehler, ungültiges Handicap in Zeile %d:'),
    (En: 'The player name needs to be a valid name (optional with ~2 at the end) and the hit points need to be in 1..%d.';
     De: 'Der Spielername muss ein gültiger Name sein (optional mit ~2 am Ende) und die Trefferpunkte müssen ' +
       'in 1..%d liegen.'),
    (En: 'Error, the player name "%s" is used twice.'; De: 'Fehler, der Spielername "%s" kommt zweimal vor.'),
    // Spiel Optionen (unit5)
    (En: 'Game options'; De: 'Spieloptionen'),
    (En: 'Players play in two teams (red and white).'; De: 'Die Spieler spielen in zwei Teams (rot und weiß).'),
    (En: 'Team play'; De: 'Teamspiel'),
    (En: 'Players start at random start positions of the scheme.'; De: 'Die Spieler starten auf zufälligen Startpositionen des Schemas.'),
    (En: 'Random start positions'; De: 'Zufällige Startpositionen'),
    (En: 'If a network player loses the connection, an AI takes over.';
     De: 'Verliert ein Netzwerkspieler die Verbindung, übernimmt eine KI.'),
    (En: 'Lost net players revert to AI'; De: 'Getrennte Netzspieler werden zur KI'),
    (En: 'Play the sound effects (explosions, items, ...). The music is switched with "Play music" in the launcher.';
     De: 'Die Soundeffekte abspielen (Explosionen, Items, ...). Die Musik schaltet "Musik abspielen" im Launcher.'),
    (En: 'Play sound effects'; De: 'Soundeffekte abspielen'),
    (En: 'Scheme'; De: 'Schema'),
    (En: 'The scheme sets the bricks, the start positions and the items (files in data/schemes).';
     De: 'Das Schema legt die Steine, die Startpositionen und die Items fest (Dateien in data/schemes).'),
    (En: 'Play time'; De: 'Spielzeit'),
    (En: 'Time of a round. The hurry starts 80 s before the end. Infinity = no time limit and no hurry.';
     De: 'Zeit einer Runde. Der Hurry beginnt 80 s vor Schluss. Unendlich = kein Zeitlimit und kein Hurry.'),
    (En: 'Infinity'; De: 'Unendlich'),
    (En: 'Wins to win the match'; De: 'Siege für den Matchgewinn'),
    (En: 'How many rounds a player or team has to win to win the match.';
     De: 'Wie viele Runden ein Spieler oder Team gewinnen muss, um das Match zu gewinnen.'),
    (En: 'Conveyor speed'; De: 'Tempo der Förderbänder'),
    (En: 'Speed of the conveyor belts on maps that have them.'; De: 'Tempo der Förderbänder auf Karten, die welche haben.'),
    (En: 'Slow'; De: 'Langsam'),
    (En: 'Middle'; De: 'Mittel'),
    (En: 'Fast'; De: 'Schnell'),
    (En: 'Network port'; De: 'Netzwerk-Port'),
    (En: 'TCP port of the server when hosting a game (default 5521).';
     De: 'TCP-Port des Servers, wenn ein Spiel gehostet wird (Standard 5521).'),
    (En: 'Volume (%)'; De: 'Lautstärke (%)'),
    (En: 'Sound volume, in the game it can also be changed with + and -.'; De: 'Lautstärke, im Spiel auch mit + und - änderbar.'),
    (En: 'The same settings as in the options of the game. Field and wins can also be changed in the game when hosting.';
     De: 'Die selben Einstellungen wie in den Optionen im Spiel. Feld und Siege lassen sich beim Hosten auch ' +
       'im Spiel ändern.'),
    (En: 'Reset the settings to the defaults of the game. Saved only with OK.';
     De: 'Die Einstellungen auf die Standardwerte des Spiels zurücksetzen. Gespeichert wird erst mit OK.'),
    (En: 'Save the settings, they apply to the next start of the game.';
     De: 'Die Einstellungen speichern, sie gelten beim nächsten Start des Spiels.')
    );

Function Tr(Const Text: String): String;
Var
  i: integer;
Begin
  result := Text;
  If Text = '' Then exit;
  For i := low(Translations) To high(Translations) Do Begin
    If (Language = lgGerman) And (Translations[i].En = Text) Then Begin
      result := Translations[i].De;
      exit;
    End;
    If (Language = lgEnglish) And (Translations[i].De = Text) Then Begin
      result := Translations[i].En;
      exit;
    End;
  End;
End;

Procedure TranslateProperty(Const Instance: TObject; Const PropName: String);
Var
  Info: PPropInfo;
  s: String;
Begin
  Info := GetPropInfo(Instance, PropName, [tkAString]);
  If Not assigned(Info) Then exit;
  s := GetStrProp(Instance, Info);
  If Tr(s) <> s Then SetStrProp(Instance, Info, Tr(s));
End;

Procedure TranslateItems(Const Items: TStrings);
Var
  i: integer;
Begin
  For i := 0 To Items.Count - 1 Do Begin
    If Tr(Items[i]) <> Items[i] Then Items[i] := Tr(Items[i]);
  End;
End;

Procedure TranslateForm(Const Form: TCustomForm);
Var
  i, j, k, Index: integer;
  c: TComponent;
  Checked: Array Of Boolean;
  Grid: TStringGrid;
Begin
  Checked := Nil;
  TranslateProperty(Form, 'Caption');
  For i := 0 To Form.ComponentCount - 1 Do Begin
    c := Form.Components[i];
    TranslateProperty(c, 'Caption');
    TranslateProperty(c, 'Hint');
    TranslateProperty(c, 'TextHint');
    // Beim Ersetzen eines Eintrags können Auswahl und Haken verloren gehen, deshalb merken
    If c Is TCheckListBox Then Begin
      setlength(Checked, TCheckListBox(c).Count);
      For j := 0 To high(Checked) Do Begin
        Checked[j] := TCheckListBox(c).Checked[j];
      End;
      Index := TCheckListBox(c).ItemIndex;
      TranslateItems(TCheckListBox(c).Items);
      For j := 0 To high(Checked) Do Begin
        TCheckListBox(c).Checked[j] := Checked[j];
      End;
      TCheckListBox(c).ItemIndex := Index;
    End;
    If c Is TCustomComboBox Then Begin
      Index := TCustomComboBox(c).ItemIndex;
      TranslateItems(TCustomComboBox(c).Items);
      TCustomComboBox(c).ItemIndex := Index;
    End;
    // Die Kopfzeilen von Tabellen
    If c Is TStringGrid Then Begin
      Grid := TStringGrid(c);
      For j := 0 To Grid.FixedRows - 1 Do Begin
        For k := 0 To Grid.ColCount - 1 Do Begin
          Grid.Cells[k, j] := Tr(Grid.Cells[k, j]);
        End;
      End;
    End;
  End;
End;

End.
