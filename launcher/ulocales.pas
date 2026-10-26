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
Unit ulocales;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms;

Type
  TLanguage = (lgEnglish, lgGerman, lgFrench);

Const
  LanguageCodes: Array[TLanguage] Of String = ('en', 'de', 'fr'); // So steht die Sprache in fpc_atomic.cfg ([Launcher] Language)
  LanguageNames: Array[TLanguage] Of String = ('English', 'Deutsch', 'Français'); // Wie die Sprache in der Auswahl heißt, jeweils in der eigenen Sprache

Var
  Language: TLanguage = lgEnglish; // Die Sprache des Launchers, die .lfm Dateien und der Code sind englisch

(*
 * Übersetzt einen Text in die eingestellte Sprache, unbekannte Texte kommen
 * unverändert zurück. Geht in beide Richtungen, ein schon übersetzter Text
 * wird also auch wieder englisch.
 *)
Function Tr(Const Text: String): String;

(*
 * Liefert die Sprache zu einem Code aus fpc_atomic.cfg, bei einem unbekannten Code Englisch.
 *)
Function CodeToLanguage(Const Code: String): TLanguage;

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
    En, De, Fr: String;
  End;

Const
  (*
   * Englisch -> Deutsch und Französisch. Ein Text darf nur in einem Eintrag
   * vorkommen (egal in welcher Sprache), sonst ist der Weg zurück ins Englische
   * oder in eine andere Sprache nicht eindeutig. Innerhalb eines Eintrags dürfen
   * Texte gleich sein (z.B. "Po&rt").
   *)
  Translations: Array[0..252] Of TTranslation = (
    // Hauptfenster (unit1)
    (En: 'The language of the launcher.'; De: 'Die Sprache des Launchers.'; Fr: 'La langue du lanceur.'),
    (En: ' Player '; De: ' Spieler ';
     Fr: ' Joueur '),
    (En: '&Player name'; De: 'S&pielername';
     Fr: '&Nom du joueur'),
    (En: 'Choose the keys or a gamepad for the 1st and 2nd player on this PC.';
     De: 'Tasten oder Gamepad für den 1. und 2. Spieler an diesem PC wählen.';
     Fr: 'Choisissez les touches ou une manette pour les premier et deuxième joueurs sur ce PC.'),
    (En: '&Key assignment...'; De: '&Tastenbelegung...';
     Fr: 'Affectation des &touches...'),
    (En: ' Display && sound '; De: ' Anzeige && Ton ';
     Fr: ' Affichage && son '),
    (En: 'Start the game in fullscreen mode.'; De: 'Das Spiel im Vollbild starten.';
     Fr: 'Lancer le jeu en plein écran.'),
    (En: '&Fullscreen'; De: '&Vollbild';
     Fr: '&Plein écran'),
    (En: 'Play the music. The sound effects can be switched off in Game...';
     De: 'Die Musik abspielen. Die Soundeffekte lassen sich unter Spiel... abschalten.';
     Fr: 'Jouer la musique. Les effets sonores peuvent être désactivés dans « Jeu... ».'),
    (En: 'Play mu&sic'; De: 'Musik a&bspielen';
     Fr: 'Jouer la mu&sique'),
    (En: 'Stretch the game to fill the whole window. Off: keep the aspect ratio with black bars.';
     De: 'Das Spiel auf das ganze Fenster strecken. Aus: Seitenverhältnis behalten, mit schwarzen Rändern.';
     Fr: 'Étendre le jeu à toute la fenêtre. Désactivé : conserver les proportions avec ' +
       'des bandes noires.'),
    (En: 'Fill &window'; De: '&Fenster füllen';
     Fr: 'Remplir la &fenêtre'),
    (En: 'Show the frames per second in the game.'; De: 'Die Bilder pro Sekunde im Spiel anzeigen.';
     Fr: 'Afficher les images par seconde dans le jeu.'),
    (En: 'S&how FPS'; De: 'FPS an&zeigen';
     Fr: 'Afficher les &FPS'),
    (En: ' Internet game (optional) '; De: ' Internetspiel (optional) ';
     Fr: ' Partie sur Internet (facultatif) '),
    (En: 'Join a game over the internet: the game connects to the router IP and port below. Not needed in the ' +

      'local network.';
     De: 'Einem Spiel über das Internet beitreten: Das Spiel verbindet sich mit der Router-IP und dem Port ' +

       'unten. Im lokalen Netz nicht nötig.';
     Fr: 'Rejoindre une partie via Internet : le jeu se connecte à l''adresse IP du routeur et au ' +
       'port ci-dessous. Inutile en réseau local.'),
    (En: '&Join through a router'; De: 'Über einen &Router beitreten';
     Fr: '&Rejoindre via un routeur'),
    (En: 'Router &IP'; De: 'Router-&IP';
     Fr: '&IP du routeur'),
    (En: 'IP address of the router of the player who hosts the game.'; De: 'IP-Adresse des Routers des Spielers, der das Spiel hostet.';
     Fr: 'Adresse IP du routeur du joueur qui héberge la partie.'),
    (En: 'Po&rt'; De: 'P&ort';
     Fr: 'Po&rt'),
    (En: 'Port the router forwards to the server of the host (default 5521).';
     De: 'Port, den der Router an den Server des Hosts weiterleitet (Standard 5521).';
     Fr: 'Port redirigé par le routeur vers le serveur de l''hôte (5521 par défaut).'),
    (En: 'Start the game.'#13#10#13#10 +

      'Ctrl + click (or Ctrl+Alt+L):'#13#10'host a network game directly.'#13#10#13#10 +

      'Shift + click (or Shift+Alt+L):'#13#10'join a network game directly.';
     De: 'Das Spiel starten (Alt+L).'#13#10#13#10 +

       'Strg + Klick (oder Strg+Alt+L):'#13#10'direkt ein Netzwerkspiel hosten.'#13#10#13#10 +

       'Shift + Klick (oder Shift+Alt+L):'#13#10'direkt einem Netzwerkspiel beitreten.';
     Fr: 'Lancer le jeu (Alt+L).'#13#10#13#10'Ctrl + clic (ou Ctrl+Alt+L) :'#13#10 +
       'héberger directement une partie en réseau.'#13#10#13#10'Maj + clic (ou Maj+Alt+L) :'#13#10 +
       'rejoindre directement une partie en réseau.'),
    (En: '&Launch FPC Atomic'; De: 'FPC Atomic starten';
     Fr: 'Lancer FPC Atomic'),
    (En: 'Installed game version. "up to date" is only shown after "Updates...".';
     De: 'Installierte Spielversion. "aktuell" erscheint erst nach "Updates...".';
     Fr: 'Version du jeu installée. « à jour » n''apparaît qu''après « Mises à jour... ».'),
    (En: 'Do not close the launcher when the game starts, it comes back when the game is closed.';
     De: 'Den Launcher beim Spielstart nicht schließen, er erscheint wieder, wenn das Spiel beendet wird.';
     Fr: 'Ne pas fermer le lanceur au démarrage du jeu, il réapparaît à la fermeture du ' +
       'jeu.'),
    (En: 'Show the launcher again &after the game'; De: 'Launcher nach dem Spiel wieder &anzeigen';
     Fr: 'Réafficher le lanceur &après le jeu'),
    (En: ' Tools '; De: ' Werkzeuge ';
     Fr: ' Outils '),
    (En: 'Optional game mods like stun, ghosts or hit points. They apply when a game is hosted on this PC.';
     De: 'Optionale Spiel-Mods wie Stun, Geister oder Trefferpunkte. Sie gelten, wenn auf diesem PC ein Spiel ' +

       'gehostet wird.';
     Fr: 'Mods de jeu facultatifs comme l''étourdissement, les fantômes ou les points de ' +
       'vie. Ils s''appliquent quand une partie est hébergée sur ce PC.'),
    (En: 'The options of the game (team play, scheme, play time, ...), the same as in the options of the game.';
     De: 'Die Optionen des Spiels (Teamspiel, Schema, Spielzeit, ...), die selben wie in den Optionen im Spiel.';
     Fr: 'Les options de la partie (jeu en équipe, schéma, durée, etc.), identiques à celles disponibles dans les ' +
       'options du jeu.'),
    (En: '&Game...'; De: '&Spiel...';
     Fr: '&Jeu...'),
    (En: 'Check online for a new version and download it.'; De: 'Online nach einer neuen Version suchen und sie herunterladen.';
     Fr: 'Rechercher en ligne une nouvelle version et la télécharger.'),
    (En: 'Extract the game data from the original Atomic Bomberman CD.';
     De: 'Die Spieldaten von der originalen Atomic Bomberman CD auslesen.';
     Fr: 'Extraire les données du jeu du CD original d''Atomic Bomberman.'),
    (En: 'CD e&xtractor...'; De: 'CD-E&xtraktor...';
     Fr: 'E&xtracteur de CD...'),
    (En: 'Open the folder with the settings, the logs and atomic_server.ini.';
     De: 'Den Ordner mit den Einstellungen, den Logs und atomic_server.ini öffnen.';
     Fr: 'Ouvrir le dossier contenant les paramètres, les journaux et le fichier atomic_server.ini.'),
    (En: '&Config folder'; De: '&Konfigordner';
     Fr: 'Dossier de &configuration'),
    (En: 'Open the game manual (MANUAL.md).'; De: 'Die Spielanleitung öffnen (MANUAL_DE.md).';
     Fr: 'Ouvrir le manuel du jeu (MANUAL.md, en anglais).'),
    (En: 'Manual'; De: 'Anleitung';
     Fr: 'Manuel (en anglais)'),
    (En: 'Cl&ose'; De: 'Schli&eßen';
     Fr: '&Fermer'),
    (En: '✓ valid name'; De: '✓ gültiger Name';
     Fr: '✓ nom valide'),
    (En: '✗ a-z 0-9 _ space, max. 19'; De: '✗ a-z 0-9 _ Leerz., max. 19';
     Fr: '✗ a-z, 0-9, _ et espace ; 19 caractères max.'),
    (En: '✓ Game data found'; De: '✓ Spieldaten gefunden';
     Fr: '✓ Données du jeu trouvées'),
    (En: '✗ Game data missing, run CD extractor'; De: '✗ Spieldaten fehlen, CD-Extraktor starten';
     Fr: '✗ Données du jeu manquantes, lancer l''extracteur de CD'),
    (En: 'Game ver. unknown'; De: 'Spielversion unbekannt';
     Fr: 'Version du jeu inconnue'),
    (En: 'Game ver. '; De: 'Spielversion ';
     Fr: 'Version du jeu '),
    (En: ', up to date'; De: ', aktuell';
     Fr: ', à jour'),
    (En: 'is not a valid nodename, (need to be less than 20 chars and no spezial chars!';
     De: 'ist kein gültiger Spielername (weniger als 20 Zeichen und keine Sonderzeichen)!';
     Fr: 'n''est pas un nom de joueur valide (moins de 20 caractères, sans caractères ' +
       'spéciaux) !'),
    (En: 'Error, the following files are missing:'; De: 'Fehler, die folgenden Dateien fehlen:';
     Fr: 'Erreur, les fichiers suivants sont manquants :'),
    (En: 'Please re run cd_data_extractor'; De: 'Bitte den cd_data_extractor noch einmal ausführen';
     Fr: 'Veuillez relancer cd_data_extractor'),
    (En: 'Error, installation not complete, please run "Check for updates"';
     De: 'Fehler, die Installation ist nicht vollständig, bitte "Updates..." ausführen';
     Fr: 'Erreur, l''installation n''est pas complète, veuillez exécuter « Mises à jour... »'),
    (En: 'In the launcher location there is a folder named "cd_data_extractor", this will crash the update process';
     De: 'Neben dem Launcher liegt ein Ordner "cd_data_extractor", damit stürzt das Update ab';
     Fr: 'Un dossier « cd_data_extractor » se trouve à côté du lanceur ; cela fait échouer ' +
       'la mise à jour'),
    (En: 'Do you want to rename that folder ?'; De: 'Soll der Ordner umbenannt werden?';
     Fr: 'Voulez-vous renommer ce dossier ?'),
    (En: 'Warning'; De: 'Warnung';
     Fr: 'Avertissement'),
    (En: 'Error, could not rename.'; De: 'Fehler, umbenennen nicht möglich.';
     Fr: 'Erreur : impossible de renommer le dossier.'),
    (En: 'Your version of fpc_atomic is up to date.'; De: 'Deine Version von fpc_atomic ist aktuell.';
     Fr: 'Votre version de fpc_atomic est à jour.'),
    // Tastenbelegung (ukeyboarddialog)
    (En: 'Keyboard settings'; De: 'Tastenbelegung';
     Fr: 'Réglages du clavier'),
    (En: 'Keyboard 0'; De: 'Tastatur 0';
     Fr: 'Clavier 0'),
    (En: 'Keyboard 1'; De: 'Tastatur 1';
     Fr: 'Clavier 1'),
    (En: 'Key Up:'; De: 'Hoch:';
     Fr: 'Haut :'),
    (En: 'Key Down:'; De: 'Runter:';
     Fr: 'Bas :'),
    (En: 'Key Left:'; De: 'Links:';
     Fr: 'Gauche :'),
    (En: 'Key Right:'; De: 'Rechts:';
     Fr: 'Droite :'),
    (En: 'Key Primary:'; De: 'Erste Aktion:';
     Fr: 'Première action :'),
    (En: 'Key Secondary:'; De: 'Zweite Aktion:';
     Fr: 'Deuxième action :'),
    (En: 'Grab'; De: 'Ändern';
     Fr: 'Modifier'),
    (En: 'Use joystick'; De: 'Joystick';
     Fr: 'Manette'),
    (En: 'Use keyboard'; De: 'Tastatur';
     Fr: 'Clavier'),
    (En: 'default'; De: 'Standard';
     Fr: 'Par défaut'),
    (En: 'Cancel'; De: 'Abbrechen';
     Fr: 'Annuler'),
    // Updates (unit3)
    (En: 'Update overview'; De: 'Update-Übersicht';
     Fr: 'Aperçu des mises à jour'),
    (En: 'Update launcher'; De: 'Launcher aktualisieren';
     Fr: 'Mettre à jour le lanceur'),
    (En: 'Files that should be updated:'; De: 'Dateien, die aktualisiert werden sollten:';
     Fr: 'Fichiers à mettre à jour :'),
    (En: 'Additional files that can be downloaded:'; De: 'Zusätzliche Dateien, die heruntergeladen werden können:';
     Fr: 'Fichiers supplémentaires téléchargeables :'),
    (En: 'Download and update files'; De: 'Dateien herunterladen und aktualisieren';
     Fr: 'Télécharger et mettre à jour les fichiers'),
    (En: 'All'; De: 'Alle';
     Fr: 'Tous'),
    (En: 'None'; De: 'Keine';
     Fr: 'Aucun'),
    (En: 'Last release message:'; De: 'Nachricht zum letzten Release:';
     Fr: 'Message de la dernière version :'),
    (En: '%d files to download'; De: '%d Dateien zum Herunterladen';
     Fr: '%d fichiers à télécharger'),
    (En: 'Nothing for download selected.'; De: 'Nichts zum Herunterladen ausgewählt.';
     Fr: 'Rien n''est sélectionné pour le téléchargement.'),
    (En: 'Zip files are merged without checks, do you really want to continue ?';
     De: 'Zip Dateien werden ohne Prüfung eingespielt, wirklich fortfahren?';
     Fr: 'Les fichiers zip sont intégrés sans vérification, voulez-vous vraiment ' +
       'continuer ?'),
    (En: 'Need to download '; De: 'Herunterzuladen sind ';
     Fr: 'Il faut télécharger '),
    (En: ' continue?'; De: ', fortfahren?';
     Fr: ', continuer ?'),
    (En: 'Error, '; De: 'Fehler, ';
     Fr: 'Erreur, '),
    (En: ' not found, without updater launcher can not be updated.';
     De: ' nicht gefunden, ohne Updater kann der Launcher nicht aktualisiert werden.';
     Fr: ' introuvable, sans updater le lanceur ne peut pas être mis à jour.'),
    // Mods (unit4)
    (En: 'Check a mod to switch it on, click its name to see its settings.';
     De: 'Einen Mod anhaken schaltet ihn ein, ein Klick auf den Namen zeigt seine Einstellungen.';
     Fr: 'Cochez un mod pour l''activer ; cliquez sur son nom pour afficher ses paramètres.'),
    (En: 'Stun on bomb landing'; De: 'Stun bei Bombenlandung';
     Fr: 'Étourdissement à l''atterrissage d''une bombe'),
    (En: 'Boxing glove'; De: 'Boxhandschuh';
     Fr: 'Gant de boxe'),
    (En: 'Positive disease'; De: 'Positive Krankheit';
     Fr: 'Maladie bénéfique'),
    (En: 'Walk through bricks'; De: 'Durch Steine laufen';
     Fr: 'Traverser les briques'),
    (En: 'White screen'; De: 'Weißer Bildschirm';
     Fr: 'Écran blanc'),
    (En: 'White bomb'; De: 'Weiße Bombe';
     Fr: 'Bombe blanche'),
    (En: 'Flamethrower'; De: 'Flammenwerfer';
     Fr: 'Lance-flammes'),
    (En: 'Hit points'; De: 'Trefferpunkte';
     Fr: 'Points de vie'),
    (En: 'Ghosts'; De: 'Geister';
     Fr: 'Fantômes'),
    (En: 'Glove at start'; De: 'Handschuh beim Start';
     Fr: 'Gant au départ'),
    (En: 'Open borders'; De: 'Offene Grenzen';
     Fr: 'Bords ouverts'),
    (En: 'Teleporters'; De: 'Teleporter';
     Fr: 'Téléporteurs'),
    (En: 'Mod test mode'; De: 'Mod-Test-Modus';
     Fr: 'Mode test des mods'),
    (En: 'On: a thrown bomb (glove or ghost) that lands on a player stuns everyone on that tile.';
     De: 'An: Eine geworfene Bombe (Handschuh oder Geist), die auf einem Spieler landet, stunnt alle auf diesem Feld.';
     Fr: 'Activé : une bombe lancée (gant ou fantôme) qui atterrit sur un joueur étourdit ' +
       'tous ceux qui sont sur cette case.'),
    (En: 'A thrown bomb (glove or ghost) that lands on a player stuns everyone on that tile. Punched bombs do not stun.';
     De: 'Eine geworfene Bombe (Handschuh, Geist), die auf einem Spieler landet, stunnt alle auf dem Feld. ' +

       'Geboxte nicht.';
     Fr: 'Une bombe lancée (avec le gant ou par un fantôme) qui atterrit sur un joueur étourdit tous ceux ' +
       'qui sont qui se trouvent sur cette case. Les bombes frappées ne provoquent pas d''étourdissement.'),
    (En: 'Min. stun time (ms)'; De: 'Min. Stun-Zeit (ms)';
     Fr: 'Durée min. d''étourdissement (ms)'),
    (En: 'Shortest stun time. The stun time is random between min. and max.';
     De: 'Kürzeste Stun-Zeit. Die Stun-Zeit ist zufällig zwischen Min. und Max.';
     Fr: 'Durée d''étourdissement la plus courte. La durée est aléatoire entre min. et max.'),
    (En: 'Max. stun time (ms)'; De: 'Max. Stun-Zeit (ms)';
     Fr: 'Durée max. d''étourdissement (ms)'),
    (En: 'Longest stun time. The stun time is random between min. and max.';
     De: 'Längste Stun-Zeit. Die Stun-Zeit ist zufällig zwischen Min. und Max.';
     Fr: 'Durée d''étourdissement la plus longue. La durée est aléatoire entre min. et max.'),
    (En: 'On: with the punch item the second action stuns an opponent standing in front of the player.';
     De: 'An: Mit dem Box-Item stunnt die zweite Aktion einen Gegner, der vor dem Spieler steht.';
     Fr: 'Activé : avec l''objet de coup de poing, la deuxième action étourdit un ' +
       'adversaire placé devant le joueur.'),
    (En: 'With the punch item the second action stuns an opponent standing on the tile in front of the player.';
     De: 'Mit dem Box-Item stunnt die zweite Aktion einen Gegner, der auf dem Feld vor dem Spieler steht.';
     Fr: 'Avec l''objet de coup de poing, la deuxième action étourdit un adversaire placé ' +
       'sur la case devant le joueur.'),
    (En: 'Wait before next punch (ms)'; De: 'Pause bis zum nächsten Schlag (ms)';
     Fr: 'Pause avant le prochain coup (ms)'),
    (En: 'After a punch the same player cannot punch again for this time, so nobody can be stunned all the time.';
     De: 'Nach einem Schlag kann der selbe Spieler so lange nicht wieder schlagen, damit niemand dauernd gestunnt ist.';
     Fr: 'Après un coup, le même joueur ne peut pas frapper à nouveau pendant ce temps, ' +
       'afin que personne ne soit étourdi en permanence.'),
    (En: 'Works also while standing still. A bomb on the same tile stays where it is. Team mates and ' +

      'invulnerable players are not stunned.';
     De: 'Geht auch im Stehen. Eine Bombe auf dem selben Feld bleibt liegen. Teamkollegen und unverwundbare ' +

       'Spieler werden nicht gestunnt.';
     Fr: 'Fonctionne aussi à l''arrêt. Une bombe sur la même case reste en place. Les ' +
       'coéquipiers et les joueurs invulnérables ne sont pas étourdis.'),
    (En: 'On: some disease items make the player invulnerable for a while instead of sick.';
     De: 'An: Manche Krankheits-Items machen den Spieler eine Weile unverwundbar statt krank.';
     Fr: 'Activé : certains objets de maladie rendent le joueur invulnérable un moment au ' +
       'lieu de le rendre malade.'),
    (En: 'Some disease items make the player invulnerable for a random time instead of sick (not during hurry).';
     De: 'Manche Krankheits-Items machen den Spieler eine zufällige Zeit unverwundbar statt krank (nicht im Hurry).';
     Fr: 'Certains objets de maladie rendent le joueur invulnérable pendant une durée ' +
       'aléatoire au lieu de le rendre malade (pas pendant le hurry).'),
    (En: 'Chance of a disease item (%)'; De: 'Chance pro Krankheits-Item (%)';
     Fr: 'Chance par objet de maladie (%)'),
    (En: 'Share of the disease items that make the player invulnerable instead of sick.';
     De: 'Anteil der Krankheits-Items, die unverwundbar statt krank machen.';
     Fr: 'Part des objets de maladie qui rendent invulnérable au lieu de rendre malade.'),
    (En: 'Min. invulnerable time (ms)'; De: 'Min. Unverwundbarkeit (ms)';
     Fr: 'Invulnérabilité min. (ms)'),
    (En: 'Shortest invulnerable time. The time is random between min. and max.';
     De: 'Kürzeste Zeit der Unverwundbarkeit. Die Zeit ist zufällig zwischen Min. und Max.';
     Fr: 'Durée d''invulnérabilité la plus courte. La durée est aléatoire entre min. et ' +
       'max.'),
    (En: 'Max. invulnerable time (ms)'; De: 'Max. Unverwundbarkeit (ms)';
     Fr: 'Invulnérabilité max. (ms)'),
    (En: 'Longest invulnerable time. The time is random between min. and max.';
     De: 'Längste Zeit der Unverwundbarkeit. Die Zeit ist zufällig zwischen Min. und Max.';
     Fr: 'Durée d''invulnérabilité la plus longue. La durée est aléatoire entre min. et ' +
       'max.'),
    (En: 'On: some disease items let the player walk through destructible bricks for a while.';
     De: 'An: Mit manchen Krankheits-Items läuft der Spieler eine Weile durch zerstörbare Steine.';
     Fr: 'Activé : certains objets de maladie permettent au joueur de traverser les ' +
       'briques destructibles un moment.'),
    (En: 'Some disease items let the player walk through destructible bricks for a random time (not during hurry).';
     De: 'Mit manchen Krankheits-Items läuft der Spieler eine zufällige Zeit durch zerstörbare Steine (nicht im Hurry).';
     Fr: 'Certains objets de maladie permettent au joueur de traverser les briques ' +
       'destructibles pendant une durée aléatoire (pas pendant le hurry).'),
    (En: 'Share of the disease items that let the player walk through bricks. Rolled after the positive disease.';
     De: 'Anteil der Krankheits-Items, mit denen der Spieler durch Steine läuft. Wird nach der positiven ' +

       'Krankheit ausgewürfelt.';
     Fr: 'Proportion des objets de maladie permettant de traverser les briques. Cette probabilité est calculée après ' +
       'la maladie positive.'),
    (En: 'Min. time (ms)'; De: 'Min. Zeit (ms)';
     Fr: 'Durée min. (ms)'),
    (En: 'Shortest time. The time is random between min. and max.'; De: 'Kürzeste Zeit. Die Zeit ist zufällig zwischen Min. und Max.';
     Fr: 'Durée la plus courte. La durée est aléatoire entre min. et max.'),
    (En: 'Max. time (ms)'; De: 'Max. Zeit (ms)';
     Fr: 'Durée max. (ms)'),
    (En: 'Longest time. The time is random between min. and max.'; De: 'Längste Zeit. Die Zeit ist zufällig zwischen Min. und Max.';
     Fr: 'Durée la plus longue. La durée est aléatoire entre min. et max.'),
    (En: 'See-through (%)'; De: 'Durchsichtig (%)';
     Fr: 'Transparence (%)'),
    (En: 'How see-through the player is drawn: 0 = normal, 100 = invisible.';
     De: 'Wie durchsichtig der Spieler gezeichnet wird: 0 = normal, 100 = unsichtbar.';
     Fr: 'Transparence du joueur : 0 = normal, 100 = invisible.'),
    (En: 'If the time ends inside a brick, the player can only walk out of it (through further bricks only ' +

      'while there is no free tile next to the player). No bombs can be placed inside a brick, and the ' +

      'player is hit when the brick explodes.';
     De: 'Endet die Zeit in einem Stein, kann der Spieler nur noch hinauslaufen (durch weitere Steine nur, ' +

       'solange neben ihm kein freies Feld ist). In einem Stein kann er keine Bomben legen, und er wird ' +

       'getroffen, wenn der Stein explodiert.';
     Fr: 'Si le temps s''écoule à l''intérieur d''une brique, le joueur ne peut plus qu''en ' +
       'sortir (à travers d''autres briques seulement tant qu''il n''y a pas de case libre ' +
       'à côté du joueur). Il ne peut pas poser de bombes dans une brique et il est ' +
       'touché quand la brique explose.'),
    (En: 'On: some disease items turn the screen of the player white for a while.';
     De: 'An: Manche Krankheits-Items machen den Bildschirm des Spielers eine Weile weiß.';
     Fr: 'Activé : certains objets de maladie rendent l''écran du joueur blanc un moment.'),
    (En: 'Some disease items turn the screen of the player white for a while.';
     De: 'Manche Krankheits-Items machen den Bildschirm des Spielers eine Weile weiß.';
     Fr: 'Certains objets de maladie rendent l''écran du joueur blanc un moment.'),
    (En: 'Share of the disease items that turn the screen white. Rolled after the positive effects.';
     De: 'Anteil der Krankheits-Items, die den Bildschirm weiß machen. Wird nach den positiven Effekten ausgewürfelt.';
     Fr: 'Part des objets de maladie qui rendent l''écran blanc. Tirée après les effets ' +
       'positifs.'),
    (En: 'Strength (%)'; De: 'Stärke (%)';
     Fr: 'Intensité (%)'),
    (En: 'How white the screen gets: 100 = nothing can be seen any more.';
     De: 'Wie weiß der Bildschirm wird: 100 = man sieht nichts mehr.';
     Fr: 'Blancheur de l''écran : 100 = on ne voit plus rien.'),
    (En: 'Duration (ms)'; De: 'Dauer (ms)';
     Fr: 'Durée (ms)'),
    (En: 'How long the screen stays white.'; De: 'Wie lange der Bildschirm weiß bleibt.';
     Fr: 'Durée pendant laquelle l''écran reste blanc.'),
    (En: 'Fade back (ms)'; De: 'Ausblenden (ms)';
     Fr: 'Fondu de retour (ms)'),
    (En: 'How long it takes until the screen is normal again.'; De: 'Wie lange es dauert, bis der Bildschirm wieder normal ist.';
     Fr: 'Temps nécessaire pour que l''écran redevienne normal.'),
    (En: 'AI players are not affected. If two players play on one PC, that PC shows no white screen, because ' +

      'both share the screen.';
     De: 'KI-Spieler sind nicht betroffen. Spielen zwei Spieler an einem PC, zeigt dieser PC keinen weißen ' +

       'Bildschirm, weil sich beide den Bildschirm teilen.';
     Fr: 'Les joueurs IA ne sont pas concernés. Si deux joueurs jouent sur un PC, ce PC ' +
       'n''affiche pas d''écran blanc, car les deux partagent l''écran.'),
    (En: 'On: some items below the bricks are white bombs that turn the screens of all opponents white.';
     De: 'An: Manche Items unter den Steinen sind weiße Bomben, die die Bildschirme aller Gegner weiß machen.';
     Fr: 'Activé : certains objets sous les briques sont des bombes blanches qui rendent ' +
       'blancs les écrans de tous les adversaires.'),
    (En: 'A white bomb item: the next bomb explodes without flames and turns the screens of all opponents white.';
     De: 'Weiße Bombe: Die nächste Bombe explodiert ohne Flammen und macht die Bildschirme aller Gegner weiß.';
     Fr: 'Une bombe blanche : la prochaine bombe explose sans flammes et rend blancs les ' +
       'écrans de tous les adversaires.'),
    (En: 'Share of the items (%)'; De: 'Anteil der Items (%)';
     Fr: 'Part des objets (%)'),
    (En: 'Share of the items below the bricks that become a white bomb at the start of a round.';
     De: 'Anteil der Items unter den Steinen, die zu Rundenbeginn eine weiße Bombe werden.';
     Fr: 'Part des objets sous les briques qui deviennent une bombe blanche au début ' +
       'd''une manche.'),
    (En: 'How white the screens get: 100 = nothing can be seen any more.';
     De: 'Wie weiß die Bildschirme werden: 100 = man sieht nichts mehr.';
     Fr: 'Blancheur des écrans : 100 = on ne voit plus rien.'),
    (En: 'How long the screens stay white.'; De: 'Wie lange die Bildschirme weiß bleiben.';
     Fr: 'Durée pendant laquelle les écrans restent blancs.'),
    (En: 'How long it takes until the screens are normal again.'; De: 'Wie lange es dauert, bis die Bildschirme wieder normal sind.';
     Fr: 'Temps nécessaire pour que les écrans redeviennent normaux.'),
    (En: 'Team mates and the player who placed it are not affected. The white bomb is an extra bomb, ' +

      'afterwards the player places normal bombs again.';
     De: 'Teamkollegen und wer sie gelegt hat sind nicht betroffen. Die weiße Bombe ist eine Extrabombe, ' +

       'danach legt der Spieler wieder normale Bomben.';
     Fr: 'Les coéquipiers et le joueur qui a posé la bombe ne sont pas affectés. La bombe ' +
       'blanche est une bombe supplémentaire ; ensuite, le joueur peut à nouveau poser des bombes ' +
       'normales.'),
    (En: 'On: some items below the bricks are flamethrowers, hold the second action to fire.';
     De: 'An: Manche Items unter den Steinen sind Flammenwerfer, zum Feuern die zweite Aktion gedrückt halten.';
     Fr: 'Activé : certains objets sous les briques sont des lance-flammes, maintenir la ' +
       'deuxième action pour tirer.'),
    (En: 'A flamethrower item: hold the second action to fire in the direction you look.';
     De: 'Flammenwerfer: Die zweite Aktion gedrückt halten, um in Blickrichtung zu feuern.';
     Fr: 'Lance-flammes : maintenez la deuxième action pour tirer dans la direction du ' +
       'regard.'),
    (En: 'Share of the items below the bricks that become a flamethrower at the start of a round.';
     De: 'Anteil der Items unter den Steinen, die zu Rundenbeginn ein Flammenwerfer werden.';
     Fr: 'Part des objets sous les briques qui deviennent un lance-flammes au début d''une ' +
       'manche.'),
    (En: 'Fuel per item (ms)'; De: 'Treibstoff pro Item (ms)';
     Fr: 'Carburant par objet (ms)'),
    (En: 'How long a player can fire with one item. Only used up while firing, more items add fuel.';
     De: 'Wie lange ein Spieler mit einem Item feuern kann. Wird nur beim Feuern verbraucht, weitere Items ' +

       'geben mehr Treibstoff.';
     Fr: 'Durée pendant laquelle un joueur peut tirer avec un objet. Consommé seulement ' +
       'en tirant, d''autres objets ajoutent du carburant.'),
    (En: 'Range (tiles)'; De: 'Reichweite (Felder)';
     Fr: 'Portée (cases)'),
    (En: 'How far the beam reaches. It stops at bricks and solid blocks.';
     De: 'Wie weit der Strahl reicht. Er endet an Steinen und festen Blöcken.';
     Fr: 'Distance atteinte par le jet. Il s''arrête aux briques et aux blocs solides.'),
    (En: 'Time to kill (ms)'; De: 'Zeit bis zum Treffer (ms)';
     Fr: 'Temps avant d''être touché (ms)'),
    (En: 'How long an opponent has to stay in the beam without a break until being hit.';
     De: 'Wie lange ein Gegner ohne Unterbrechung im Strahl stehen muss, bis er getroffen wird.';
     Fr: 'Durée pendant laquelle un adversaire doit rester dans le jet sans interruption ' +
       'avant d''être touché.'),
    (En: 'On: items in the beam burn at once, like with a flame. Off: the beam goes over them.';
     De: 'An: Items im Strahl verbrennen sofort, wie bei einer Flamme. Aus: Der Strahl geht über sie hinweg.';
     Fr: 'Activé : les objets dans le jet brûlent aussitôt, comme avec une flamme. ' +
       'Désactivé : le jet passe au-dessus.'),
    (En: 'Burn items in the beam'; De: 'Items im Strahl verbrennen';
     Fr: 'Brûler les objets dans le jet'),
    (En: 'The beam stops at bricks and ignites bombs. Team mates are not hurt. While the player has fuel, the ' +

      'second action does not trigger or punch bombs.';
     De: 'Der Strahl endet an Steinen und zündet Bomben. Teamkollegen werden nicht verletzt. Solange der ' +

       'Spieler Treibstoff hat, zündet oder boxt die zweite Aktion keine Bomben.';
     Fr: 'Le jet s''arrête aux briques et allume les bombes. Les coéquipiers ne sont pas ' +
       'blessés. Tant que le joueur a du carburant, la deuxième action ne déclenche ni ' +
       'ne frappe les bombes.'),
    (En: 'On: players survive several hits, set per player name in the handicap table.';
     De: 'An: Spieler überleben mehrere Treffer, einstellbar pro Spielername in der Handicap-Tabelle.';
     Fr: 'Activé : les joueurs survivent à plusieurs coups, réglable par nom de joueur ' +
       'dans le tableau de handicap.'),
    (En: 'Players survive several hits, set per player name. During hurry every hit is lethal.';
     De: 'Spieler überleben mehrere Treffer, einstellbar pro Spielername. Im Hurry ist jeder Treffer tödlich.';
     Fr: 'Les joueurs survivent à plusieurs coups, réglable par nom de joueur. Pendant le ' +
       'hurry, chaque coup est mortel.'),
    (En: 'Invulnerable after a hit (ms)'; De: 'Unverwundbar nach Treffer (ms)';
     Fr: 'Invulnérable après un coup (ms)'),
    (En: 'After a hit that is not lethal the player is invulnerable for this time, so the same flame does not hit twice.';
     De: 'Nach einem nicht tödlichen Treffer ist der Spieler so lange unverwundbar, damit die selbe Flamme ' +

       'nicht zweimal trifft.';
     Fr: 'Après un coup non mortel, le joueur est invulnérable pendant ce temps, afin que ' +
       'la même flamme ne touche pas deux fois.'),
    (En: 'Default hit points'; De: 'Standard-Trefferpunkte';
     Fr: 'Points de vie par défaut'),
    (En: 'Hit points of all players that are not in the handicap table.';
     De: 'Trefferpunkte aller Spieler, die nicht in der Handicap-Tabelle stehen.';
     Fr: 'Points de vie de tous les joueurs qui ne figurent pas dans le tableau de ' +
       'handicap.'),
    (En: 'Show the remaining hit points above each player (only if more than 1).';
     De: 'Die restlichen Trefferpunkte über jedem Spieler anzeigen (nur wenn mehr als 1).';
     Fr: 'Afficher les points de vie restants au-dessus de chaque joueur (seulement s''il ' +
       'y en a plus de 1).'),
    (En: 'Show hit points above the players'; De: 'Trefferpunkte über den Spielern anzeigen';
     Fr: 'Afficher les points de vie au-dessus des joueurs'),
    (En: 'Handicap per player:'; De: 'Handicap pro Spieler:';
     Fr: 'Handicap par joueur :'),
    (En: 'Hit points per player name, e.g. more for weaker players. Name~2 is the second player on the same PC.';
     De: 'Trefferpunkte pro Spielername, z.B. mehr für schwächere Spieler. Name~2 ist der zweite Spieler am selben PC.';
     Fr: 'Points de vie par nom de joueur, p. ex. plus pour les joueurs plus faibles. ' +
       'Nom~2 est le deuxième joueur sur le même PC.'),
    (En: 'Add a line.'; De: 'Eine Zeile hinzufügen.';
     Fr: 'Ajouter une ligne.'),
    (En: 'Delete the selected line.'; De: 'Die gewählte Zeile löschen.';
     Fr: 'Supprimer la ligne sélectionnée.'),
    (En: 'Name~2 = second player on the same PC.'; De: 'Name~2 = zweiter Spieler am selben PC.';
     Fr: 'Nom~2 = deuxième joueur sur le même PC.'),
    (En: 'Player name'; De: 'Spielername';
     Fr: 'Nom du joueur'),
    (En: 'On: dead players walk around the field as ghosts and can throw bombs to get back into the game.';
     De: 'An: Tote Spieler laufen als Geister über das Feld und können mit Bomben zurück ins Spiel kommen.';
     Fr: 'Activé : les joueurs morts se promènent sur le terrain en fantômes et peuvent ' +
       'lancer des bombes pour revenir dans la partie.'),
    (En: 'Dead players walk around the field as ghosts until hurry. A ghost bomb that hits an opponent brings ' +

      'the ghost back.';
     De: 'Tote Spieler laufen bis zum Hurry als Geister herum. Trifft ihre Bombe einen Gegner, kommen sie zurück.';
     Fr: 'Les joueurs morts se promènent en fantômes jusqu''au hurry. Si leur bombe touche ' +
       'un adversaire, ils reviennent.'),
    (En: 'Flame length of the ghost bomb'; De: 'Flammenlänge der Geisterbombe';
     Fr: 'Longueur de flamme de la bombe fantôme'),
    (En: 'Flame length of the bomb a ghost throws.'; De: 'Flammenlänge der Bombe, die ein Geist wirft.';
     Fr: 'Longueur de flamme de la bombe qu''un fantôme lance.'),
    (En: 'Invulnerable after return (ms)'; De: 'Unverwundbar nach Rückkehr (ms)';
     Fr: 'Invulnérable après le retour (ms)'),
    (En: 'A ghost that comes back is invulnerable for this time.'; De: 'Ein zurückgekehrter Geist ist so lange unverwundbar.';
     Fr: 'Un fantôme qui revient est invulnérable pendant ce temps.'),
    (En: 'Max. times ghost per round'; De: 'Max. Anzahl Geist pro Runde';
     Fr: 'Nb max. de fois fantôme par manche'),
    (En: 'How often a player can become a ghost per round, 0 = unlimited.';
     De: 'Wie oft ein Spieler pro Runde zum Geist werden kann, 0 = unbegrenzt.';
     Fr: 'Combien de fois un joueur peut devenir fantôme par manche, 0 = illimité.'),
    (En: 'Max. time as ghost (ms)'; De: 'Max. Zeit als Geist (ms)';
     Fr: 'Durée max. en fantôme (ms)'),
    (En: 'A ghost that hits no opponent within this time vanishes, 0 = unlimited.';
     De: 'Ein Geist, der in dieser Zeit keinen Gegner trifft, verschwindet, 0 = unbegrenzt.';
     Fr: 'Un fantôme qui ne touche aucun adversaire dans ce délai disparaît, 0 = illimité.'),
    (En: 'Flicker time (ms)'; De: 'Flackerzeit (ms)';
     Fr: 'Durée de clignotement (ms)'),
    (En: 'Ghosts are visible for 3 ticks and invisible for 1 tick, this is the length of a tick. 0 = no flicker.';
     De: 'Geister sind 3 Takte sichtbar und 1 Takt unsichtbar, das ist die Länge eines Takts. 0 = kein Flackern.';
     Fr: 'Les fantômes sont visibles pendant 3 ticks et invisibles pendant 1 tick ; cette valeur correspond à la durée d''un ' +
       'tick. 0 = aucun clignotement.'),
    (En: '0 = unlimited, flicker time 0 = no flicker. In the last 5 s before a ghost vanishes it flickers faster.';
     De: '0 = unbegrenzt, Flackerzeit 0 = kein Flackern. In den letzten 5 s vor dem Verschwinden flackert ein ' +

       'Geist schneller.';
     Fr: '0 = illimité, durée de clignotement 0 = pas de clignotement. Dans les 5 ' +
       'dernières secondes avant sa disparition, un fantôme clignote plus vite.'),
    (En: 'On: until the first step of a round every player can throw the bomb below the player like with the glove.';
     De: 'An: Bis zum ersten Schritt einer Runde kann jeder Spieler die Bombe unter sich werfen wie mit dem Handschuh.';
     Fr: 'Activé : jusqu''au premier pas d''une manche, chaque joueur peut lancer la bombe ' +
       'sous lui comme avec le gant.'),
    (En: 'Until the first step of a round every player can throw the bomb below the player like with the glove ' +

      '(press the first action twice). Pressing against a wall only turns, so you can aim before throwing.';
     De: 'Bis zum ersten Schritt einer Runde kann jeder Spieler die Bombe unter sich werfen wie mit dem ' +

       'Handschuh (erste Aktion zweimal drücken). Gegen eine Wand drücken dreht nur, so kann man vor dem ' +

       'Wurf zielen.';
     Fr: 'Jusqu''au premier déplacement d''une manche, chaque joueur peut lancer la bombe depuis sa case ' +
       'comme avec le gant (appuyer deux fois sur la première action). Appuyer contre ' +
       'un mur ne fait que tourner, on peut donc viser avant de lancer.'),
    (En: 'On: players can walk over the border and come out on the opposite side.';
     De: 'An: Spieler können über den Rand laufen und kommen auf der gegenüberliegenden Seite heraus.';
     Fr: 'Activé : les joueurs peuvent franchir le bord et ressortir du côté opposé.'),
    (En: 'Players can walk over the border of the field and come out on the opposite side.';
     De: 'Spieler können über den Rand des Felds laufen und kommen auf der Gegenseite heraus.';
     Fr: 'Les joueurs peuvent franchir le bord du terrain et ressortir du côté opposé.'),
    (En: 'Players walk from the top border to the bottom border and back.'; De: 'Spieler laufen vom oberen zum unteren Rand und zurück.';
     Fr: 'Les joueurs vont du bord supérieur au bord inférieur et inversement.'),
    (En: 'Top <-> bottom'; De: 'Oben <-> unten';
     Fr: 'Haut <-> bas'),
    (En: 'Players walk from the left border to the right border and back.'; De: 'Spieler laufen vom linken zum rechten Rand und zurück.';
     Fr: 'Les joueurs vont du bord gauche au bord droit et inversement.'),
    (En: 'Left <-> right'; De: 'Links <-> rechts';
     Fr: 'Gauche <-> droite'),
    (En: 'Open after (s)'; De: 'Offen nach (s)';
     Fr: 'Ouvert après (s)'),
    (En: 'Seconds after the start of a round until the borders open, so nobody flees from the start corner at ' +

      'once. 0 = from the start.';
     De: 'Sekunden nach Rundenbeginn, bis die Ränder offen sind, damit niemand sofort aus der Startecke ' +

       'flieht. 0 = ab Start.';
     Fr: 'Nombre de secondes après le début d''une manche avant l''ouverture des bords, afin que ' +
       'personne ne fuie aussitôt le coin de départ. 0 = dès le début.'),
    (En: 'Arrows on the frame show the open borders, they blink 3 s before the borders open. If the tile on ' +

      'the opposite side is blocked (brick, bomb), the border works like a wall. Only players go over the ' +

      'border.';
     De: 'Pfeile am Rahmen zeigen die offenen Ränder, 3 s vor dem Öffnen blinken sie. Ist das Feld auf der ' +

       'Gegenseite belegt (Stein, Bombe), wirkt der Rand wie eine Wand. Nur Spieler gehen über den Rand.';
     Fr: 'Des flèches sur le cadre montrent les bords ouverts, elles clignotent 3 s avant ' +
       'l''ouverture. Si la case du côté opposé est occupée (brique, bombe), le bord ' +
       'agit comme un mur. Seuls les joueurs passent le bord.'),
    (En: 'On: teleporters in the middle of the borders bring the player to the opposite border.';
     De: 'An: Teleporter in der Mitte der Ränder bringen den Spieler zum gegenüberliegenden Rand.';
     Fr: 'Activé : des téléporteurs au milieu des bords amènent le joueur au bord opposé.'),
    (En: 'Teleporters in the middle of the borders bring the player to the opposite border (not during hurry).';
     De: 'Teleporter in der Mitte der Ränder bringen den Spieler zum gegenüberliegenden Rand (nicht im Hurry).';
     Fr: 'Des téléporteurs au milieu des bords amènent le joueur au bord opposé (pas ' +
       'pendant le hurry).'),
    (En: 'Teleporters in the middle of the top and the bottom border.'; De: 'Teleporter in der Mitte des oberen und des unteren Rands.';
     Fr: 'Téléporteurs au milieu du bord supérieur et du bord inférieur.'),
    (En: 'Teleporters in the middle of the left and the right border.'; De: 'Teleporter in der Mitte des linken und des rechten Rands.';
     Fr: 'Téléporteurs au milieu du bord gauche et du bord droit.'),
    (En: 'Appear after (s)'; De: 'Erscheinen nach (s)';
     Fr: 'Apparition après (s)'),
    (En: 'Seconds after the start of a round until the teleporters appear and work, 0 = from the start.';
     De: 'Sekunden nach Rundenbeginn, bis die Teleporter erscheinen und funktionieren, 0 = ab Start.';
     Fr: 'Secondes après le début d''une manche avant que les téléporteurs apparaissent et ' +
       'fonctionnent, 0 = dès le début.'),
    (En: 'The teleporter tiles and the tiles next to them stay free of bricks from the start. No bombs can be ' +

      'placed on a teleporter, rolling bombs stop in front of it.';
     De: 'Die Teleporterfelder und die Felder daneben bleiben von Anfang an frei von Steinen. Auf einem ' +

       'Teleporter kann man keine Bomben legen, rollende Bomben halten davor an.';
     Fr: 'Les cases des téléporteurs et leurs voisines restent libres de briques dès ' +
       'le début. On ne peut pas poser de bombes sur un téléporteur, les bombes qui ' +
       'roulent s''arrêtent devant.'),
    (En: 'On: test keys F1..F12 and Ins for the host and some test settings. Shows a hint to all players.';
     De: 'An: Testtasten F1..F12 und Einfg für den Host und einige Testeinstellungen. Zeigt allen Spielern ' +

       'einen Hinweis.';
     Fr: 'Activé : touches de test F1..F12 et Inser pour l''hôte et quelques réglages de ' +
       'test. Affiche un avertissement à tous les joueurs.'),
    (En: 'Only for testing, shows a hint to all players. In game F3 shows the test keys (only for the master).';
     De: 'Nur zum Testen, zeigt allen Spielern einen Hinweis. Im Spiel zeigt F3 die Testtasten (nur für den Master).';
     Fr: 'Uniquement pour les tests, affiche un avertissement à tous les joueurs. Dans le ' +
       'jeu, F3 affiche les touches de test (seulement pour le maître).'),
    (En: 'Start items'; De: 'Start-Items';
     Fr: 'Objets de départ'),
    (En: 'Items every player gets at round start, comma separated, *n = n times. Possible: bomb, flame, ' +

      'goldflame, speed, kick, punch, grab, spooger, jelly, trigger.';
     De: 'Items, die jeder Spieler zu Rundenbeginn bekommt, mit Komma getrennt, *n = n mal. Möglich: bomb, ' +

       'flame, goldflame, speed, kick, punch, grab, spooger, jelly, trigger.';
     Fr: 'Objets que chaque joueur reçoit au début d''une manche, séparés par des ' +
       'virgules, *n = n fois. Possibles : bomb, flame, goldflame, speed, kick, punch, ' +
       'grab, spooger, jelly, trigger.'),
    (En: 'e.g. kick,punch,grab,bomb*3'; De: 'z.B. kick,punch,grab,bomb*3';
     Fr: 'p. ex. kick,punch,grab,bomb*3'),
    (En: 'Round time (s, 0 = normal)'; De: 'Rundenzeit (s, 0 = normal)';
     Fr: 'Durée de manche (s, 0 = normale)'),
    (En: 'Round time in seconds, hurry starts at 80 s remaining. 0 = time from the game settings.';
     De: 'Rundenzeit in Sekunden, der Hurry beginnt 80 s vor Schluss. 0 = Zeit aus den Spieleinstellungen.';
     Fr: 'Durée de la manche en secondes, le hurry commence à 80 s de la fin. 0 = durée ' +
       'des réglages du jeu.'),
    (En: 'Invulnerable at start (ms)'; De: 'Unverwundbar beim Start (ms)';
     Fr: 'Invulnérable au départ (ms)'),
    (En: 'All players are invulnerable for this time at round start.'; De: 'Alle Spieler sind zu Rundenbeginn so lange unverwundbar.';
     Fr: 'Tous les joueurs sont invulnérables pendant ce temps au début d''une manche.'),
    (En: 'Ghosts count as alive, so the round does not end when only one player is left. Allows testing ghosts ' +

      'with 2 players.';
     De: 'Geister zählen als lebendig, die Runde endet also nicht, wenn nur noch ein Spieler übrig ist. So ' +

       'lassen sich Geister mit 2 Spielern testen.';
     Fr: 'Les fantômes comptent comme vivants, la manche ne se termine donc pas quand il ' +
       'ne reste qu''un joueur. Permet de tester les fantômes à 2 joueurs.'),
    (En: 'Ghosts count as alive (test ghosts with 2 players)'; De: 'Geister zählen als lebendig (Geister mit 2 Spielern testen)';
     Fr: 'Les fantômes comptent comme vivants (tester les fantômes à 2 joueurs)'),
    (En: 'Changes apply to the next game started on this PC.';
     De: 'Änderungen gelten für das nächste Spiel, das auf diesem PC gestartet wird.';
     Fr: 'Les modifications s''appliquent à la prochaine partie lancée sur ce PC.'),
    (En: 'Open the mods manual (MODS_MANUAL.md, with a link to the German version).';
     De: 'Die Anleitung der Mods öffnen (MODS_MANUAL_DE.md).';
     Fr: 'Ouvrir le manuel des mods (MODS_MANUAL.md, en anglais).'),
    (En: 'Mods manual'; De: 'Mods-Anleitung';
     Fr: 'Manuel des mods'),
    (En: 'Reset all mods to their default values (all off). Saved only with OK.';
     De: 'Alle Mods auf die Standardwerte zurücksetzen (alle aus). Gespeichert wird erst mit OK.';
     Fr: 'Remettre tous les mods à leurs valeurs par défaut (tous désactivés). Enregistré ' +
       'seulement avec OK.'),
    (En: '&Defaults'; De: '&Standard';
     Fr: '&Par défaut'),
    (En: 'Save the settings to atomic_server.ini. They apply to the next game hosted on this PC.';
     De: 'Die Einstellungen in atomic_server.ini speichern. Sie gelten für das nächste Spiel, das auf diesem ' +

       'PC gehostet wird.';
     Fr: 'Enregistrer les réglages dans atomic_server.ini. Ils s''appliquent à la ' +
       'prochaine partie hébergée sur ce PC.'),
    (En: 'Close without saving.'; De: 'Schließen, ohne zu speichern.';
     Fr: 'Fermer sans enregistrer.'),
    (En: 'Error, could not write:'; De: 'Fehler, kann nicht schreiben:';
     Fr: 'Erreur, écriture impossible :'),
    (En: 'Error, max. stun time is smaller than min. stun time.'; De: 'Fehler, die max. Stun-Zeit ist kleiner als die min. Stun-Zeit.';
     Fr: 'Erreur, la durée max. d''étourdissement est inférieure à la durée min.'),
    (En: 'Error, max. time is smaller than min. time.'; De: 'Fehler, die max. Zeit ist kleiner als die min. Zeit.';
     Fr: 'Erreur, la durée max. est inférieure à la durée min.'),
    (En: 'Error, invalid handicap in line %d:'; De: 'Fehler, ungültiges Handicap in Zeile %d:';
     Fr: 'Erreur, handicap invalide à la ligne %d :'),
    (En: 'The player name needs to be a valid name (optional with ~2 at the end) and the hit points need to be in 1..%d.';
     De: 'Der Spielername muss ein gültiger Name sein (optional mit ~2 am Ende) und die Trefferpunkte müssen ' +

       'in 1..%d liegen.';
     Fr: 'Le nom du joueur doit être un nom valide (avec ~2 facultatif à la fin) et les ' +
       'points de vie doivent être compris dans 1..%d.'),
    (En: 'Error, the player name "%s" is used twice.'; De: 'Fehler, der Spielername "%s" kommt zweimal vor.';
     Fr: 'Erreur, le nom de joueur « %s » apparaît deux fois.'),
    // Spiel Optionen (unit5)
    (En: 'Game options'; De: 'Spieloptionen';
     Fr: 'Options du jeu'),
    (En: 'Players play in two teams (red and white).'; De: 'Die Spieler spielen in zwei Teams (rot und weiß).';
     Fr: 'Les joueurs jouent en deux équipes (rouge et blanche).'),
    (En: 'Team play'; De: 'Teamspiel';
     Fr: 'Jeu en équipe'),
    (En: 'Players start at random start positions of the scheme.'; De: 'Die Spieler starten auf zufälligen Startpositionen des Schemas.';
     Fr: 'Les joueurs démarrent à des positions de départ aléatoires du schéma.'),
    (En: 'Random start positions'; De: 'Zufällige Startpositionen';
     Fr: 'Positions de départ aléatoires'),
    (En: 'If a network player loses the connection, an AI takes over.';
     De: 'Verliert ein Netzwerkspieler die Verbindung, übernimmt eine KI.';
     Fr: 'Si un joueur réseau perd la connexion, une IA prend le relais.'),
    (En: 'Lost net players revert to AI'; De: 'Getrennte Netzspieler werden zur KI';
     Fr: 'Les joueurs réseau déconnectés passent à l''IA'),
    (En: 'Play the sound effects (explosions, items, ...). The music is switched with "Play music" in the launcher.';
     De: 'Die Soundeffekte abspielen (Explosionen, Items, ...). Die Musik schaltet "Musik abspielen" im Launcher.';
     Fr: 'Jouer les effets sonores (explosions, objets, etc.). La musique se règle avec « ' +
       'Jouer la musique » dans le lanceur.'),
    (En: 'Play sound effects'; De: 'Soundeffekte abspielen';
     Fr: 'Jouer les effets sonores'),
    (En: 'Scheme'; De: 'Schema';
     Fr: 'Schéma'),
    (En: 'The scheme sets the bricks, the start positions and the items (files in data/schemes).';
     De: 'Das Schema legt die Steine, die Startpositionen und die Items fest (Dateien in data/schemes).';
     Fr: 'Le schéma définit les briques, les positions de départ et les objets (fichiers ' +
       'dans data/schemes).'),
    (En: 'Play time'; De: 'Spielzeit';
     Fr: 'Durée de jeu'),
    (En: 'Time of a round. The hurry starts 80 s before the end. Infinity = no time limit and no hurry.';
     De: 'Zeit einer Runde. Der Hurry beginnt 80 s vor Schluss. Unendlich = kein Zeitlimit und kein Hurry.';
     Fr: 'Durée d''une manche. Le hurry commence 80 s avant la fin. Infini = pas de limite ' +
       'de temps et pas de hurry.'),
    (En: 'Infinity'; De: 'Unendlich';
     Fr: 'Infini'),
    (En: 'Wins to win the match'; De: 'Siege für den Matchgewinn';
     Fr: 'Victoires pour gagner le match'),
    (En: 'How many rounds a player or team has to win to win the match.';
     De: 'Wie viele Runden ein Spieler oder Team gewinnen muss, um das Match zu gewinnen.';
     Fr: 'Nombre de manches qu''un joueur ou une équipe doit gagner pour remporter le ' +
       'match.'),
    (En: 'Conveyor speed'; De: 'Tempo der Förderbänder';
     Fr: 'Vitesse des tapis roulants'),
    (En: 'Speed of the conveyor belts on maps that have them.'; De: 'Tempo der Förderbänder auf Karten, die welche haben.';
     Fr: 'Vitesse des tapis roulants sur les cartes qui en comportent.'),
    (En: 'Slow'; De: 'Langsam';
     Fr: 'Lent'),
    (En: 'Middle'; De: 'Mittel';
     Fr: 'Moyen'),
    (En: 'Fast'; De: 'Schnell';
     Fr: 'Rapide'),
    (En: 'Network port'; De: 'Netzwerk-Port';
     Fr: 'Port réseau'),
    (En: 'TCP port of the server when hosting a game (default 5521).';
     De: 'TCP-Port des Servers, wenn ein Spiel gehostet wird (Standard 5521).';
     Fr: 'Port TCP du serveur lors de l''hébergement d''une partie (par défaut 5521).'),
    (En: 'Volume (%)'; De: 'Lautstärke (%)';
     Fr: 'Volume (%)'),
    (En: 'Sound volume, in the game it can also be changed with + and -.'; De: 'Lautstärke, im Spiel auch mit + und - änderbar.';
     Fr: 'Volume sonore, modifiable aussi dans le jeu avec + et -.'),
    (En: 'The same settings as in the options of the game. Field and wins can also be changed in the game when hosting.';
     De: 'Die selben Einstellungen wie in den Optionen im Spiel. Feld und Siege lassen sich beim Hosten auch ' +

       'im Spiel ändern.';
     Fr: 'Les mêmes réglages que dans les options du jeu. Le terrain et les victoires ' +
       'peuvent aussi être modifiés dans le jeu lors de l''hébergement.'),
    (En: 'Reset the settings to the defaults of the game. Saved only with OK.';
     De: 'Die Einstellungen auf die Standardwerte des Spiels zurücksetzen. Gespeichert wird erst mit OK.';
     Fr: 'Remettre les réglages aux valeurs par défaut du jeu. Enregistré seulement avec ' +
       'OK.'),
    (En: 'Save the settings, they apply to the next start of the game.';
     De: 'Die Einstellungen speichern, sie gelten beim nächsten Start des Spiels.';
     Fr: 'Enregistrer les paramètres ; ils seront appliqués au prochain démarrage du jeu.')
    );

Function GetText(Const Translation: TTranslation; Lang: TLanguage): String;
Begin
  Case Lang Of
    lgGerman: result := Translation.De;
    lgFrench: result := Translation.Fr;
  Else
    result := Translation.En;
  End;
End;

Function Tr(Const Text: String): String;
Var
  i: integer;
Begin
  result := Text;
  If Text = '' Then exit;
  For i := low(Translations) To high(Translations) Do Begin
    If (Translations[i].En = Text) Or (Translations[i].De = Text) Or (Translations[i].Fr = Text) Then Begin
      result := GetText(Translations[i], Language);
      exit;
    End;
  End;
End;

Function CodeToLanguage(Const Code: String): TLanguage;
Var
  l: TLanguage;
Begin
  result := lgEnglish;
  For l := low(TLanguage) To high(TLanguage) Do Begin
    If LanguageCodes[l] = Code Then Begin
      result := l;
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
