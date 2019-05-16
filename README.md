# Übung 3: Hoogle Sheets

Das hier ist die Vorlage für Aufgabenblatt 3.

## Haskell Server:

Die Quellen liegen unter `haskell-server`

Bauen mit:

```sh
stack setup # nur beim ersten mall
stack build # übersetzen
stack exec hoogle-sheets # server starten
```

## Scala Server:

Die Quellen liegen unter `scala-server`

Bauen und ausführen mit:

```sh
sbt run # oder sbt ~run für continuous build
```

Server Stoppen mit Enter

## Client

Wer am Client ewtas anpassen will kann die Dateien in `scala-client` anpassen (Nicht nötig für das lösen der Aufgabe)

Übersetzen von Scala nach JavaScript:

```sh
cd scala-client
sbt fastOptJS
```