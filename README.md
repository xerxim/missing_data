# Von Mäusen und Bäumen

Dieses Repository bildet die Grundlage unserer Prüfungsleistung **“Of mice and trees: towards proper MI using CART”** im Kurs Statistische Analyse unvollständiger Daten (WiSe 2025/26).

Wir – Simon Fehrenbach (), Tom Goldschmidt ([tom-moritz.goldschmidt@stud.uni-bamberg.de](mailto:tom-moritz.goldschmidt@stud.uni-bamberg.de)) und Alice Kar – erweitern darin die CART-Implementation innerhalb der mice-Pipeline um ein Bootstrap-Resampling. Ziel ist es, den fehlenden P-Step zu approximieren, der in der aktuellen Funktion `mice.impute.cart` nicht berücksichtigt wird und somit implizit eine Between-Imputation-Varianz von 0 annimmt.

Das Kernstück unserer Arbeit ist `mice.impute.cart_boot.R` (im src-Ordner). Dieses Skript enthält unsere mice-kompatible, gebootstrapte CART-Implementation.
Der übrige Code im Repository dient der Evaluation des Ansatzes anhand mehrerer Monte-Carlo-Simulationen mit variierenden Parametern.

## Projektstruktur

```
project-root/
│
├─ project/                         → Hauptordner
│  │
│  ├─ dev/                          → Experimentelle Vorarbeiten
│  │
│  ├─ dta/                          → Speicher für RData Objekte
│  │  ├─ single/                    → Daten aus einzelnen Schritten
│  │  └─ *.RData                    → Gesammte Daten eines Experiments
│  │
│  ├─ plots/                        → Diagramme
│  │  ├─ old/                       → Nicht aktuelle Diagramme
│  │  └─ *.pdf                      → Aktuelle Diagramme
│  │
│  ├─ src/                          → Funktionen
│  │  ├─ old/                       → Nicht aktuelle Funktionen
│  │  ├─ graph_functions.R          → Erstellen von Diagrammen
│  │  ├─ mc_helpers.R               → Hilfsfunktionen für mc_study
│  │  ├─ mc_study.R                 → Abstrakte Monte-Carlo-Simulation
│  │  └─ mice.impute.cart_boot.R    → Erweiterte mice Unterfunktion
│  │
│  ├─ tst/                          → Tests
│  │
│  ├─ bias_plots.R                  → Erstellung der Bias Digramme
│  ├─ coverage_plots.R              → Erstellung der Coverage Diagramme
│  ├─ cycle_study.R                 → Untersuchung nach idealer Iterationszahl
│  └─ main.R                        → Durchführung der Monte-Carlo-Simulationen
│
└─ *                                → Git Dateien
```

## Zur Nutzung von KI


KI, explizit ChatGPT, war ein zentrales Werkzeug in unserem Arbeitsprozess. Es wurde für Nachfragen zu MI-Theorie, als Code-Advisor und als Lektor für Texte verwendet. Ideen, die wir auf diese Weise bekommen haben, haben wir mit belastbaren Quellen (zB den Seminarmaterialien oder der Literatur die wir in den Referenzen angegeben haben) abgeglichen. Wir sind davon überzeugt, dass wir die Technologie auf ethische Weise verwendet haben: Während sie uns einiges an Basis-Arbeit abgenommen hat, haben wir nie Arbeitsprozesse komplett abgegeben oder Code- bzw Textabschnitte einfach kopiert, sondern Feedback zu einzelnen Schritten eingeholt und kritisch eingearbeitet. Im Endeffekt ist unser Projekt wahrscheinlich umfassender geworden, als wenn wir es ohne die Hilfe von KI umgesetzt hätten, jedoch wurden keine Entscheidungen getroffen, die wir nicht begründen können.

<!-- KI, explizit ChatGPT, war ein zentrales Werkzeug in unserem Arbeitsprozess und stellt in gewisser Weise das vierte Gruppenmitglied dar. Jede/r von uns hatte indivudelle Chatverläufe mit der Maschine, in denen sie Tutor für MI-Theorie, Code-Advisor und Lektor in einem war. Ideen, die wir auf diese Weise bekommen haben, haben wir mit belastbaren Quellen (zB den Seminarmaterialien oder der Literatur die wir in den Referenzen angegeben haben) abgeglichen. Wir sind davon überzeugt, dass wir die Technologie auf ethische Weise verwendet haben: Während sie uns einiges an Basis-Arbeit abgenommen hat, haben wir nie Arbeitsprozesse komplett abgegeben oder Code- bzw Textabschnitte einfach kopiert, sondern Feedback zu einzelnen Schritten eingeholt und kritisch eingearbeitet. Im Endeffekt ist unser Projekt wahrscheinlich umfassender geworden, als wenn wir es ohne die Hilfe von KI umgesetzt hätten, jedoch wurden keine Entscheidungen getroffen, die wir nicht begründen können. -->

<!-- KI, explizit ChatGPT, wurde im Codingprozess als Troubleshooting/Debugging (e.g. Nachvollziehen von Fehlermeldungen, Korrektur nicht-funktionierender Codeschnipsel) und zur Erstellung von Testdatensätzen verwendet. Außerdem haben wir uns stellenweise geeignete Packages und Funktionen zur Lösung spezifischer Probleme vorschlagen lassen. -->
