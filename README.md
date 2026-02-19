# Von Mäusen und Bäumen

Dieses Repository stellt die Grundlage unserer Prüfungsleistung mit dem Titel **"Of mice and trees: towards proper MI using CART"** im Kurs "Statistische Analyse unvollständiger Daten" im WiSe 2025/26 dar. 

Wir, das sind Simon Fehrenbach, Tom Goldschmidt und Alice Kar, sind angetreten, die Implementation des CART Algorithmus innerhalb der mice Pipeline um ein bootstrap resampling zu erweitern. Auf diese Weise wollen wir den P-Step approximieren, was in der aktuellen Version von mice.impute.cart nicht geschieht und daher bisher eine between-imputation-variance von 0 annimmt.

Das Herzstück unserer Arbeit stellt **mice.impute.cart_boot.R** (im src Ordner) dar. In diesem Skript befindet sich unsere mice-ready bootstrapped CART Implementation. Der restliche Code in diesem Repository befasst sich mit der Evaluation unserer Methode durch mehrere Monte Carlo Simulationen mit wechselnden Parametern.

## Zur Nutzung von KI

KI, explizit ChatGPT, wurde im Codingprozess als Troubleshooting/Debugging (e.g. Nachvollziehen von Fehlermeldungen, Korrektur nicht-funktionierender Codeschnipsel) und zur Erstellung von Testdatensätzen verwendet. Außerdem haben wir uns stellenweise geeignete Packages und Funktionen zur Lösung spezifischer Probleme vorschlagen lassen.
