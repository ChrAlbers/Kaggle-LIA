# Plots erzeugen, um für jede Dim einen Zusammenhang zwischen Response und der
# Variablen zu erkennen (Points für gute Features?)

setwd("d:/Projects/kaggle/Prudential_LIA/")

# Daten einlesen
trainraw = read.csv("train.csv")

# Die Daten beinhalten 128 Spalten, wobei die Response-Spalte das Ziel ist.
# Id sollte eigentlich irrelevant sein, aber man kann ja mal ID gegen Response
# plotten, vielleicht kommt da ja was bei rum

# Ich muss also 127 plots erstellen. Die werden als scatterplots ausgeführt,
# was "plot" als default hat. Dazu kommt ein linearer Fit an die Daten, so dass man
# einen Trend erkennen kann.

# Ich mach mal 3x3 plots pro Fenster
nameliste = names(trainraw)
nameliste = nameliste[nameliste != "Response"]

zaehler = 0
for (aussen in 1:14) {
    windows(width = 1600, height = 1000)
    par(mfrow = c(3, 3))
    for (gaga in 1:9) {
        zaehler = zaehler + 1
        plot(x = trainraw$Response, y = trainraw[, zaehler],
        xlab = "Response", ylab = nameliste[zaehler])
        model <- lm(trainraw[, zaehler] ~ trainraw$Response)
        abline(model, col = 2)    
    }
}
