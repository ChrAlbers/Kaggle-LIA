# Versuch der Auswertung der Lebensversicherungsdaten

# Parameterteil

setwd("d:/Projects/kaggle/Prudential_LIA/")

NA_Schwelle = 1
seeed = 1               # Seed für die Zufallszahlen setzen (Zur Unterscheidung: 3 mal e)
testgroesse = 0.9      # Größe des Testsets aus den Trainingsdaten
n = 1                   # Welche Response soll getestet werden in one-vs-all?

# Seed für Zufallszahlenstrom setzen
set.seed(seeed)

# Daten einlesen
trainraw = read.csv("train.csv")
# testraw = read.csv("test.csv")
num_obs = dim(trainraw)[1] # Anzahl Observationen

# Medical_Keyword_1-48 sollen dummies sein. Ich nehme sie mal raus
train = trainraw[, !grepl("^Medical_Keyword", names(trainraw))]

# Dann die Dinger mit zu viel NA rausnehmen:
train = train[, sapply(train, function(x) sum(is.na(x))) < NA_Schwelle]

# Den vollen Datensatz in test-Daten und train-Daten aufteilen
tt_breakup = sample(1:num_obs) < testgroesse*num_obs
test = train[tt_breakup, ]
train = train[!tt_breakup, ]

# Hier wird das Modell gefittet; One-vs-All, indem die Response-Spalte == 1 gesetzt wird
resp1model <- glm(Response == n ~ ., family = binomial(link = "logit"), data = train)


# Eine ROC-Kurve erzeuge, um die Güte des Modells zu testen
# Die False-Positive-Rate gegen True-Positive-Rate für verschiedene Werte der Schwelle plotten
schwell_vec = seq(0, 1, length = 21)
fpr_vec = rep(0, length(schwell_vec))
tpr_vec = rep(0, length(schwell_vec))

# Das Modell hernehmen, um die Testdaten zu fitten. Die Schwelle wird in der for-Schleife
# unten appliziert
fitted_results <- predict(resp1model, newdata = test, type = "response")
# Die vorgabe sind die wahren Responses
vorgabe = test$Response == n

zaehler = 0
for (schwelle in schwell_vec) {
    zaehler = zaehler + 1
    tp = sum(vorgabe*(fitted_results > schwelle))
    fn = sum(vorgabe*(!(fitted_results > schwelle)))
    fp = sum((!vorgabe)*(fitted_results > schwelle))
    tn = sum((!vorgabe)*(!(fitted_results > schwelle)))
    
    fpr_vec[zaehler] = fp/(fp + tn)
    tpr_vec[zaehler] = tp/(tp + fn)
}

windows()
plot(x = fpr_vec, y = tpr_vec, type = "l")
lines(x = c(0, 1), y = c(0, 1), col = 2)