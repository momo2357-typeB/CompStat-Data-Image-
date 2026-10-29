
getwd()

list.files("archive", recursive = TRUE)

# Dateipfade anpassen
datei1 <- "data/fashion-mnist_train1.csv"
datei2 <- "data/fashion-mnist_train2.csv"

# Beide Teile einlesen
data1 <- read.csv(datei1)
data2 <- read.csv(datei2)

# Dimensionen überprüfen
dim(data1)
dim(data2)

# Spaltennamen überprüfen
names(data1)[1:10]
names(data2)[1:10]

identical(names(data1), names(data2))

# Nur Sneaker und Ankle Boots behalten

shoes1 <- subset(data1, label %in% c(7, 9))

shoes2 <- subset(data2, label %in% c(7, 9))


dim(shoes1)
dim(shoes2)

table(shoes1$label)
table(shoes2$label)


# Beide Datensätze zusammenführen
shoes <- rbind(shoes1, shoes2)

# Dimension überprüfen
dim(shoes)

# Anzahl je Kategorie überprüfen
table(shoes$label)


# Gesamtanzahl fehlender Werte
sum(is.na(shoes))

# Prüfen, ob nur die Kategorien 7 und 9 vorhanden sind
unique(shoes$label)

