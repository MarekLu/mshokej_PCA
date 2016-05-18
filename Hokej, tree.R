# Hokej, clustering
# =================

library(data.table)
library(cluster)

d <- read.csv2("Corsi_Data_crosstab.csv", stringsAsFactors=FALSE, fileEncoding = "UTF-8")
d <- as.data.table(d)

d[, no := NULL]

d[, cf.prevaha := sub(",", ".", cf.prevaha)]
d[, cf.prevaha := as.numeric(sub("%", "", cf.prevaha)) / 100]

d[, rozdil.vs.tym := sub(",", ".", rozdil.vs.tym)]
d[, rozdil.vs.tym := as.numeric(sub("%", "", rozdil.vs.tym)) / 100]

d[, jak.casto.strili := sub(",", ".", jak.casto.strili)]
d[, jak.casto.strili := as.numeric(sub("%", "", jak.casto.strili)) / 100]


# Tree týmy ---------------------------------------------------------------

d.tym <- d[, c(2:11), with = FALSE]  # vybrané parametry
d.tym <- d.tym[, lapply(.SD, mean, na.rm = TRUE), by = "team"]
d.tym <- as.data.frame(d.tym)
rownames(d.tym) <- d.tym[, "team"]
tymy <- d.tym[, "team"]
d.tym$team <- NULL

d.tym <- scale(d.tym) 

hc.tym <- hclust(dist(d.tym), method = "average")
plot(hc.tym)


t(table(cutree(hc.tym, 5), tymy))


# Tree hráči --------------------------------------------------------------

d.player <- d[, c(1, 3:11), with = FALSE]  # vybrané parametry
d.player <- d.player[, lapply(.SD, mean, na.rm = TRUE), by = "player"]
d.player <- as.data.frame(d.player)
rownames(d.player) <- d.player[, "player"]
hraci <- d.player[, "player"]
d.player$player <- NULL

d.player <- scale(d.player) 

hc.player <- hclust(dist(d.player), method = "average")
plot(hc.player)


t(table(cutree(hc.player, 5), hraci))


# Kmeans týmy ------------------------------------------------------------------

k.tym <- d[, c(2:11), with = FALSE]  # vybrané parametry
k.tym <- k.tym[, lapply(.SD, mean, na.rm = TRUE), by = "team"]
k.tym <- as.data.frame(k.tym)
rownames(k.tym) <- k.tym[, "team"]
k.tym$team <- NULL

k.tym.x <- k.tym[, c(1, 2, 4, 5, 6, 8, 9)]

k.tym.x <- scale(k.tym.x) 

fit.tym <- kmeans(k.tym.x, 4, iter.max = 50)
k.tym.x <- data.frame(k.tym.x, fit.tym$cluster)

clusplot(k.tym.x, fit.tym$cluster, main = "MS v hokeji, týmy, kmeans clustering",
         color = TRUE, shade = FALSE, labels = 3, lines = 0)


# Kmeans CZE hráči ------------------------------------------------------------------

k.cz <- d[team == "CZE", c(1, 3:11), with = FALSE]
k.cz <- as.data.frame(k.cz)
rownames(k.cz) <- k.cz[, "player"]
k.cz$player <- NULL

k.cz.x <- scale(k.cz) 

fit.cz <- kmeans(k.cz.x, 4, iter.max = 100)
k.cz.x <- data.frame(k.cz.x, fit.cz$cluster)

clusplot(k.cz.x, fit.cz$cluster, main = "MS v hokeji, hráči CZE, kmeans clustering",
         color = TRUE, shade = FALSE, labels = 3, lines = 0)