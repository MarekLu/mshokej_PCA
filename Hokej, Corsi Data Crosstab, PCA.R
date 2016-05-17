# Hokej, Corsi Data Crosstab, Principal Component Analysis
# ========================================================

# Zdroj: http://www.thehockeyninja.com/data-ms2016/
# Stáhnout data Crosstab do souboru Corsi_Data_crosstab.csv
# Změnit oddělovat na středník (nakonec nebylo potřeba, problém byl jinde, ale už jsem to nechal)
# Změnit záhlaví na: player;team;no;corsi.plus;corsi.minus;corsi.plus.minus;cf.prevaha;rozdil.vs.tym;corsi.plus.game;corsi.minus.game;indiv.strely;jak.casto.strili

library(data.table)
library(FactoMineR)
library(factoextra)
library(PerformanceAnalytics)
library(ggplot2)

d <- read.csv2("Corsi_Data_crosstab.csv", stringsAsFactors=FALSE, fileEncoding = "UTF-8")
d <- as.data.table(d)

d[, no := NULL]

d[, cf.prevaha := sub(",", ".", cf.prevaha)]
d[, cf.prevaha := as.numeric(sub("%", "", cf.prevaha)) / 100]

d[, rozdil.vs.tym := sub(",", ".", rozdil.vs.tym)]
d[, rozdil.vs.tym := as.numeric(sub("%", "", rozdil.vs.tym)) / 100]

d[, jak.casto.strili := sub(",", ".", jak.casto.strili)]
d[, jak.casto.strili := as.numeric(sub("%", "", jak.casto.strili)) / 100]


# PCA týmy ---------------------------------------------------------------------

d.tym.pca <- d[, c(2:11), with = FALSE]  # vybrané parametry
d.tym.pca <- d.tym.pca[, lapply(.SD, mean, na.rm = TRUE), by = "team"]
d.tym.pca <- as.data.frame(d.tym.pca)

rownames(d.tym.pca) <- d.tym.pca[, "team"]
d.tym.pca$team <- NULL

d.tym.pca <- PCA(d.tym.pca, graph = FALSE)

fviz_pca_biplot(d.tym.pca, alpha.var = "contrib", geom = c("text", "point"), labelsize = 5, pointsize = 2, col.circle = "red") +
  theme_bw() +
  ggtitle("\nTýmy na MS v hokeji, analýza hlavních komponent\n") +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 18, hjust = 0)
  )

ggsave("PCA tymy.png", width = 15, height = 10, dpi = 75)


# PCA český tým ---------------------------------------------------------------------

d.cze.pca <- d[team == "CZE", c(1, 3:11), with = FALSE]  # vybrané parametry
# d.cze.pca <- d.cze.pca[, lapply(.SD, mean, na.rm = TRUE), by = "player"]
d.cze.pca <- as.data.frame(d.cze.pca)

rownames(d.cze.pca) <- d.cze.pca[, "player"]
d.cze.pca$player <- NULL

d.cze.pca <- PCA(d.cze.pca, graph = FALSE)

fviz_pca_biplot(d.cze.pca, alpha.var = "contrib", geom = c("text", "point"), labelsize = 5, pointsize = 2, col.circle = "red") +
  theme_bw() +
  ggtitle("\nČeský tým na MS v hokeji, analýza hlavních komponent\n") +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 18, hjust = 0)
  )

ggsave("PCA CZE tym.png", width = 15, height = 10, dpi = 75)