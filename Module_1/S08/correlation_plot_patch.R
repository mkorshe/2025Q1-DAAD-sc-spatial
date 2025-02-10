
# Таблиця кореляцій:

library(corrplot)
library(RColorBrewer)

# завантажуємо та оформлюємо дані:

corrCoV <- read.csv("correlation_data.csv", header = TRUE)

corrCoV <- corrCoV[-1]
corrCoV$ParenchimeCT <- as.numeric(factor(corrCoV$ParenchimeCT))

R <- cor(corrCoV, method = "kendall")

# проводимо кореляційний аналіз

testRes <- cor.mtest(corrCoV, method = "kendall", conf.level = 0.95)

# формуємо рисунок

Nt <- corrplot(R, p.mat = testRes$p, method = "color",
               type = "upper", col = COL2("PuOr", 30),
               diag = FALSE, outline = "white", order = "FPC",
               addCoef.col = FALSE, tl.col = "black",
               sig.level = c(0.05), pch.cex = 1.5, pch.col = "grey60",
               insig = "label_sig", cl.length = 5, cl.cex = 1.5,
               cl.pos = "r", cl.offset = 0.7)

# визначаємо, де мають бути достовірності та колір «зірочок»

pointcol <- ifelse(Nt$corrPos[,5] < 0.53,
            ifelse(Nt$corrPos[,5] > -0.4, "black", "white"), "white")

# власне, рисунок

corrplot(R, p.mat = testRes$p, method = "color",
            type = "upper", col = COL2("PuOr", 30),
            diag = FALSE, outline = "white", order = "FPC",
            addCoef.col = FALSE, tl.col = "black",
            sig.level = c(0.05), pch.cex = 1.5, pch.col = pointcol,
            insig = "label_sig", tl.cex = 1.12,
            cl.ratio = 0.25, cl.length = 5, cl.cex = 1.35,
            cl.pos = "r", cl.offset = 0.25, cl.align.text = "l")
legend(-3, 10, ncol = 1, cex = 4, legend = "A", text.font = 2, bty = "n")


