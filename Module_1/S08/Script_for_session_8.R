
# Графіки в «base» та «ggplot2» та спеціалізованих пакетах («corrplot», «pROC» тощо):
# діаграми
# таблиці кореляцій
# трендові лінії
# біплоти
# площини відповіді
# дендрограми
# крива операторної характеристики приймача (ROC)
# на сесії будуть використані пакети:
# "DescTools",
# "ggplot2",
# "corrplot",
# "RColorBrewer",
# "minpack.lm",
# "pcaMethods" (з Bioconductor),
# "ggfortify",
# "ggpubr",
# "ggrepel",
# "fields",
# "msa",
# "seqinr",
# "fields",
# "pROC"

library(DescTools)

# завантаження даних з буферу обміну; команда має бути не Run, а Copy

diagram <- read.DIF("clipboard", header = FALSE, transpose = TRUE)

# формуємо таблицю з даними у «довгому» форматі. Має бути три
# колонки – «groups», «sex» і «values»

group_names <- c("Cont", "Arg", "EtOH", "Arg_EtOH")
sex_names <- c("Males", "Females")

values <- diagram |> unlist() |> unname() |> as.numeric()

mat <- matrix(values, nrow(diagram), ncol(diagram))

c(group_names, "empty", group_names) -> colnames(mat)

df <- as.data.frame(mat)

groups <- rep(names(df), each = nrow(df))

DF <- data.frame(groups, values)

DF <- DF[- (as.numeric(rownames(subset(DF, groups == "empty")))),]

sex <- rep(sex_names, each = 20)

DF <- na.omit(data.frame(groups = DF$groups, sex, values = DF$values))

c(1:length(DF$groups)) -> rownames(DF)

DF$groups <- factor(DF$group, levels = group_names)

DF$sex <- factor(DF$sex, levels = sex_names)

se = function(x) sd(x)/sqrt(length(x))

# рахуємо середні значення та похибки, які знадобляться для рисунку

stat <- setNames(aggregate(values ~ groups*sex, FUN = function(x)
                                                    c(mean = round(mean(x), 0),
                                                      SEM = round(se(x), 1),
                                                      n = round(length(x), 0)), data = DF),
                                                      c("groups", "sex", ""))

# формуємо вектори середніх та похибок

means <- stat[,3][,1]
sem <- stat[,3][,2]

# max – максимальне значення на осі ординат,
# yticks – положення та кількість позначок на осі ординат,
# ym – середнє мінус стандартна похибка,
# yp – середнє плюс стандартна похибка,
# bp.mx – матриця значень для рисунку,
# dimnames – підписи груп для легенди

max <- max(means) + max(sem) + round(max(means)/5, 0)
yticks <- round(max/4.5, -1)
ym <- means + (-sem)
yp <- means + sem
bp.mx <- matrix(means, 4, 2, byrow = FALSE,
dimnames = list(c("Control", "Arg", "EtOH", "Arg+EtOH"),
                c("Males", "Females")))

# bwd – ширина стовпчика, btwb – проміжок між стовпчиками
# btwgb – проміжок між групами стовпчиків
# allbs – вектор проміжків між стовпчиками для групи
# space – вектор проміжків між стовпчиками для всього рисунку
# xmax – максимальне значення по осі абсцис
# ftick – координата першої позначки на осі абсцис
# ftick_seq – вектор координат для першої групи стовпчиків
# stick_seq – вектор координат для всіх стовпчиків
# xticks – координати всіх позначок на осі абсцис

bwd = 0.85
btwb = 0.1
btwgb = 1.75
allbs = rep(btwb, 3)
space = c(1, allbs, btwgb, allbs)
xmax <- bwd + 4*bwd + sum(allbs)*bwd +
                                           btwgb*bwd +
              4*bwd + sum(allbs)*bwd + bwd
ftick <- bwd + bwd/2
inc <- bwd + btwb*bwd
ftick_seq <- seq(ftick, 5*inc, inc)
stick_seq <- seq(ftick_seq[4] + btwgb*bwd + bwd, xmax + (-bwd), inc)
xticks <- c(ftick_seq, stick_seq)

# вектор кольорів

col = c("white", "gray75", "gray45", "black")

# власне, рисунок (пояснення в сесії 7)

par(pty = "s", mar = c(4.5, 7.15, 4, 8), lwd = 2.5)
barplot(bp.mx, beside = TRUE, col = col,
axes = FALSE, xaxs = "i", xaxt = "n", ylim = c(0, max),
xlim = c(0, xmax), width = rep(bwd, 8),
space = space, xlab = "", ylab = "")
axis(1, at = xmax/2, labels = FALSE, lwd = 3)
axis(2, at = seq(0, max, yticks), lwd = 3, las = 1, cex.axis = 2.25)
box(lwd = 3)
mtext(sex_names, side = 1, line = 1.5, cex = 2.25, font = 2,
at = c(xmax/4, 3*xmax/4))
mtext("GST activity, mU/mg protein", side = 2, line = 5.25, cex = 2.25, font = 2)
mtext("a", side = 3, line = 1.1, cex = 4, font = 2, adj = 0)
arrows(xticks, ym, xticks, yp, length = 0.08, angle = 90,
       code = 3, lwd = 2.5, col = "black")
legend(10.75, max/2 + 0.23*max, rownames(bp.mx), pch = 22, pt.bg = col,
       cex = 1.85, text.font = 2, text.width = 0.9, pt.cex = 3.5, bty = "n", xpd = NA)

# той самий рисунок в ggplot2

library(ggplot2)

# середні та похибки у форматі long

bp.long <- data.frame(groups = factor(rep(rownames(bp.mx), times = 2),
                                 levels = rownames(bp.mx)),
                      sex = factor(rep(colnames(bp.mx), each = 4),
                                 levels = colnames(bp.mx)),
                      means = stat[,3][,1],
                      sem = stat[,3][,2])

p <- ggplot(bp.long, aes(fill = groups, x = sex, y = means)) +
            geom_bar(position = position_dodge(0.8),
                     stat = "identity", width = 0.65,
                     color = "black", linewidth = 1.4) +
                     scale_fill_manual(values = col) +
            labs(x = "", y = "GST activity, mU/mg protein", title = "a", fill = "") +
            scale_y_continuous(expand = c(0, 0), limits = c(0, max),
                               breaks = seq(0, max, yticks)) +
            geom_errorbar(aes(ymin = means - sem, ymax = means + sem),
                          position = position_dodge(0.8), width = 0.25,
                          linewidth = 1.4)


# налаштування вигляду рисунка

app <- theme(aspect.ratio = 1,
             panel.border = element_rect(fill = NA,
                            colour = "black", linewidth = 1.75),
             axis.line = element_line(linewidth = 1.4),
             axis.text.x = element_text(colour = "black", size = 27, face = "bold",
             margin = margin(t = 9, r = 0, b = 0, l = 0)),
             axis.text.y = element_text(colour = "black", size = 30,
             margin = margin(t = 0, r = 6, b = 0, l = 10)),
             axis.title.y = element_text(colour = "black", size = 27, face = "bold",
             margin = margin(t = 0, r = 6, b = 0, l = 0)),
             axis.ticks = element_line(colour = "black", linewidth = 1.4),
             axis.ticks.length = unit(0.7, "lines"),
             panel.background = element_rect(fill = NA),
             panel.grid.minor = element_blank(),
             panel.grid.major = element_blank(),
             plot.background = element_blank(),
             plot.margin = margin(t = 0, r = 0, b = 0, l = 10, unit = "lines"),
             plot.title = element_text(colour = "black", size = 54, face = "bold"),
             legend.background = element_blank(),
             legend.key = element_rect(fill = NA, colour = "white"),
             legend.key.size = unit(1.6, "lines"),
             legend.text = element_text(size = 27),
             legend.title = element_text(size = 22, face = "bold", hjust = 0.5),
             legend.key.spacing.y = unit(1.6, "lines"))

# таблиця кореляцій:

library(corrplot)
library(RColorBrewer)

# завантажуємо та оформлюємо дані:

corrCoV <- read.csv("correlation_data.csv", header = TRUE)

corrCoV$ParenchimeCT <- as.numeric(factor(corrCoV$ParenchimeCT))
R <- cor(corrCoV, method = "kendall")

corrCoV <- corrCoV[-1]

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
legend(-5, 16, ncol = 1, cex = 4, legend = "A", text.font = 2, bty = "n")

# апроксимація в base за допомогою пакету minpack.lm

library(minpack.lm)

appro <- read.csv("approximation_data.csv", header = TRUE)
mod <- subset(appro, cohort == "ModC", select = c(age, alive))
age <- mod$age
alive <- mod$alive
age <- c(0, age)
age <- age[-length(age)]

set.seed(1000)
gomp <- nlsLM(alive ~ 100*exp(R*(1 - exp(Q*age))/Q),
              data = mod, start = list(R = 0.001, Q = 0.1))
summary(gomp)

par(pty = "s", mar = c(6, 7, 1, 1))
plot(age, alive, pch = 21, bg = "limegreen", cex = 1.75,
     cex.axis = 1.85, las = 1, ylim = c(0, 105), xaxs = "i",
     xlab = "\n\nAge (days) ", ylab = "Percent alive",
     cex.lab = 2.25, font.lab = 2, lwd = 2, xpd = NA)
lines(age, fitted(gomp), lwd = 3, col = "blue")

# Аналіз головних компонентів у ggplot2:

suppressPackageStartupMessages(library(pcaMethods))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggfortify))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(ggrepel))

cortex <- read.csv("pca_data.csv", header = TRUE)

cortex.m <- subset(cortex, Sex == "male")
cortex.f <- subset(cortex, Sex == "female")

cortex.m[,1:23] <- lapply(cortex.m, as.numeric)
cortex.f[,1:23] <- lapply(cortex.f, as.numeric)

cortex_dummy.m <- cortex.m
cortex_dummy.m[is.na(cortex_dummy.m)] <- 77777
cortex_dummy.f <- cortex.f
cortex_dummy.f[is.na(cortex_dummy.f)] <- 77777

cortex.prcomp.m <- prcomp(cortex_dummy.m[,1:23], scale. = TRUE)
cortex.prcomp.f <- prcomp(cortex_dummy.f[,1:23], scale. = TRUE)

cortex.m.pca <- pca(cortex.m[,1:23], method = "nipals", nPcs = 23, scale = "vector")
cortex.f.pca <- pca(cortex.f[,1:23], method = "nipals", nPcs = 23, scale = "vector")

cortex.prcomp.m$x <- cortex.m.pca@scores
cortex.prcomp.m$rotation <- cortex.m.pca@loadings
cortex.prcomp.m$sdev <- cortex.m.pca@sDev
cortex.prcomp.m$scale <- cortex.m.pca@scale
cortex.prcomp.m$center <- cortex.m.pca@center

cortex.prcomp.f$x <- cortex.f.pca@scores
cortex.prcomp.f$rotation <- cortex.f.pca@loadings
cortex.prcomp.f$sdev <- cortex.f.pca@sDev
cortex.prcomp.f$scale <- cortex.f.pca@scale
cortex.prcomp.f$center <- cortex.f.pca@center

PCAloadings.m <- data.frame(Variables = rownames(cortex.prcomp.m$rotation), cortex.prcomp.m$rotation)
PCAloadings.f <- data.frame(Variables = rownames(cortex.prcomp.f$rotation), cortex.prcomp.f$rotation)

greys1 = c("gray40", "gray5", "gray60")
greys2 = c("white", "gray50", "gray5")
breaks = c("Y", "M", "O")
scalemod = c("Young", "Middle-aged", "Old")
scale.shape <- scale_shape_manual(name = "Regimen", values = c(21, 22),
labels = c(expression(italic("Ad libitum")), "EODF"))
scale.colour <- scale_color_manual(values = greys1)
scale.fill <- scale_fill_manual(name = "Age", values = greys2,
                                breaks = breaks, labels = scalemod)
guide1 <- guides(fill = guide_legend(override.aes = list(shape=21)))
guide2 <- guides(colour = "none")

ellipse.m <- stat_ellipse(aes(colour = cortex.m$Age,
             linetype = cortex.m$Age), linewidth = 1.25,
             type = "norm", level = 0.68, show.legend = FALSE)
ellipse.f <- stat_ellipse(aes(colour = cortex.f$Age,
             linetype = cortex.f$Age), linewidth = 1.25,
             type = "norm", level = 0.68, show.legend = FALSE)
points.m <- geom_point(aes(fill = cortex.m$Age,
            shape = cortex.m$Regimen), cex = 5, stroke = 1.2)
points.f <- geom_point(aes(fill = cortex.f$Age,
            shape = cortex.f$Regimen), cex = 5, stroke = 1.2)
label.m <- geom_label_repel(data = PCAloadings.m,
                            aes(label = Variables, x = (PC1), y = (PC2)),
                            fill = "white", color = "black", size = 5)
label.f <- geom_label_repel(data = PCAloadings.f,
                            aes(label = Variables, x = (PC1), y = (PC2)),
                            fill = "white", color = "black", size = 5)

appearance <- theme(
            aspect.ratio = 1,
            axis.line = element_blank(),
            axis.text.x = element_text(colour = "black", size = 18,
            margin = margin(t = 3, r = 0, b = 0, l = 0)),
            axis.text.y = element_text(colour = "black", size = 18,
            margin = margin(t = 0, r = 3, b = 0, l = 0)),
            axis.title.x = element_text(colour = "black", size = 20,
            margin = margin(t = 4, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(colour = "black", size = 20,
            margin = margin(t = 0, r = 2, b = 0, l = 0)),
            axis.ticks = element_line(colour = "black", size = 1.4),
            axis.ticks.length = unit(0.7, "lines"),
            panel.background = element_rect(fill = NA,
                               colour = "black", linewidth = 1.4),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.background = element_blank(),
            legend.background = element_blank(),
            legend.key = element_rect(fill = NA, colour = "white"),
            legend.key.size = unit(1.6, "lines"),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 18, face = "bold", hjust = 0))

P1 <- autoplot(cortex.prcomp.m, data = cortex.m)
P2 <- autoplot(cortex.prcomp.f, data = cortex.f)

p1 <- P1 + points.m + ellipse.m + scale.shape + scale.fill +
           scale.colour + guide1 + guide2 + label.m + appearance
p2 <- P2 + points.f + ellipse.f + scale.shape + scale.fill +
           scale.colour + guide1 + guide2 + label.f + appearance

ggarrange(p1, p2, labels = c("a", "b"), font.label = list(size = 48),
          hjust = c(-0.6, -0.35), vjust = 1.2, ncol = 2, nrow = 1,
          common.legend = TRUE)

# Візуалізація площини відповіді за допомогою пакету fields

suppressPackageStartupMessages(library(fields))

Hgl.w1118 <- read.csv("responseSurface_data.csv", header = TRUE)
Hgl.w1118 <- Hgl.w1118[-1]

Hgl.w1118.Tps <- Tps(Hgl.w1118[,1:2], Hgl.w1118[,3], lambda = 0.05)
Hgl.w1118.sfd <- predictSurface(Hgl.w1118.Tps, nx = 600, ny = 600)

image(Hgl.w1118.sfd, col = tim.colors(256),
xlab = expression(paste("Protein eaten,",~mu,"g")),
ylab = expression(paste("Carbohydrate eaten,",~~mu,"g")),
cex.lab = 1.75, xaxt = "n", yaxt = "n")
contour(Hgl.w1118.sfd, nlevels = 6, labcex = 0.75, add = TRUE)
axis(1, c(20, 30, 40, 50), cex.axis = 1.25, las = 1)
axis(2, c(210, 230, 250, 270), cex.axis = 1.25, las = 1)




# Візуалізація дендрограм

suppressPackageStartupMessages(library(ggdendro))
suppressPackageStartupMessages(library(dendextend))

data(mtcars)

dendf <- mtcars %>% scale %>% dist %>% hclust(method = "average") %>% as.dendrogram %>%
set("branches_k_color", value = c("blue", "red", "forestgreen"), k = 3) %>%
set("branches_lwd", 2) %>% set("labels_cex", 1)

dendf %>% plot(center = TRUE, horiz = TRUE, yaxt = "n")
mtext("MtCars", side = 3, cex = 2.5, font = 2)

suppressPackageStartupMessages(library(msa))
suppressPackageStartupMessages(library(seqinr))
suppressPackageStartupMessages(library(ape))

mySeqs <- readAAStringSet("stunted.txt")
myAln <- msa(mySeqs)
myAln2 <- msaConvert(myAln, type="seqinr::alignment")
tr <- nj(dist.alignment(myAln2, "identity"))
tr <- fastme.bal(dist.alignment(myAln2, "identity"))
plot(tr, main="Phylogenetic tree of ATP synthase epsilon subunit")
plot(tr, tip.col = c("firebrick", "forestgreen", "royalblue"),
                      main="Phylogenetic tree of ATP synthase epsilon subunit")

# візуалізація кривих ROC

appearance <- theme(
            aspect.ratio = 1,
            axis.line = element_blank(),
            axis.text.x = element_text(colour = "black", size = 25,
            margin = margin(t = 3, r = 0, b = 0, l = 0)),
            axis.text.y = element_text(colour = "black", size = 25,
            margin = margin(t = 0, r = 3, b = 0, l = 0)),
            axis.title.x = element_text(colour = "black", size = 30, face = "bold",
            margin = margin(t = 6, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(colour = "black", size = 30, face = "bold",
            margin = margin(t = 0, r = 5, b = 0, l = 0)),
            axis.ticks = element_line(colour = "black", linewidth = 1.4),
            axis.ticks.length = unit(0.7, "lines"),
            panel.background = element_rect(fill = NA, colour = "black", linewidth = 1.4),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.background = element_blank())

roc1 <- roc(corrCoV$Group, corrCoV$ParenchimeCT)
roc2 <- roc(corrCoV$Group, corrCoV$Ulcer)

p1 <- ggroc(roc1, size = 1.5) + labs(title = "", x = "Specificity", y = "Sensitivity") +
      geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), size = 1.5, color = "gray55",
      lineend = "round", linejoin = "round", linetype = "dashed") + appearance +
      theme(plot.margin = margin(2, 0, 5, 36, unit = "pt"))
p2 <- ggroc(roc2, size = 1.5) + labs(title = "", x = "Specificity", y = "") +
      geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), size = 1.5, color = "gray55",
      lineend = "round", linejoin = "round", linetype = "dashed") + appearance +
      theme(plot.margin = margin(6.5, 36, 10.5, 0, unit = "pt"),
      axis.title.y = element_text(colour = "white", size = 1),
      axis.text.y = element_text(colour = "white", size = 1))
ggarrange(p1, p2, labels = c("A", "B"), font.label = list(size = 72),
          hjust = c(-2.9, -1.45), vjust = 1.8, ncol = 2, nrow = 1)

# ЗАВДАННЯ

# Спробуйте відтворити скрипт із власними даними, які не обов’язково мають бути великими, але
# повинні мати ідентичну або схожу структуру.
# При малюванні задайте власні налаштування рисунків – кольори стовпчиків, похибок, зірочок,
# кольорову схему для таблиці кореляцій і площини відповіді (наприклад, topo.colors, cm.colors
# і тому подібне).