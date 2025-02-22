#Методи копіювання даних з електронних таблиць
#Завантаження даних, збережених у форматі з розділеними комами
#Обчислення середнього, медіани та дисперсії в базовому оточенні R
#Тестування на нормальність та рівність дисперсій
#Параметричні парні порівняння
#Аналіз дисперсії з використанням пакетів 'base', 'DescTools' та 'multcomp' та інших.
# Необхідні пакети

install.packages("DescTools")
install.packages("nortest")
install.packages("multcomp")
install.packages("readxl")

# завантаження даних з буферу обміну; команда має бути не Run, а Copy

A <- scan("clipboard") # копіюємо дані з електронної таблиці, після цього – Enter

# завантаження даних з електронних таблиць з буферу обміну; команда має бути не Run, а Copy
# якщо header = TRUE, то перший рядок читатиметься як назви стовпців
# transpose має бути TRUE для правильного прочитання даних з MS Excel; дивіться ?read.DIF

B <- read.DIF("clipboard", header = TRUE, transpose = TRUE)

# завантаження даних з електронних таблиць без заголовку з буферу обміну
# перший рядок не вважатиметься назвами стовпців

D <- read.DIF("clipboard", header = FALSE, transpose = TRUE)
D

# створюємо вектор values з даних таблиці D
# функція unname забирає імена з колонок, unlist – створює вектор зі значень колонок,
# as.numeric – перетворює створений вектор у вектор чисел, а порожні комірки – на NA

values <- as.numeric(unlist(unname(D)))

# створюємо матрицю і таблицю з даними для подальшого користування; таблицею D було б
# не зручно користуватись, оскільки дані не зберігались як числа через наявність порожніх
# комірок. D$V1 дає вектор текстових значень і т. п.

mat <- matrix(values, nrow(D), ncol(D))

df <- as.data.frame(mat)

# призначаємо літери A, B, C, D, E іменами колонок в таблиці df

LETTERS[1:5] -> colnames(df)

# створюємо вектор текстових значень groups, для перетворення даних таблиці D в long формат

groups <- rep(names(df), each = nrow(df))

# створюємо таблицю з даними DF в long форматі і паралельно видаляємо NA

DF <- na.omit(data.frame(groups, values))

# перевіряємо, чи все ОК

head(DF)

# перетворює groups у фактор; це полегшить низку подальших кроків

DF$groups <- factor(DF$groups)

# швидкий спосіб отримати середні значення для стовпчиків; використовуємо df:

colMeans(df, na.rm = TRUE)

# те саме можна зробити за допомогою команди lapply:

unlist(lapply(df, mean, na.rm = TRUE))

# альтернативний спосіб перетворити таблицю D в таблицю з даними, з якої можна робити піднабори # даних та розділяти на вектори чисел:

LETTERS[1:5] -> colnames(D)
uD <- unclass(D)

# розбиваємо таблицю на вектори з іменами літер A, B, C, D, E за допомогою циклу:

for (i in seq_along(uD)) {
assign(paste0(names(uD)[i]), as.vector(as.numeric(uD[[i]])))}

# створюємо таблицю з даними dff за допомогою команд cbind та as.data.frame. Надалі вона нам не # знадобиться

dff <- cbind(A, B, C, D, E)
dff <- as.data.frame(dff)

# альтернативний спосіб копіювання даних з таблиці MS Excel за допомогою функцій пакету readxl:

library(readxl)
datta <- read_xlsx("Data_for_Lecture_7.xlsx", range = "B3:F10", col_names = FALSE)

# бачимо, що в цьому випадку ми отримуємо tibble з готовими NA:

datta

# назви колонок – латинські літери:

LETTERS[1:5] -> colnames(datta)

datta
datta$A

# функція aggregate дозволяє створити табличку з середніми та стандартними відхиленнями:

aggregate(values ~ groups, FUN = function(x) c(mean = mean(x), sdev = sd(x)), data = DF)

# момент перфекціонізму: за допомогою функції setNames даємо нормальні назви колонкам:

setNames(aggregate(values ~ groups, FUN = function(x) c(mean = mean(x),
                                                        sdev = sd(x)),
                                                        data = DF), c("groups", ""))

# момент перфекціонізму: за допомогою функції round даємо нормальний вигляд числам:

setNames(aggregate(values ~ groups, FUN = function(x) c(mean = round(mean(x), 3),
                                                        sdev = round(sd(x), 3)),
                                                        data = DF), c("groups", ""))

# часто використовують стандартну похибку середнього як альтернативу стандартного відхилення
# (хоч насправді вона не є альтернативою). В більшості статистичних програм немає спеціальної
# функції для розрахунку стандартної похибки. Пишемо таку функцію:

se <- function(x) sd(x)/sqrt(length(x))

# тепер aggregate з нею:

setNames(aggregate(values ~ groups, FUN = function(x) c(mean = round(mean(x), 3),
                                                        SEM = round(se(x), 3)),
                                                        data = DF), c("groups", ""))
# тепер дуже серйозно:

means_se <- setNames(aggregate(values ~ groups, FUN = function(x)
                                                c(mean = round(mean(x), 3),
                                                  SEM = round(se(x), 3)),
                                                  data = DF), c("groups", ""))

# дані mean_se нам знадобляться пізніше. Тепер – по максимуму: allmeas – середнє (mean),
# дисперсія (var), стандартне відхилення (sd), стандартна похибка (se; ми самі придумали назву),
# медіана (median), перший і третій квартиль. Останні можна рахувати за допомогою функцій
# quantile(), boxplot.stats() та fivenum() (зараз цю останню не наводжу)

allmeas <- setNames(aggregate(values ~ groups, FUN = function(x)
                                                c(mean = round(mean(x), 3),
                                                  var = round(var(x), 3),
                                                  SD = round(sd(x), 3),
                                                  SEM = round(se(x), 3),
                                                  median = round(median(x), 3),
                                                  Q1 = round(boxplot.stats(x)$stats[2], 3),
                                                  Q3 = quantile(x, 0.75, na.rm = TRUE,
                                                                names = FALSE)),
                                                  data = DF), c("groups", ""))
allmeas

# тільки значення стандартних похибок:

allmeas[,2][,4]

# Інколи виникає потреба записати створену таблицю з даними для подальшого використання,
# особливо, якщо маємо великий масив даних, над яким треба працювати декілька днів поспіль.
# Можемо це зробити за допомогою функції write.csv(), і відкрити дані – за допомогою функції
# read.csv:

write.csv(dff, "dff.csv", row.names = FALSE)

fd <- read.csv("dff.csv")

fd

# Як перевірити дані на відповідність нормальному розподілу для подальшого вибору алгоритму
# статистичного аналізу? Робимо це за допомогою функції shapiro.test пакету base. Це є тест
# Шапіро-Вілка.

shapiro.test(A)

# Але нас найбльше цікавить значення p:

shapiro.test(A)$p.value

# Еволюція тесту Шапіро за допомогою aggregate:

setNames(aggregate(values ~ groups,
                                   FUN = function(x) c(shapiro = shapiro.test(x)$p.value),
                                   data = DF),
                                   c("groups", "shapiro"))

setNames(aggregate(values ~ groups,
                                   FUN = function(x)
                                   c(shapiro = round(shapiro.test(x)$p.value, 4)),
                                   data = DF),
                                   c("groups", "shapiro"))

# Нарешті, завантажуємо в середовище надзвичайно корисний пакет DescTools. В ньому нас зараз
# цікавить функція LillieTest(), яка забезпечує альтернативну перевірку на нормальність за
# допомогою тесту Ліллієфорса.

library(DescTools)

setNames(aggregate(values ~ groups, FUN = function(x)
                                    c(shapiro = round(shapiro.test(x)$p.value, 4),
                                      lillie = round(LillieTest(x)$p.value, 4)),
                                      data = DF), c("groups", "shapiro", "lillie"))
agg <-
setNames(aggregate(values ~ groups, FUN = function(x)
                                    c(shapiro = round(shapiro.test(x)$p.value, 4),
                                      lillie = ifelse(length(x) <= 4, ":(",
                                                      round(LillieTest(x)$p.value, 4))),
                                                      data = DF),
                                                      c("groups", ""))

# Пакет nortest теж містить непогану підбірку функцій для перевірки на нормальність:

library(nortest)

# Тест Андерсона-Дарлінга (але буде помилка, оскільки замалий розмір вибірки). Для того, щоб
# видалити NA з вектора застосовуємо код df$B[!is.na(df$B)]).

ad.test(df$B[!is.na(df$B)])

# Те саме, але за допомогою функції з пакету DescTools. Рахує, але значення абсолютно useless:

AndersonDarlingTest(df$B[!is.na(df$B)])

# Ось це вже набагато корисніше; тест Жарке-Бера (до речі, дозволяє одразу викинути NA):

JarqueBeraTest(df$B, na.rm = TRUE)

# Ну що ж. Аналіз дисперсії та порівняння. Але спершу візуалізуємо дані за допомогою функції
# barplot пре-інстальованого пакету graphics. Ось тут нам знадобиться mean_se.

barplot(means_se[,2][,1], names.arg = means_se[,1])

# Дуже «сирий» малюнок. При нагоді намалюємо красивішу стовпчикову діаграму і порівняємо графіку
# graphics та ggplot2. Створюємо важливі змінні:

means <- means_se[,2][,1]
categories <- means_se[,1]

# створюємо матрицю:

bp.mx1 <- matrix(means, nrow = nrow(means_se), dimnames = list(categories))

# задаємо максимальне значення осі ординат та число позначок на ній, а також межі планок
# похибок:

max <- max(means + round(max(means)/4, 0))
yticks <- round(max/4.5, 0)
ym <- means + (-means_se[,2][,2])
yp <- means + means_se[,2][,2]

# Звузимо стовпчики, задамо їм відносне значення 0.8, а проміжки між ними як 0.2:

bwd <- 0.8
bsp <- 0.2

# Викликом є задати правильні координати позначок на осі абсцис. Використовуємо функцію seq().
# Ця функція задається наступним чином – seq(початкове значення, кінцеве значення, інтервал):

inc <- 0.8/2 + 0.2*0.8 + 0.8/2
xticks <- c(bwd + bwd/2, (seq(bwd + bwd/2,
                          length(levels(DF$groups)),
                          by = inc) + inc))

# par() – глобальні параметри рисунку; pty – plot type, "s" – square, mar – margins,
# lwd – line width, t() – transpose, col – color, gray1 – майже чорний колір,
# gray99 – майже білий колір, las = 1 – всі підписи по осях є горизонтальними,
# xaxs – стиль осі X, xaxt – тип осі X ("n" – відсутність осі), xlim() та ylim() задають
# мінімальне та максимальне значення осей. xlab = "" – забираємо автоматичний підпис осі.
# axis(1 – вісь X; axis(2 – вісь Y. at – задаємо положення позначок на осях, labels – задаємо
# підписи по осях, cex.axis – розміри цих підписів, mtext – текст довкола осей (включаючи
# підписи осей), side – сторона (1 – низ (вісь X), 2 – ліва (вісь Y), 3 – верх, 4 – права.
# line – задає положення напису; font = 2 – жирний шрифт.

par(pty = "s", mar = c(7.4, 5, 3.5, 0.1), lwd = 2.5)
barplot(t(bp.mx1), col = c("gray65"), axes = FALSE,
        xaxs = "i", xaxt = "n", ylim = c(0, max),
                                xlim = c(0, xticks[5]+1.2), width = rep(0.8, 5),
                                space = c(1, 0.2, 0.2, 0.2, 0.2), xlab = "", ylab = "")
axis(1, at = xticks, labels = FALSE, lwd = 3)
axis(2, at = seq(0, max, yticks), lwd = 3, las = 1, cex.axis = 3)
box(lwd = 3)
mtext(categories, side = 1, line = 2.5, cex = 3.5, font = 2, at = xticks)
mtext("Height", side = 2, line = 3.5, cex = 3.5, font = 2)
arrows(xticks, ym, xticks, yp, length = 0.08, angle = 90, code = 3, lwd = 2.5, col = "black")

# порівняння: звичайнісінький t-тест (двобічний, непарний, з нерівними дисперсіями):

t.test(df$C, df$D)

# тільки значення p:

t.test(df$C, df$D)$p.value

# для парного t-тесту маємо змінити дані. Робимо піднабір з груп C та D:

DF.pair <- DF[DF$groups %in% c("C", "D"),]

# ділимо його на вектори:

DF.pair <- split(DF.pair$values, DF.pair$groups)

# перевіряємо, як працює t-тест:

t.test(DF.pair$C, DF.pair$D)$p.value

# var.equal = TRUE – тест з рівними дисперсіями (гомоскедастичний):

t.test(DF.pair$C, DF.pair$D, var.equal = TRUE)$p.value

# модифікуємо вектор C за допомогою функції rep_len – додаємо до нього значень так, щоб розміри
# вибірок C та D були однаковими:

DF.pair$C <- rep_len(DF.pair$C, length(DF.pair$D))

# парний t-тест:

t.test(DF.pair$C, DF.pair$D, paired = TRUE)$p.value

# однофакторний аналіз дисперсії за допомогою функції aov (analysis of variance):

DF.aov <- aov(values ~ groups, data = DF)

# результати та їхні частини:

summary(DF.aov)

table <- unlist(summary(DF.aov))
table

pval <- unname(table[9])

summary(DF.aov)[[1]][[5]][1]

# множинні порівняння за допомогою тесту Тьюкі (найкраща опція):

TukeyHSD(DF.aov)

TukeyHSD(DF.aov)$groups

# тільки значення p:

round(TukeyHSD(DF.aov)$groups[,4], 4)

# множинні порівняння за допомогою t-тесту з поправкаю fdr
# (false discovery rate = Benjamini-Hochberg; btw, найкраща поправка):

pairwise.t.test(values, groups, data = DF, p.adjust.method = "fdr")

# пакет DescTools, функція PostHocTest, тест LSD (least significant difference) Фішера.
# чесно, не рекомендується його використовувати через високу ймовірність хибного відкриття.

PostHocTest(DF.aov, method = "lsd")

# тест Стьюдента-Ньюмена-Койлза. Теж дає ймовірність хибного відкриття (але меншу за LSD)

PostHocTest(DF.aov, method = "newmankeuls")

# тест Дункана. Посередині між LSD та SNK. Деякі журнали не приймають результатів з такою
# статистикою

PostHocTest(DF.aov, method = "duncan")

PostHocTest(DF.aov, method = "duncan")$groups[,4]

# Гарне оформлення результатів тесту:

data.frame(comparisons = names(PostHocTest(DF.aov, method = "duncan")$groups[,4]),
           p_values = round(unname(PostHocTest(DF.aov, method = "duncan")$groups[,4]), 4))

# Позначки («зірочки») достовірної різниці

star <- ifelse(unname(PostHocTest(DF.aov, method = "duncan")$groups[,4]) < 0.05, "*", "")

data.frame(comparisons = names(PostHocTest(DF.aov, method = "duncan")$groups[,4]),
           p_values = round(unname(PostHocTest(DF.aov, method = "duncan")$groups[,4]), 4),
           signif = star)

# Тест Даннета – порівняння всіх груп з контрольною:

DunnettTest(values, groups, data = DF)

# Тест Даннета за допомогою функцій пакету multcomp

library(multcomp)

DF.dt <- glht(DF.aov, linfct = mcp(groups = "Dunnett"))

# set.seed() може мати значення

set.seed(1000)

DunnettTest(values, groups, data = DF)

# Надзвичайно легкий спосіб перетворити прості дані з wide формату в long і навпаки за допомогою
# функцій пакету DescTools:

dcf <- ToLong(df, varnames = c("groups", "values"))
c(1:length(dcf$values)) -> rownames(dcf)

dct <- ToWide(values, groups, by = c(1:8), varnames = levels(factor(groups)))
dct <- dct[-1]

# ЗАВДАННЯ

# Оформіть наступні результати в окремі вектори і в таблицю з даними у форматі wide та long:
# Контроль – 45.1, 39.6, 33.9, 50.8, 23.2
# Мутант А – 56.8, 49.7, 74.2, 99.2, 107.3, 85.1, 43.4
# Мутант Б – 39.2, 47.7, 41.4, 33.6, 22.6, 55.3, 29.1
# Мутант В – 76.3, 51.1, 82.2, 59.3, 45.9
#
# Порахуйте середні, стандартні відхилення та стандартні похибки для всіх груп
# Перевірте нормальність розподілу для кожної групи тестами Шапіро-Вілка, Лілієфорса та Жарке-
# Бера. Чи всі тести дають однакові результати?
# Порівняйте непарним, двобічним, гомоскедастичним t-тестом групи «Контроль» та «Мутант Б»
# Порівняйте парним t-тестом групи «Мутант А» та «Мутант Б»
# Проведіть аналіз дисперсії з усіма даними. Чи достовірний вплив мутацій?
# Проведіть множинні порівняння тестом Тьюкі, тестом SNK та тестом Даннета. Між якими групами є
# достовірні відмінності?
# Проведіть множинні порівняння t-тестом з корекцією значень p на множинні порівняння.
# Намалюйте графік з планками похибок в ggplot2.
