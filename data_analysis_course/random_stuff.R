# apply family
#1
get_negative_values <- function(test_data){
  tmp <- apply(test_data, 2, function(x) x[!is.na(x) & x < 0])
  tmp[sapply(tmp, function(x) length(x) > 0)]
}

test_data <- as.data.frame(list(V1 = c(NA, -0.5, -0.7, -8), V2 = c(-0.3, NA, -2, -1.2), V3 = c(1, 2, 3, NA)))
get_negative_values(test_data)

#2
naughty <- function(x){
  x[is.na(x)] <- mean(x, na.rm = T)
  return(x)
}

na_rm  <- function(x){
  as.data.frame(apply(x, 2, naughty))
}

test_data <- as.data.frame(list(V1 = c(NA, NA, NA, NA, 13, 12, 9, 10, 8, 9, 11, 11, 10, 12, 9), V2 = c(NA, 12, 8, NA, 11, 11, 9, 8, 8, 10, 10, 11, 10, 10, 10), V3 = c(NA, 5, NA, 13, 12, 11, 11, 14, 8, 12, 8, 8, 10, 10, 8), V4 = c(10, 10, 10, 10, 13, 10, 11, 7, 12, 10, 7, 10, 13, 10, 9)))
na_rm(test_data)

#3
positive_sum <-  function(test_data){
  lapply(test_data, function(x) sum(x[x > 0], na.rm = T))
}

d <- data.frame(X1 = c(-1, -2, 0), X2 = c(10, 4, NA), X3 = c(-4, NA, NA))
positive_sum(d)

#4
my_names <- function (dataset, names){
  good_rows <- sapply(dataset$name, function(x) any(sapply(names, grepl, x = x)))
  dataset[good_rows, ]
}

test_data <- as.data.frame(list(name = c("p4@HPS1", "p7@HPS2", "p4@HPS3", "p7@HPS4", "p7@HPS5", "p9@HPS6", "p11@HPS7", "p10@HPS8", "p15@HPS9"), expression = c(118.84, 90.04, 106.6, 104.99, 93.2, 66.84, 90.02, 108.03, 111.83)))
names = c("HPS5", "HPS6", "HPS9", "HPS2", "HPS3", "HPS7", "HPS4", "HPS8")
my_names(test_data, names)

# Fucking data.table
library(data.table)
#1
filter.expensive.available <- function(products, brands) {
  return(products[price >= 500000 & available == TRUE & brand %in% brands])
}

sample.products <- data.table(price = c(10000, 600000, 700000, 1000000),
                              brand = c("a", "b", "c", "d"),
                              available = c(T, T, F, T))
ff <- filter.expensive.available(sample.products, c("a", "c", "d"))

#2
ordered.short.purchase.data <- function(purchases) {
  ord <- order(purchases$price, decreasing = T)
  purchases[ord][quantity >= 0][, list(odernumber = ordernumber, product_id = product_id)]
}

sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = 1:4,
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)
ordered.short.purchase.data(sample.purchases)

#3
purchases.median.order.price <- function(purchases) {
  purchases[quantity >= 0][, list(total_price = sum(price * quantity)), by = ordernumber][, list(med = median(total_price))]$med
}

sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = c(1,2,2,3),
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)
purchases.median.order.price(sample.purchases)

#4
get.category.ratings <- function(purchases, product.category) {
  setkey(purchases, product_id, totalcents, quantity)
  setkey(product.category, product_id, category_id)
  purchases.with.category <- merge(purchases, product.category, by = "product_id")
  purchases.with.category[ , list(totalcents = sum(totalcents), quantity = sum(quantity)), by = "category_id"]
}

product.category <- data.table(product_id = c(1,1,2,2,3),
                               category_id = c(1,2,1,3,3))
purchases <- data.table(product_id = c(1, 2, 3),
                        totalcents = c(100, 200, 300),
                        quantity = c(1, 1, 3))

get.category.ratings(purchases, product.category)

# ggplot mastering
library(ggplot2)
#1
sales = read.csv("https://stepic.org/media/attachments/course/724/sales.csv")
str(sales)

ggplot(sales, aes(income, sale)) + 
  geom_point(aes(color = shop)) + 
  geom_smooth()

#2
ggplot(sales, aes(shop, income, color = season)) + 
  stat_summary(fun.data = mean_cl_boot, position = position_dodge(0.2))

#3
ggplot(sales, aes(date, sale, color = shop, group = shop)) + 
  stat_summary(fun.data = mean_cl_boot, position = position_dodge(0.2), geom = "errorbar", width = 0.2) +
  stat_summary(fun.y = mean, geom = "point", position = position_dodge(0.2)) + 
  stat_summary(fun.y = mean, geom = "line", position = position_dodge(0.2))

#4
iris_plot <- ggplot(iris, aes(Sepal.Length, Petal.Length, col = Species))+
  geom_point() +
  geom_smooth(method = "lm")+
  scale_color_discrete(name = "Вид цветка", labels = c("Ирис щетинистый", "Ирис разноцветный", "Ирис виргинский"))+
  scale_x_continuous(name = "Длина чашелистика", breaks = 4:8, limits = c(4, 8))+ 
  scale_y_continuous(name = "Длина лепестка", breaks = 1:7)

#5
library(plotly)
teapot.coords <- read.csv("~/Projects/R/github/teapot.csv", sep = ";")

Sys.setenv("plotly_username" = "quantum-pie")
Sys.setenv("plotly_api_key" = "fGo8KfzI8I0f4RWeUsKS")

make.fancy.teapot <- function(teapot.coords) {
  i.s <- seq(0, nrow(teapot.coords) - 1, 3)
  j.s <- seq(1, nrow(teapot.coords) - 1, 3)
  k.s <- seq(2, nrow(teapot.coords) - 1, 3)
  p <- plot_ly(teapot.coords, type="mesh3d", x = ~x, y = ~y, z = ~z, i = i.s, j = j.s, k = k.s)
  plotly_POST(p, "test")
}