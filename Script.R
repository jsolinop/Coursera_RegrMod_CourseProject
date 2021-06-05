library(datasets)
library(dplyr)
library(ggplot2)
library(GGally)


head(mtcars)

##########################################
# MPG vs transmision type - Boxplot
##########################################
summary(my_cars)

ggpairs(
        my_cars,
        lower = list(continuous = "smooth", size = 2, alpha = 0.5,binwidth = .5),
        upper = list(continuous = wrap('cor', size = 2)),
        title = "mtcars - Pairs"
) + theme(axis.text = element_text(size = 7))


my_cars <- mtcars %>% 
        mutate( mpg = mpg,
                   am = factor(am, labels = c("manual","automatic")),
                   vs = factor(vs, labels = c("V","S")))

my_cars %>% ggplot(aes(x = am, y = mpg)) +
        geom_boxplot() +
        geom_jitter(aes(color = am), size = 4, alpha = .35) +
        labs(title = "MPG vs transmision type",
             x = "transmision type",
             y = "mpg")+
        theme(axis.text.x = element_text(
                angle = 0,
                size = 8,
                vjust = 0.5
        ),
        panel.background = element_rect(fill = "grey90"),
        panel.grid = element_line(color = "grey"),
        legend.position = "bottom"
        ) 

my_cars %>% ggplot(aes(x = vs, y = mpg)) +
        geom_boxplot() +
        geom_jitter(aes(color = vs), size = 4, alpha = .35) +
        labs(title = "MPG vs motor shape",
             x = "v-shape / straight",
             y = "mpg")+
        theme(axis.text.x = element_text(
                angle = 0,
                size = 8,
                vjust = 0.5
        ),
        panel.background = element_rect(fill = "grey90"),
        panel.grid = element_line(color = "grey"),
        legend.position = "bottom"
        ) 



##########################################
# MPG vs transmission type - Boxplot
##########################################
cor(my_cars[c(1,2,3,4,5,6,7,10,11)], my_cars[c(1,2,3,4,5,6,7,10,11)])

# test hypothesis
with(my_cars,t.test(mpg ~ am))
with(my_cars,t.test(mpg ~ vs))


fit_trans <- lm(mpg ~ am, data = my_cars)
summary(fit_trans)
fit_trans$coefficients
mean(fit_trans$residuals)

fit_all <- lm(mpg ~ ., data = my_cars)
summary(fit_all)
fit_all$coefficients
mean(fit_all$residuals)

fit_5 <- lm(mpg ~ am + wt + qsec + disp + hp , data = my_cars)
summary(fit_5)
fit_5$coefficients
mean(fit_5$residuals)

fit_4 <- lm(mpg ~ wt + qsec + disp + hp , data = my_cars)
summary(fit_4)
fit_5$coefficients
mean(fit_5$residuals)


fit_3 <- lm(mpg ~ am + wt + qsec, data = my_cars)
summary(fit_3)
fit_3$coefficients
mean(fit_3$residuals)


par(mfrow = c(2,2))
plot(fit_trans)
plot(fit_2)
plot(fit_3)
plot(fit_4)
plot(fit_5)
plot(fit_all)




