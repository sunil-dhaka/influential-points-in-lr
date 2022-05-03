influence4 <- read.table("../data/influence1.txt", header=T)
attach(influence4)
plot(x, y)
model.1 <- lm(y ~ x)
summary(model.1)

model.2 <- lm(y ~ x, subset=1:20) # exclude obs #21
summary(model.2)

plot(x=x, y=y, col=ifelse(Row<=20, "blue", "red"),
panel.last = c(lines(sort(x), fitted(model.1)[order(x)], col="red"),
lines(sort(x[-21]), fitted(model.2)[order(x[-21])],
col="red", lty=2)))
legend("topleft", col="red", lty=c(1,2),
inset=0.02, legend=c("Red point included", "Red point excluded"))

influence.measures(model.1)

influence.measures(model.2)
