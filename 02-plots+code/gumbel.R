library('ggplot2')


gumbelD = function(x, mu=0, beta=1) {
  1/beta * exp(-(x - mu)/beta) * exp(-exp(-(x - mu)/beta)) 
}

mf1 = function(x) gumbelD(x, beta=1)
mf2 = function(x) gumbelD(x, beta=1/3)
mf3 = function(x) gumbelD(x, beta=1/5)

myplot = ggplot(data.frame(x = c(-2.5,5)), aes(x)) +
  stat_function(fun = mf1, aes(linetype = "lambda=1")) +
  stat_function(fun = mf2, aes(linetype = "lambda=3")) +
  # stat_function(fun = mf3, aes(color = "lambda=5")) +
  scale_linetype_manual("", breaks = c("lambda=1", "lambda=3"), values = c("dotted", "dashed")) + 
  theme_bw() + ylab("P(x)")
show(myplot)


ggsave("gumbel_examples.pdf", width = 12, height = 6, units = "cm", scale = 1.4)