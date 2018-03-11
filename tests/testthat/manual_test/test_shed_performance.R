library(ggplot2)

shed(diamonds[1:999, ])
shed(diamonds[1:1001, ])
shed(diamonds[1:9999, ])


tf <-tempfile()

write.csv(diamonds[1:1001, ], tf)

shed(diamonds[1:10001, ])
