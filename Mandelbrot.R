#manelbrot package required
mb=mandelbrot(xlim = c(-0.8335, -0.8325),ylim = c(0.205, 0.206),resolution = 1200L,iterations = 10000)
mm=c(colorRampPalette(c("black","white","yellow"))(20),
colorRampPalette(c("#DE4C8A", "cyan", "violet",
"red","yellow"), bias=5)(200),"black")
df=as.data.frame(mb)
ggplot(df, aes(x = x, y = y, fill = value)) +
geom_raster(interpolate = TRUE) + theme_void() +
scale_fill_gradientn(colours = mm, guide = "none")
