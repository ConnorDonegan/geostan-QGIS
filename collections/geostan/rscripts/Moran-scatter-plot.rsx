##geostan=group
##Moran scatter plot=name
##Layer=vector polygon
##Variable=Field Layer
##Matrix_type=selection Row_standardized;Binary Row_standardized
##File_png=file moran-plot.png
##Width_inches=number 4.66
##Height_inches=number 4

type <- ifelse(Matrix_type == 'Binary', 'B', 'W')
M <- geostan::shape2mat(Layer, style = type)
y <- Layer[[Variable]]
geostan::moran_plot(y, M, na.rm = TRUE)
ggplot2::ggsave(File_png, width = Width_inches, height = Height_inches)