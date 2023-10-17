##geostan=group
##Summary figures=name
##Layer=vector polygon
##Variable=Field Layer
##Matrix_type=selection Row_standardized;Binary Row_standardized
##File_name_png=file 
##Width_inches=number 4.66
##Height_inches=number 4
##showplots

type <- ifelse(Matrix_type == 'Binary', 'B', 'W')
M <- geostan::shape2mat(Layer, style = type)
y <- Layer[[Variable]]
geostan::sp_diag(y, Layer, w = M)
geostan::moran_plot(y, M, na.rm = TRUE)
ggplot2::ggsave(File_png, width = Width_inches, height = Height_inches)