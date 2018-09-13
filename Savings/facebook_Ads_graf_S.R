library(ggplot2)

#GRÁFICOS

#GRÁFICOS POR FORMATO

e <- ggplot(format.global, aes(reorder(formato, -Solicitudes),Solicitudes, fill = month))
e +  geom_col(width=0.7, position = "dodge") + theme_light() + 
  geom_text(aes(label = Solicitudes), position = position_dodge(0.7), vjust= -1) + my_font() 


b <- ggplot(Fcreatividad.global, aes(reorder(Producto, -Solicitudes),Solicitudes, fill = month))
b +  geom_col(width=0.7, position = "dodge") + theme_light() + 
  geom_text(aes(label = Solicitudes), position = position_dodge(0.7), vjust= -1) + my_font() 

c <- ggplot(Producto.global, aes(reorder(Producto, -Solicitudes),Solicitudes, fill = month))
c +  geom_col(width=0.7, position = "dodge") + theme_light() + 
  geom_text(aes(label = Solicitudes), position = position_dodge(0.7), vjust= -1) + my_font() 