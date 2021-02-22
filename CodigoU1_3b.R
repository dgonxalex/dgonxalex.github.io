carrera=c("Ingeniería Electrónica","Ingeniería Civil","Ingeniería Civil",
          "Biología","Ingeniería Electrónica","Biología",                
          "Ingeniería Electrónica","Ingeniería Mecánica","Biología","Biología",
          "Biología","Biología","Ingeniería Civil","Biología","Ingeniería Civil",
          "IngenieríaCivil","Biología","Biología","Ingeniería Civil","Biología",
          "Biología", "Ingeniería Civil","Ingeniería Civil","Biología",
          "Ingeniería Electrónica","Ingeniería Mecánica","Biología",
          "Ingeniería Civil","Biología","Biología","Ingeniería Civil","Biología",
          "Biología","Ingeniería de Sistemas","Ingeniería Civil","Biología", 
          "Ingeniería Civil","Ingeniería Civil","Ingeniería Civil", 
          "Ingeniería Civil","Ingeniería Civil","Ingeniería Civil",
          "Ingeniería Civil","Ingeniería Civil","Ingeniería Civil","Biología",
          "Ingeniería Civil","Ingeniería Civil","Ingeniería Civil",
          "Ingeniería Civil","Ingeniería Civil", "Ingeniería Civil",
          "Ingeniería Mecánica","Ingeniería de Sistemas", "Ingeniería Civil",
          "Ingeniería Civil","Ingeniería Civil","Ingeniería de Sistemas",
          "Ingeniería Civil","Ingeniería Civil","Ingeniería Civil",
          "Ingeniería Civil","Ingeniería Civil","Ingeniería de Sistemas",
          "Ingeniería Civil","Biología","Ingeniería Mecánica","Ingeniería Civil",
          "Biología","Negocios Internacionales","Ingeniería Civil","Biología",
          "Ingeniería Civil","NegociosInternacionales","Ingeniería Civil",
          "Ingeniería Electrónica","Biología","Negocios Internacionales",
          "Negocios Internacionales","Ingeniería Civil","Ingeniería Civil") 

grupo=c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
        "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
        "A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "B", "B", "B", 
        "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
        "B", "B", "B", "B", "B", "B", "F", "F", "F", "F", "F", "F", "F", "F", 
        "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F")


promacum =c(4.18, 3.49, 3.85, 3.75, 3.69, 3.47, 4.29, 4.03, 4.05, 3.90, 3.55, 
            4.63, 3.94, 4.28, 4.02, 4.02, 4.02, 3.79, 3.82, 3.34, 3.50, 3.40, 
            3.51, 3.56, 3.84, 3.65, 4.18, 4.08, 4.58, 3.47, 4.22, 3.96, 3.59, 
            3.42, 4.71, 4.17, 3.54, 4.16, 3.60, 3.65, 4.25, 3.61, 3.81, 4.20, 
            3.37,   NA, 3.42, 4.02, 4.83, 3.59, 3.91, 3.55, 3.46, 3.51, 4.50, 
            4.26, 3.54, 3.69, 3.35, 4.24, 3.86, 3.65, 4.24, 4.47, 4.21, 3.65, 
            4.12, 3.57, 3.56, 3.73, 3.98, 3.71, 4.00, 4.16, 3.36, 3.42, 4.08, 
            3.49, 4.07, 3.53, 4.33)

edad=c(19, 19, 22, 21, 19, 20, 21, 19, 21, 20, 20, 21, 21, 20, 19, 21, 19, 20, 
       20, 21, 21, 18, 19, 19, 18, 19, 20, 19, 22, 19, 22, 19, 20, 19, 21, 20, 
       19, 20, 20, 20, 20, 20, 18, 20, 18, 18, 20, 20, 21, 20, 21, 21, 20, 20, 
       19, 19, 22, 18, 20, 20, 22, 21, 19, 21, 21, 21, 22, 21, 19, 22, 21, 23, 
       19, 21, 20, 20, 19, 20, 20, 20, 20)

id=1:81
bd0052=data.frame(id, grupo, carrera,promacum, edad)
# una variable cualitativa
t=table(bd0052$grupo)
pie(t)

# dos variables cualitativas
t2=table(bd0052$carrera)
t3=table(bd0052$carrera, bd0052$grupo)
t2
names(t2)=c("I.Civ","Bio","I.Sis","I.Ele","I.Mec","N.Int", " "," ")
t2=sort(t2, decreasing = TRUE)
barplot(t2[1:5])
barplot(t3)

#  cuantitativa - cualitativa
boxplot(bd0052$promacum~bd0052$grupo)

# una variable cuantitativa
hist(bd0052$promacum, breaks = 3, xlim = c(3,5), ylim = c(0,40))
grid()
boxplot(bd0052$promacum)

# dos variables cuantitativas
bd0052$edad=rnorm(81, 19,2)
plot(bd0052$promacum, bd0052$edad)

# matriz de graficos 2x2
par(mfrow=c(2,2))
pie(t)
barplot(t3)
boxplot(bd0052$promacum~bd0052$grupo)
plot(bd0052$promacum, bd0052$edad)
    
