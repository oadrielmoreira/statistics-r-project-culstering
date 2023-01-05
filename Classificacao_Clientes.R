pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "misc3d", #gráficos 3D
             "plot3D", #gráficos 3D
             "cluster", #função 'agnes' para elaboração de clusters hierárquicos
             "factoextra", #função 'fviz_dend' para construção de dendrogramas
             "ade4") #função 'ade4' para matriz de distâncias em var. binárias

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


#importando df
library(readxl)
df <- read_excel("Base_Classificacao_Clientes.xlsx")
View(df)

rownames(df) <- df$ID_Cliente

#Gráfico Scatter
scatter3D(x=df$,
          y=df$`Receita bruta`,
          z=df$`Quantidade bruta`,
          phi = 0, bty = "g", pch = 20, cex = 2,
          xlab = "Curva",
          ylab = "Receita",
          zlab = "Quantidade Bruta",
          main = "Clientes",
          clab = "Receita")>
  text3D(x=df$`Receita R$`,
         y=df$Curva,
         z=df$`Receita bruta`,
         labels = rownames(ID_Cliente),
         add = TRUE, cex = 1)

#Estatísticas descritivas
summary(df$`Receita R$`)
summary(df$Frequency)
summary(df$`Meses Positivados`)


#---------- Esquema de aglomeração hierárquico ---------------------------------

# Matriz de dissimilaridades
matriz_D <- df %>% 
  select(`Receita R$`, Frequency, `Meses Positivados`) %>% 
  dist(method = "euclidean")

# Visualizando a matriz de dissimilaridades
data.matrix(matriz_D) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Elaboração da clusterização hierárquica
cluster_hier <- agnes(x = matriz_D, method = "average")

# As distâncias para as combinações em cada estágio
coeficientes <- sort(cluster_hier$height, decreasing = FALSE) 
coeficientes

esquema <- as.data.frame(cbind(cluster_hier$merge, coeficientes))
names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")
esquema

# Visualização do esquema hierárquico de aglomeração
esquema %>%
  kable(row.names = T) %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Dendrograma com visualização dos clusters
fviz_dend(x = cluster_hier,
          k = 3,
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          lwd = 1,
          ggtheme = theme_bw())

# Criando variável categórica para indicação do cluster no banco de dados
## O argumento 'k' indica a quantidade de clusters
df$cluster_H <- factor(cutree(tree = cluster_hier, k = 3))

# Visualização da base de dados com a alocação das observações nos clusters
df %>%
  select(ID_Cliente, cluster_H) %>% 
  arrange(ID_Cliente) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# Estatísticas descritivas da variável '`Receita R$`'
group_by(df, cluster_H) %>%
  summarise(
    mean = mean(`Receita R$`, na.rm = TRUE),
    sd = sd(`Receita R$`, na.rm = TRUE),
    min = min(`Receita R$`, na.rm = TRUE),
    max = max(`Receita R$`, na.rm = TRUE))

# Estatísticas descritivas da variável 'Frequency'
group_by(df, cluster_H) %>%
  summarise(
    mean = mean(Frequency, na.rm = TRUE),
    sd = sd(Frequency, na.rm = TRUE),
    min = min(Frequency, na.rm = TRUE),
    max = max(Frequency, na.rm = TRUE))

# Estatísticas descritivas da variável '`Meses Positivados`'
group_by(df, cluster_H) %>%
  summarise(
    mean = mean(`Meses Positivados`, na.rm = TRUE),
    sd = sd(`Meses Positivados`, na.rm = TRUE),
    min = min(`Meses Positivados`, na.rm = TRUE),
    max = max(`Meses Positivados`, na.rm = TRUE))

#-------------------------ANOVA

# ANOVA da variável '`Receita R$`'
summary(anova_receita <- aov(formula = `Receita R$` ~ cluster_H,
                                 data = df))

# ANOVA da variável 'Frequency'
summary(anova_frequency <- aov(formula = Frequency ~ cluster_H,
                                data = df))

# ANOVA da variável '`Meses Positivados`'
summary(anova_meses <- aov(formula = `Meses Positivados` ~ cluster_H,
                              data = df))

