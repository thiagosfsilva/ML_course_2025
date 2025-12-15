## Explorand o ggplot2
## Thiago Sanna F. Silva
## 12/12/2025
##

# Carregando pacotes necessários
library(tidyverse)

# Plotando dados do dataset iris
# Scatterplot ou gráfico de dispersão
head(iris)

ggplot(iris,aes(x=Sepal.Length,
                y = Sepal.Width,
                color = Species)) +
    geom_point() +
    geom_smooth(method = 'lm') +
 #  facet_wrap(~ Species, scale ='fixed') +
    ylab('Sepal Width (cm)') +
    xlab('Sepal Length (cm)') +
    ggtitle('Relationship between sepal length and width') +
    theme_gray(base_size = 8) +
    scale_colour_viridis_d()

    ggsave('resultados/meu_grafico.jpg',
       width=15,
       height=10,
       units='cm')

# O mesmo plot uando R base
plot(Sepal.Width ~ Sepal.Length, data = iris, col=Species)

## Histograma
ggplot(iris,aes(x=Sepal.Length)) +
    geom_histogram(binwidth = 0.25) +
    ylab('Frequency') +
    xlab('Sepal Length (cm)') +
    ggtitle('Distribution of Sepal length') +
    theme_gray(base_size = 12) +
    facet_grid(cols = vars(Species))

## Dot plot
ggplot(iris,aes(x=Sepal.Length)) +
    geom_dotplot(binwidth = 0.25) +
    ylab(NULL) +
    xlab('Sepal Length (cm)') +
    ggtitle('Distribution of Sepal length') +
    theme_gray(base_size = 12) +
    facet_grid(cols = vars(Species))

## Densidade
ggplot(iris,aes(x=Sepal.Length
                , colour = Species,
                fill = Species)) +
    geom_density(bw=0.2,alpha=.20) +
    ylab('Frequency') +
    xlab('Sepal Length (cm)') +
    ggtitle('Distribution of Sepal length') +
    theme_gray(base_size = 12) +
    scale_colour_brewer(palette='Dark2') +
    scale_fill_brewer(palette='Dark2')
   # facet_grid(cols = vars(Species))
   #


## Grafico de Barras
## geom_bar = 1D

ggplot(iris,aes(x=Species)) +
    geom_bar()

## geom_col = 2D
iris_sum <- iris %>%
    group_by(Species) %>%
    summarise(SL_mean = mean(Sepal.Length),
              SL_sd = sd(Sepal.Length))

iris_sum

ggplot(iris_sum,aes(x=Species, y=SL_mean)) +
    geom_col() +
    geom_errorbar(aes(ymin=SL_mean-SL_sd,
                        ymax=SL_mean+SL_sd),width=0.2)

## Medias com desvio padrao
ggplot(iris_sum,aes(Species,SL_mean)) +
    geom_point() +
    geom_pointrange(aes(ymin=SL_mean-SL_sd,
                      ymax=SL_mean+SL_sd),width=0.2)

## Boxplot
ggplot(iris,aes(Species,Sepal.Length)) +
    geom_boxplot()

## Violin Plot
ggplot(iris,aes(Species,Sepal.Length)) +
    geom_violin()

## Mostrando mais de duas dimensões
## Dados de riqueza de espécies de macrófitas
## e características limnológicas

mac <- read_csv('dados/brutos/rich_env_jun.csv')

# N total vs P total
ggplot(mac,aes(x=n.tot,y=p.tot)) +
    geom_point() +
    xlab('Nitrogenio Total') +
    ylab('Fosforo Total')

# N total vs P total vs Riqueza Sp
ggplot(mac,aes(x=n.tot,y=p.tot,colour = rich)) +
    geom_point() +
    xlab('Nitrogenio Total') +
    ylab('Fosforo Total')

# N total vs P total vs Riqueza Sp vs Profundidade
ggplot(mac,aes(x=n.tot,y=prof,
               colour = p.tot,
               size=rich)) +
    geom_point() +
    xlab('Nitrogenio Total') +
    ylab('Profundidade')


