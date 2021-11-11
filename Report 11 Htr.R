# 0) Prelúdio =======

setwd('/home/heitor/Área de Trabalho/R Projects/Análise Macro/Labs/Lab 11')

library(tidyverse)
library(arules)
library(arulesViz)
library(RColorBrewer)
library(plotly)

# 1) Importação e Descrição =======

dds <- read.transactions("groceries.csv", sep = ",")
inspect(dds[1:4])
summary(dds)

# 1.1) Frequência ---

itemFrequency(dds[,1:3])

tst <- itemFrequency(dds) %>%
	as.data.frame() %>%
	rownames_to_column(var='product')
tst <- dplyr::rename(tst, 'frq' = `.`)
tst %>% dplyr::filter(str_detect(product, 'ine'))

sort(itemFrequency(dds[dds %in% "coffee"]),
	 decreasing = T)

itemFrequencyPlot(dds, support = 0.1)
itemFrequencyPlot(dds, topN = 20,
				  type = "relative",
				  col  = brewer.pal(8,'Pastel2'),
				  main = "Absolute Item Frequency Plot")

specific_sub <- subset(dds, items %pin% "newspapers")
itemFrequency(specific_sub)


rule1 <- apriori(dds, parameter=
				 	list(support    = (2.5*30)/9835,
				 		 confidence = 0.25,
				 		 minlen     = 2))
rule1
rule1 %>% summary()

inspect(rule1[1:3]) %>% View()
inspect(
	subset(rule1, items %pin% "yogurt"))

inspect(sort(rule1, by = "lift")[1:5])

# Filter rules with confidence greater than 0.4 or 40%
sub_rule1 <- rule1[quality(rule1)$confidence>0.4]

plot(sub_rule1)

plot(sub_rule1,
	 method = "grouped", control = list(k = 5))
plot(sub_rule1,
	 method="graph", control=list(type="items"))
plot(sub_rule1,
	 method="paracoord",
	 control=list(alpha=.5, reorder=TRUE))
plot(sub_rule1,
	 measure=c("support","lift"),
	 shading="confidence",interactive=T)

top10subRules <- head(sub_rule1,
					  n = 10, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")

