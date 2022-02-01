library()

dado <- read.csv('C:/Users/Intel/Desktop/Formação Cientista De Dados/R/Dados/Churn.csv', sep = ';', na.strings = '', stringsAsFactors = T)
head(dados)
summary(dados)
colnames(dados) <- c('id', 'Score', 'Estados', 'Genero', 'Idade', 'Patrimonio', 'Saldo', 'produtos', 'TemCredito',
                     'ativo', 'Salario', 'Saiu')
head(dados)

#estados
counts <- table(dados$Estados)
barplot(counts, main = 'Estados', xlab = 'Estados', ylab = 'Dados', legend.text = TRUE)

#genero
count <- table(dados$Genero)
barplot(count, main = 'genero')

#score
summary(dados$Score)        
boxplot(dados$Score)

hist(dados$Score)

summary(dados$Idade)
barplot(dados$Idade)

#saldo
summary(dados$Saldo)
boxplot(dados$Saldo)
hist(dados$Saldo)

#salario

summary(dados$Salario)
boxplot(dados$Salario)
boxplot(dados$Salario, outline = F)

#Valores Faltantes

dados[!complete.cases(dados),]

#Aula 02

#Salario
summary(dados$Salario)
median(dados$Salario, na.rm = T)
mean(dados$Salario, na.rm = T)

dados[is.na(dados$Salario),]$Salario = median(dados$Salario, na.rm = T)
dados[!complete.cases(dados$Salario)]

#genero

dados[!complete.cases(dados$Genero),]

unique(dados$Genero)
summary(dados$Genero)

dados[is.na(dados$Genero) | dados$Genero == 'M',]$Genero = 'Masculino'
dados[dados$Genero == 'Fem' | dados$Genero == 'F',]$Genero = 'Feminino'
 summary(dados$Genero)

dados$Genero = factor(dados$Genero) 
summary(dados$Genero) 

#Idade

summary(dados$Idade)
dados[dados$Idade < 0 | dados$Idade > 110,]$Idade
dados[is.na(dados$Idade)]
median(dados$Idade)
dados[dados$Idade < 0 | dados$Idade > 110,]$Idade = median(dados$Idade)
summary(dados$Idade) 
dados[dados$Idade < 18,]

#Aual De Tratamento de dados 2
summary(dados$id)
boxplot(dados$id)

x = dados[duplicated(dados$id),]
x

dados = dados[-c(82),]
x
dados[dados$Id == x$id,]

#estados

unique(dados$Estados)
summary(dados$Estados)
dados[!dados$Estados %in% c('RS', 'SC', 'PR'),]$Estados = 'RS'
summary(dados$Estados)
dados$Estados = factor(dados$Estados) 

#valores fora do padrão

desv <- sd(dados$Salario, na.rm = T)
desv

dados[dados$Salario >= 2 * desv,]$Salario

boxplot(dados$Salario, outline = F)
x = boxplot(dados$Salario)$out
x

median(dados$Salario)

dados[dados$Salario >= 2 * desv ,]$Salario = median(dados$Salario)
dados[dados$Salario >= 2 * desv ,]$Salario



