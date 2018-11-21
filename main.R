# Tiago de Barros Moraes (tbm2) --------------------
# Matheus Belfort de Moura Torres (mbmt) --------------------

# ---------- QUESTÃO 1 ----------
# Basicamente usamos a biblioteca ensinada em aula para ler o .csv

data_frame = read.csv("Sandler - Página1.csv")
print('Questao 1:')
print(data_frame)
cat("\n")
# ---------- FIM DA QUESTÃO 1 ---------



# ---------- QUESTÃO 2 ----------:
# Armazenando as notas em um vetor
NOTAS = data_frame$NOTAS

# Calculando a soma das notas
sum = 0;
for(x in NOTAS){
  sum = sum + x;
}

num_filmes = length(NOTAS)

media = sum/num_filmes
resp_media = paste0('Questao 2: ', media)
print(resp_media)
cat("\n")
# ---------- FIM DA QUESTÃO 2 ---------



# ---------- QUESTÃO 3 ----------
# primeiro calculamos a variancia
# para todos os elementos em NOTAS, fazemos a diferença para a média já calculada e somamos a uma variável
# depois dividimos pelo número de notas

sum_var = 0;

for(x in NOTAS) {
  sum_var = sum_var + ((media-x)*(media-x))
}

variancia = sum_var/num_filmes

#agora como temos que o desvio padrao é a raiz quadrada da variancia
desvio_padrao = sqrt(variancia)
resp_desvio_padrao = paste0('Questao 3: ', desvio_padrao)
print(resp_desvio_padrao)
cat("\n")
# ---------- FIM DA QUESTÃO 3 ----------



# ---------- QUESTÃO 4 ----------
# fazemos um sort nas notas
# depois, para cada elemnto no vetor ordenado, vemos se o seguinte é igual a ele ou não
# se for, somamos um á variável que guarda o tamanho da tentativa de virar a moda
# no momento que o 'attempt' passa a moda, ela vira a moda e o 'attempt' é resetado
ordered_notas = sort(NOTAS)
moda = ordered_notas[1]
size_moda = 0
attempt = ordered_notas[1]
size_attempt = 0

for(n in ordered_notas) {
  if(size_attempt <= size_moda) {
    if(n == moda) {
      size_moda = size_moda + 1
    } else {
      if(n == attempt) {
        size_attempt = size_attempt + 1
      } else {
        attempt = n
        size_attempt = 1
      }
    }
  } else {
    moda = attempt
    size_moda = size_attempt
    size_attempt = 0
  }
}
resp_moda = paste0('Questao 4: ', moda)
print(resp_moda)
cat("\n")
# ---------- FIM DA QUESTÃO 4 ----------



# ---------- QUESTÃO 5 ----------
# percorremos o vetor de notas, quando o elemento analisado é maior que 6,
# adicionamos o titulo correspondente em outro vetor
getHigher = function(val) {
  high_notas = c()
  i = 1
  for(n in NOTAS) {
    if(n >= val) {
      high_notas = c(high_notas, paste0(TITULOS[i]))
    }
    i = i + 1
  }
  return(high_notas)
}
print('Questao 5:')
print(getHigher(6))
cat("\n")
# ---------- FIM DA QUESTÃO 5 ----------



# ---------- QUESTÃO 6 ----------
# usamos uma forma de remover os elementos de um vetor do outro
# removemos todos os elementos que estão no resultado da função getHigher() de uma cópia de TITULOS
getSmaller =  function(val) {
  high_films_to_remove = getHigher(val)
  small_films = TITULOS [! TITULOS %in% high_films_to_remove]
  return(length(small_films))
}

resp_small_notas = paste0('Questao 6: ', getSmaller(6), ' filmes com notas menores que 6')
print(resp_small_notas)
cat("\n")
# ---------- FIM DA QUESTÃO 6 ----------



# ---------- QUESTÃO 7 ----------
# primeiro a função que retorna os filmes com maior e menosr avaliação
# percorremos o vetor e vemos a posição da maior e menor nota
# depois pegamos as strings dos títulos no vetor de TITULOS
TITULOS = data_frame$TITULOS
ANOS = data_frame$ANO
getMinMax = function() {
  min_nota = NOTAS[1]
  min_pos = 1
  max_nota = NOTAS[1]
  max_pos = 1
  i = 1
  for(n in NOTAS) {
    if(n < min_nota) {
      min_nota = n
      min_pos = i
    } else if(n > max_nota) {
      max_nota = n;
      max_pos = i
    }
    i = i+1
  }
  resp = c(paste(TITULOS[min_pos]), paste(TITULOS[max_pos]))
  return(resp)
}
print('Questao 7:')
print(getMinMax())

# função usada para a criação do data.frame
# mesma lógica da anterior, mas retorna os índices
getMinMaxIndex = function() {
  min_nota = NOTAS[1]
  min_pos = 1
  max_nota = NOTAS[1]
  max_pos = 1
  i = 1
  for(n in NOTAS) {
    if(n < min_nota) {
      min_nota = n
      min_pos = i
    } else if(n > max_nota) {
      max_nota = n;
      max_pos = i
    }
    i = i+1
  }
  resp = c(min_pos, max_pos)
  return(resp)
}

# agora a criação do data frame
min_pos = getMinMaxIndex()[1]
max_pos = getMinMaxIndex()[2]
vector_titulo = c(paste0(TITULOS[min_pos]), paste0(TITULOS[max_pos]))
vector_ano = c(ANOS[min_pos], ANOS[max_pos])
vector_notas = c(NOTAS[min_pos], NOTAS[max_pos])
data_frame_max_min = data.frame(TITULO = vector_titulo, NOTA = vector_notas, ANO = vector_ano)

print(data_frame_max_min)
cat("\n")
# ---------- FIM DA QUESTÃO 7 ----------



# ---------- QUESTÃO 8 ----------
getBestYear = function() {
  data_frame_mod = data_frame
  
  #Primeiro ordenamos o dataframe segundo os anos da lan?amento dos filmes
  data_frame_mod = data_frame_mod[order(data_frame_mod$ANO),]
  
  #Agora tomamos vetores com os valores dos anos e notas j? reordenados
  ANOS_MOD = data_frame_mod$ANO
  NOTAS_MOD = data_frame_mod$NOTAS
  ano_atual = ANOS_MOD[1]
  films_per_year = c(0)
  all_years = c(ano_atual)
  i = 1
  
  #Por fim percorremos linearmente o vetor dos anos verificando se o vetor
  #das notas na posi??o i tem valor maior ou igual que 6.5, preenchendo um
  #vetor com o n?mero de notas maiores que 6.5 em cada um nos anos
  for(a in ANOS_MOD) {
    if(a != ano_atual) {
      all_years = c(all_years, a)
      if(NOTAS_MOD[i] >= 6.5) {
        films_per_year = c(films_per_year, 1)
      } else {
        films_per_year = c(films_per_year, 0)
      }
    } else {
      if(NOTAS_MOD[i] >= 6.5) {
        films_per_year[length(films_per_year)] = films_per_year[length(films_per_year)] + 1
      }
    }
    ano_atual = a
    i = i + 1
  }
  
  max_films = films_per_year[1]
  i = 1
  max_films_index = 0
  for(n in films_per_year) {
    if(n > max_films) {
      max_films = n
      max_films_index = i
    }
    i = i + 1
  }
  
  return(all_years[max_films_index])
}

resp_best_year = paste0('Questao 8: ', getBestYear())
print(resp_best_year)
cat("\n")
# ---------- FIM DA QUESTÃO 8 ----------

# ---------- QUESTÃO 9 ----------
# usamos a mesma abordagem da questão 8, mas, dessa vez, 
# permitimos a aparição de valores repetidos para o vetor que vai gerar o histograma

data_frame_mod = data_frame
data_frame_mod = data_frame_mod[order(data_frame_mod$ANO),]
ANOS_MOD = data_frame_mod$ANO
NOTAS_MOD = data_frame_mod$NOTAS
i = 1

anos_hist = c()

for(a in ANOS_MOD) {
  if(NOTAS_MOD[i] >= 6) {
    anos_hist = c(anos_hist, a)
  }
  i = i+1
}


hist(anos_hist, breaks = length(anos_hist), col = '#002366', border = '#ffffff', 
     main = 'Filmes com notas maiores ou iguais a seis', xlab = 'Anos', ylab = 'Frequencia')


# ---------- FIM DA QUESTÃO 9 ----------



# ---------- QUESTÃO 9 (ABORDAGEM COM GRAFICO DE BARRAS) ----------
# abordagem da questao 8
data_frame_mod = data_frame
data_frame_mod = data_frame_mod[order(data_frame_mod$ANO),]
ANOS_MOD = data_frame_mod$ANO
NOTAS_MOD = data_frame_mod$NOTAS
ano_atual = ANOS_MOD[1]
films_per_year = c(0)
all_years = c(ano_atual)
i = 1
for(a in ANOS_MOD) {
  if(a != ano_atual) {
    all_years = c(all_years, a)
    if(NOTAS_MOD[i] >= 6) {
      films_per_year = c(films_per_year, 1)
    } else {
      films_per_year = c(films_per_year, 0)
    }
  } else {
    if(NOTAS_MOD[i] >= 6) {
      films_per_year[length(films_per_year)] = films_per_year[length(films_per_year)] + 1
    }
  }
  ano_atual = a
  i = i + 1
}

#Agora preenchemos um novo vetor, para inserir os anos
#em que nao houve filmes no dado intervalo entre o
#a primeira e ultima data de lan軋mento
min_year = all_years[1]
max_year = all_years[length(all_years)]
ano_atual = min_year
all_interval_of_years = min_year:max_year
all_interval_of_films_per_year = c()
i = 1

for(a in all_interval_of_years){
  if(all_years[i] == a){
    all_interval_of_films_per_year = c(all_interval_of_films_per_year, films_per_year[i])
    i = i + 1
  }else{
    all_interval_of_films_per_year = c(all_interval_of_films_per_year,0)
  }
}

# barplot(all_interval_of_films_per_year, names.arg = all_interval_of_years, space = 0.3, col = '#8cba4e', border = '#8cba4e', main = 'Filmes com notas maiores ou iguais a seis', xlab = 'Anos', ylab = 'Frequencia')
# ---------- FIM ----------