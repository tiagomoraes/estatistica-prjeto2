# Tiago de Barros Moraes (tbm2) --------------------
# Matheus Belfort de Moura Torres (mbmt) --------------------

# ---------- QUESTÃO 1 ----------
# Basicamente usamos a biblioteca ensinada em aula para ler o .csv

data_frame = read.csv("Sandler - Página1.csv")
print(data_frame)
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
resp_media = paste0('Media: ', media)
print(resp_media)
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
resp_desvio_padrao = paste0('Desvio Padrao: ', desvio_padrao)
print(resp_desvio_padrao)
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
resp_moda = paste0('Moda: ', moda)
print(resp_moda)
# ---------- FIM DA QUESTÃO 4 ----------



# ---------- QUESTÃO 5 ----------
# percorremos o vetor de notas, quando o elemento analisado é maior que 6, adicionamos em outro vetor
getHigher = function(val) {
  high_notas = c()
  for(n in NOTAS) {
    if(n >= val) {
      high_notas = c(high_notas, n)
    }
  }
  return(high_notas)
}

print(getHigher(6))
# ---------- FIM DA QUESTÃO 5 ----------



# ---------- QUESTÃO 6 ----------
# usamos uma forma de remover os elementos de um vetor do outro
# removemos todos os elementos que estão no resultado da função getHigher() de uma cópia de NOTAS
getSmaller =  function(val) {
  high_notas_to_remove = getHigher(val)
  small_notas = NOTAS [! NOTAS %in% high_notas_to_remove]
  return(small_notas)
}

print(getSmaller(6))
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
# ---------- FIM DA QUESTÃO 7 ----------



# ---------- QUESTÃO 8 ----------

# ---------- FIM DA QUESTÃO 8 ----------



# ---------- QUESTÃO 9 ----------

# ---------- FIM DA QUESTÃO 9 ----------