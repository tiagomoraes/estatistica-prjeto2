# Tiago de Barros Moraes (tbm2) --------------------
# Matheus Belfort de Moura Torres (mbmt) --------------------

# ---------- QUESTÃO 1 ----------
# Basicamente usamos a biblioteca ensinada em aula para ler o .csv

data_frame = read.csv("Sandler - Página1.csv")
#print(data_frame)
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
#primeiro calculamos a variancia

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

print(moda)