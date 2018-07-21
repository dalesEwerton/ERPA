dados <- read.csv("data.csv")

#Substituição da virgula pelo ponto decimal
dados$Resultado_da_Pesquisa <- as.numeric(sub(",", ".", dados$Resultado_da_Pesquisa, fixed = TRUE))
dados$Brancos_Nulos <- as.numeric(sub(",", ".", dados$Brancos_Nulos, fixed = TRUE))
dados$Indecisos <- as.numeric(sub(",", ".", dados$Indecisos, fixed = TRUE))
attach(dados)

#Quais são os candidatos da nossa base de dados?
candidatos <- levels(Nome_do_Candidato)
#Quais os institutos da nossa base de dados?
pesquisa <- levels(Instituto)
#Quais os anos das pesquisas da nossa base de dados?
ano <- unique(Ano_da_Eleicao);

#qual a média de cada candidato levando em consideração o instituto e o ano?
mediaCandidatos = function () {
  contador <- 1;
  mediaDosCandidatos <- matrix(NA, nrow = 200, ncol = 4);
  for( i in 1 : length(candidatos)){
    nomeCandidato <- candidatos[i];
    for(j in 1: length(pesquisa)){
      for(k in 1: length(ano)){
        existe <- existePesquisa(nomeCandidato,(pesquisa[j]), ano[k]);
        if(existe==TRUE){
          intencoes <- filtraPorCandidatoInstitutoEAno(nomeCandidato,(pesquisa[j]),ano[k]);
          media <- mean(intencoes$Resultado_da_Pesquisa);
          mediaDosCandidatos[contador,1] = nomeCandidato;
          mediaDosCandidatos[contador,2] = pesquisa[j];
          mediaDosCandidatos[contador,3] = media;
          mediaDosCandidatos[contador,4] = ano[k]; 
          contador= contador+1;
        }
        
      }
    }
  }
  return(mediaDosCandidatos)
}

#qual a media das pesquisas de intenção de voto sobre um candidato?
mediaCandidatoPesquisado=function(candidatoPesquisado){
  contador <- 1;
  mediaDosCandidatos <- matrix(NA, nrow = 200, ncol = 4);
    nomeCandidato <- candidatoPesquisado;
    for(j in 1: length(pesquisa)){
      for(k in 1: length(ano)){
        existe <- existePesquisa(nomeCandidato,(pesquisa[j]), ano[k]);
        if(existe==TRUE){
          intencoes <- filtraPorCandidatoInstitutoEAno(nomeCandidato,(pesquisa[j]),ano[k]);
          media <- mean(intencoes$Resultado_da_Pesquisa);
          mediaDosCandidatos[contador,1] = nomeCandidato;
          mediaDosCandidatos[contador,2] = pesquisa[j];
          mediaDosCandidatos[contador,3] = media;
          mediaDosCandidatos[contador,4] = ano[k]; 
          contador= contador+1;
        }
    }
    }
  return(mediaDosCandidatos)
}

#qual a media dos candidatos em um determinado ano?
mediaCandidatosPorAno=function(anoPesquisado){
  contador <- 1;
  mediaDosCandidatos <- matrix(NA, nrow = 100, ncol = 4);
  for( i in 1 : length(candidatos)){
    nomeCandidato <- candidatos[i];
    for(j in 1: length(pesquisa)){
        existe <- existePesquisa(nomeCandidato,(pesquisa[j]), anoPesquisado);
        if(existe==TRUE){
          intencoes <- filtraPorCandidatoInstitutoEAno(nomeCandidato,(pesquisa[j]),anoPesquisado);
          media <- mean(intencoes$Resultado_da_Pesquisa);
          mediaDosCandidatos[contador,1] = nomeCandidato;
          mediaDosCandidatos[contador,2] = pesquisa[j];
          mediaDosCandidatos[contador,3] = media;
          mediaDosCandidatos[contador,4] = anoPesquisado; 
          contador= contador+1;
        }
        
      }
    }
  return(mediaDosCandidatos)
}

#Qual foi o menor e o maior valor das intencoes de voto para o candidato?
menorEMaiorResultadoDoCandidato=function(candidatoNome, anoPesquisa){
  intencoes <- filtraPorCandidatoEAno(candidatoNome, anoPesquisa);
  return(range(intencoes$Resultado_da_Pesquisa));
}


#Qual o ano em que mais temos dados de pesquisas?
anoComMaisPesquisas=function(){
  return(moda(Ano_da_Eleicao));
}

#Qual o candidato em que mais temos dados de pesquisas?
candidatoComMaisPesquisas=function(){
  return(moda(Nome_do_Candidato));
}

#Qual o instituto em que mais temos dados de pesquisas?
institutoComMaisPesquisas=function(){
  return(moda(Instituto));
}

#Qual o partido em que mais temos dados de pesquisas?
partidoComMaisPesquisas=function(){
  return(moda(Partido_do_Candidato));
}

filtraPorCandidatoEAno = function(candidatoNome, anoPesquisa){
  candidatoEscolhido <- subset(dados, Nome_do_Candidato == candidatoNome);
  anoEscolhido <- subset(candidatoEscolhido, Ano_da_Eleicao==anoPesquisa);
  return(anoEscolhido)
}

filtraPorCandidatoInstitutoEAno = function(nomeCandidato, nomeInstituto, anoPesquisa){
  candidatoEscolhido <- subset(dados, Nome_do_Candidato == nomeCandidato);
  instituto <- subset(candidatoEscolhido, Instituto == nomeInstituto);
  a <- subset(instituto, Ano_da_Eleicao == anoPesquisa)
  return(a);
}

existePesquisa= function(nomeCandidato,pesquisa, ano){
  existe <- FALSE;
  candidatoEscolhido <- subset(dados, Nome_do_Candidato == nomeCandidato);
  for( i in 1 : length(candidatoEscolhido$Nome_do_Candidato)){
     if(candidatoEscolhido[,4][i] == pesquisa && candidatoEscolhido[,6][i] == ano){
       existe <- TRUE;
     }
  }
  return(existe)
}

moda <- function(x) {
  z <- table(as.vector(x)) 
  names(z)[z == max(z)]
}
