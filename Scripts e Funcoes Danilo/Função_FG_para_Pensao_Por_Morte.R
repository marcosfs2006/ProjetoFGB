#Funcoes
{
  #Caso geral: considera-se que o conjuge recebe a pensao integral enquanto estiver vivo.
  #Isso porque o valor a ser pago sera o mesmo, independente de haver filho dependente
  #Em caso de morte do conjuge, a pensao eh revertida para o filho mais novo que seja menor do que a idade limite legal
  #Em caso de filho pensionista (ou seja, o servidor falecido nao possuia conjuge), considera-se que o filho mais novo eh o pensionista
  #Em caso da morte deste filho pensionista, cessa-se o beneficio
  calculaFGPensaoMorte = function (ValorPensaoAnual,Idade,Sexo,Estado,TabuaConjuge,TabuaFilho,TaxaJuros,IdadeLimite,DiferencaIdadePaisFilhos) {
    #Se o dependente for o filho, o FG sera uma anuidade temporaria ate a idade limite de recebimento do beneficio para o filho
    if(Estado=="Filho") {
      DuracaoBeneficio = IdadeLimite-Idade+1
      if(DuracaoBeneficio<=0) {
        return(print("Erro: dependente filho superou a idade limite para recebimento do beneficio."))
      }
      TabuaFilho$axn = (TabuaFilho$Nx-TabuaFilho$Nx[DuracaoBeneficio:(nrow(TabuaFilho)+DuracaoBeneficio-1)])/TabuaFilho$Dx
      FundoGarantidor = TabuaFilho$axn[TabuaFilho$x==Idade]*ValorPensaoAnual
      return(FundoGarantidor)
      
    } else if (Estado=="Conjuge") {
      #Se o dependente for o conjuge, o FG sera uma anuidade vitalicia para o conjuge mais uma anuidade temporaria reversivel para o filho ate o limite legal da idade
      axConj = TabuaConjuge$ax[TabuaConjuge$x==Idade]
      IdadeFilho = Idade-DiferencaIdadePaisFilhos
      DuracaoBeneficio = IdadeLimite-IdadeFilho+1
      
      #Se hoje a idade do filho eh superior a idade limite legal para recebimento do beneficio ou se ainda nao tem filho, o FG sera somente a anuidade vitalicia para o conjuge
      if(IdadeFilho>IdadeLimite | IdadeFilho<0) {
        FundoGarantidor = axConj*ValorPensaoAnual
        return(FundoGarantidor)
      }
      #Juntando duas tabuas lado a lado para facilitar o calculo das funcoes de comutacao para multiplas vidas
      TabuaM = cbind(TabuaConjuge,TabuaConjuge)
      TabuaM[,9:16] = NA #Apagando a tabua duplicada da direita, para incluir os dados da tabua do filho
      #Enfileirando a direita a tabua do filho
      TabuaM[(DiferencaIdadePaisFilhos+1):nrow(TabuaM),9:16] =
        TabuaFilho[1:(nrow(TabuaM)-DiferencaIdadePaisFilhos),]
      
      #Nomeando as colunas
      names(TabuaM) = c("xconj","lxconj","qxconj","dxconj","vconj","Dxconj","Nxconj","axconj","xfil","lxfil","qxfil","dxfil","vfil","Dxfil","Nxfil","axfil")
      
      #Calculando anuidade temporaria para o filho da idade atual ate a idade limite para recebimento do beneficio
      TabuaM$axnfil = (TabuaM$Nxfil-TabuaM$Nxfil[(DuracaoBeneficio+1):(nrow(TabuaM)+DuracaoBeneficio)])/TabuaM$Dxfil
      
      #Calculando as funcoes de comutacao de multiplas vidas
      TabuaM$Dxy = TabuaM$vconj*TabuaM$lxconj*TabuaM$lxfil
      TabuaM$Nxy = NA
      TabuaM$Nxy[which(!is.na(TabuaM$Dxy))] = rev(cumsum(rev(TabuaM$Dxy[which(!is.na(TabuaM$Dxy))])))
      TabuaM$axy = TabuaM$Nxy/TabuaM$Dxy
      
      #Calculando anuidade temporaria para o status de vida conjunto ate a idade limite para recebimento do beneficio
      TabuaM$axyn = (TabuaM$Nxy-TabuaM$Nxy[(DuracaoBeneficio+1):(nrow(TabuaM)+(DuracaoBeneficio))])/TabuaM$Dxy
      
      #Anuidade reversivel: o filho recebe uma anuidade somente depois da morte do pai/mae (conjuge pensionista)
      aconj_filn = TabuaM$axnfil[which(TabuaM$xfil==IdadeFilho)] - TabuaM$axyn[which(TabuaM$xfil==IdadeFilho)]
      FundoGarantidor = (axConj + aconj_filn)*ValorPensaoAnual
      return(FundoGarantidor)
    }
  }
  
  geraTabuaComutUmaVida = function(Tabua,TaxaJuros) {
    Tabua$dx = Tabua$lx*Tabua$qx
    Tabua$v = 1/((1+TaxaJuros)^Tabua$x)
    Tabua$Dx = Tabua$lx*Tabua$v
    Tabua$Nx[Tabua$sexo=="Masculino"] = rev(cumsum(rev(Tabua$Dx[Tabua$sexo=="Masculino"])))
    Tabua$Nx[Tabua$sexo=="Feminino"] = rev(cumsum(rev(Tabua$Dx[Tabua$sexo=="Feminino"])))
    Tabua$ax = Tabua$Nx/Tabua$Dx
    return(Tabua)
  }
  
}

#Definindo pasta padrao que contem a tabua de vida
setwd("C:/Users/danil/Dropbox/Projeto Fundos Garantidores RCC/Funções no R")
#options("scipen" = 999)

#Definindo taxa de juros
TaxaJuros = 0.06

#Carregando as tabuas de mortalidade (IBGE2010)
Tabua = read.csv("TabuaMorteIBGE2010.csv",sep = ";",dec=',')
Tabua = geraTabuaComutUmaVida(Tabua,TaxaJuros)

#Definindo parametros para rodar uma execucao da funcao
Idade = 18
Sexo = "Feminino"
Estado = "Conjuge"
TabuaConjuge = Tabua[Tabua$sexo==Sexo,-2]
TabuaFilho = Tabua[Tabua$sexo=="Feminino",-2]
IdadeLimite = 21
ValorPensaoAnual = 13000
DiferencaIdadePaiFilho = 30
DiferencaIdadeMaeFilho = 25
if(Sexo=="Masculino") {
  DiferencaIdadePaisFilhos = DiferencaIdadePaiFilho
} else {
  DiferencaIdadePaisFilhos = DiferencaIdadeMaeFilho
}

#Executando funcao com os parametros acima selecionados
calculaFGPensaoMorte(ValorPensaoAnual,Idade,Sexo,Estado,TabuaConjuge,TabuaFilho,TaxaJuros,IdadeLimite,DiferencaIdadePaisFilhos)


#Plotando grafico para todas idades do conjuge
Dist = NULL
for(i in 18:max(TabuaConjuge$x)) {
  FG = calculaFGPensaoMorte(ValorPensaoAnual,i,Sexo,Estado,TabuaConjuge,TabuaFilho,TaxaJuros,IdadeLimite,DiferencaIdadePaisFilhos)
  Dist = c(Dist,FG)
}
plot(Dist,xaxt='n')
axis(1,1:length(Dist),18:(max(TabuaConjuge$x)))
abline(v=8)

#Plotando grafico para todas idades do filho
Dist = NULL
for(i in 0:IdadeLimite) {
  FG = calculaFGPensaoMorte(ValorPensaoAnual,i,Sexo,Estado,TabuaConjuge,TabuaFilho,TaxaJuros,IdadeLimite,DiferencaIdadePaisFilhos)
  Dist = c(Dist,FG)
}
plot(Dist)



