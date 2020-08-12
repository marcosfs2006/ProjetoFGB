#Funcoes

  # Considera-se que, na morte do invalido, o conjuge recebe a pensao integral enquanto estiver vivo.
  # Isso porque o valor a ser pago sera o mesmo, independente de haver filho dependente
  # Em caso de morte do conjuge, a pensao eh revertida para o filho mais novo que seja menor do que a idade limite legal
  # Em caso de filho pensionista (ou seja, o servidor falecido nao possuia conjuge), considera-se que o filho mais novo eh o pensionista
  # Em caso da morte deste filho pensionista, cessa-se o beneficio
  
  calculaFGInvalidez = function (ValorBenefAnual,
                                 Idade,
                                 Sexo,
                                 TabuaInvalido,
                                 TabuaConjuge,
                                 TabuaFilho,
                                 TaxaJuros,
                                 IdadeLimite,
                                 DiferencaIdadeConjuge,
                                 DiferencaIdadePaisFilhos) {
    
    IdadeConjuge = Idade - DiferencaIdadeConjuge # Porque não pedir a idade do conjuge diretamente? e dos filhos?
    
    if(Sexo == "Masculino") {
      
      SexoConjuge = "Feminino"
    
    } else {
    
      SexoConjuge = "Masculino"
    
    }
    
    Cx = 0
    
    for(n in 0:(max(TabuaInvalido$x)-Idade)) {
      
      Hx1 = calculaFGPensaoMorte(ValorBenefAnual,(IdadeConjuge+n+1),SexoConjuge,2,TabuaConjuge,TabuaFilho,TaxaJuros,IdadeLimite,DiferencaIdadePaisFilhos)/ValorBenefAnual
      
      pyz = calculaProbUltimoSobrev(TabuaConjuge,TabuaFilho,IdadeConjuge+n,Idade-DiferencaIdadePaisFilhos+n,IdadeLimite)
      
      Vn1 = TabuaInvalido$v[TabuaInvalido$x==(n+1)]
      
      npx = TabuaInvalido$lx[TabuaInvalido$x==Idade+n]/TabuaInvalido$lx[TabuaInvalido$x==Idade]
      
      qxn = TabuaInvalido$qx[TabuaInvalido$x==Idade+n]
      
      Cxn = Vn1*npx*qxn*Hx1*pyz
      
      if( length(Cxn) > 0 ) Cx = Cx + Cxn
    }
    
    FGAposentadorias = ValorBenefAnual*TabuaInvalido$ax[TabuaInvalido$x==Idade]
    FGPensoes = ValorBenefAnual*Cx
    FGTotal = FGAposentadorias +FGPensoes
    TabelaResultados = data.frame(FGAposentadorias=FGAposentadorias,FGPensoes=FGPensoes,FGTotal=FGTotal)
    return(TabelaResultados)
  }
  
  
  geraTabuaComutUmaVida = function(Tabua, TaxaJuros) { 
    Tabua$dx = Tabua$lx*Tabua$qx
    Tabua$v = 1 / ((1 + TaxaJuros)^Tabua$x)
    Tabua$Dx = Tabua$lx *Tabua$v
    Tabua$Nx[Tabua$sexo=="Masculino"] = rev(cumsum(rev(Tabua$Dx[Tabua$sexo=="Masculino"])))
    Tabua$Nx[Tabua$sexo=="Feminino"]  = rev(cumsum(rev(Tabua$Dx[Tabua$sexo=="Feminino"])))
    Tabua$ax = Tabua$Nx/Tabua$Dx
    return(Tabua)
  }
  
  
  
  
  #Caso geral: considera-se que o conjuge recebe a pensao integral enquanto estiver vivo.
  #Isso porque o valor a ser pago sera o mesmo, independente de haver filho dependente
  #Em caso de morte do conjuge, a pensao eh revertida para o filho mais novo que seja menor do que a idade limite legal
  #Em caso de filho pensionista (ou seja, o servidor falecido nao possuia conjuge), considera-se que o filho mais novo eh o pensionista
  #Em caso da morte deste filho pensionista, cessa-se o beneficio
  
  calculaFGPensaoMorte = function (ValorBenefAnual,Idade,Sexo,Estado,TabuaConjuge,TabuaFilho,TaxaJuros,IdadeLimite,DiferencaIdadePaisFilhos) {
    #Se o dependente for o filho, o FG sera uma anuidade temporaria ate a idade limite de recebimento do beneficio para o filho
    if(Estado==3) {
      DuracaoBeneficio = IdadeLimite-Idade+1
      if(DuracaoBeneficio<=0) {
        return(print("Erro: dependente filho superou a idade limite para recebimento do beneficio."))
      }
      TabuaFilho$axn = (TabuaFilho$Nx-TabuaFilho$Nx[(DuracaoBeneficio+1):(nrow(TabuaFilho)+DuracaoBeneficio)])/TabuaFilho$Dx
      FundoGarantidor = TabuaFilho$axn[TabuaFilho$x==Idade]*ValorBenefAnual
      return(FundoGarantidor)
      
    } else if (Estado==2) {
      #Se o dependente for o conjuge, o FG sera uma anuidade vitalicia para o conjuge mais uma anuidade temporaria reversivel para o filho ate o limite legal da idade
      axConj = TabuaConjuge$ax[TabuaConjuge$x==Idade]

      #Se hoje a idade do filho eh superior a idade limite legal para recebimento do beneficio, o FG sera somente a anuidade vitalicia para o conjuge
      if((Idade-DiferencaIdadePaisFilhos)>IdadeLimite) {
        FundoGarantidor = axConj*ValorBenefAnual
        return(FundoGarantidor)
      }
      
      #Juntando duas tabuas lado a lado para facilitar o calculo das funcoes de comutacao para multiplas vidas
      TabuaM = cbind(TabuaConjuge,TabuaConjuge)
      TabuaM = rbind(TabuaM,TabuaM)
      TabuaM[,9:16] = NA #Apagando a tabua duplicada da direita, para incluir os dados da tabua do filho
      TabuaM[(nrow(TabuaConjuge)+1):nrow(TabuaM),] = NA #Apagando a tabua duplicada de baixo
      #Enfileirando a direita a tabua do filho
      TabuaM[(DiferencaIdadePaisFilhos+1):nrow(TabuaM),9:16] =
        TabuaFilho[1:(nrow(TabuaM)-DiferencaIdadePaisFilhos),]
      
      #Nomeando as colunas
      names(TabuaM) = c("xconj","lxconj","qxconj","dxconj","vconj","Dxconj","Nxconj","axconj","xfil","lxfil","qxfil","dxfil","vfil","Dxfil","Nxfil","axfil")

      #Removendo as linhas do final da tabela que estão vazias
      TabuaM = TabuaM[1:which(TabuaM$xfil==max(TabuaM$xfil,na.rm = TRUE)),]
      
      #IdadeFilho pode ser negativo, caso o pensionista ainda nao tenha filho
      IdadeFilho = Idade-DiferencaIdadePaisFilhos
      
      #Calculando o tempo restante para o nascimento do filho (m) e o tempo maximo de duracao do beneficio para o filho (n)
      if(IdadeFilho>=0) {
        m = 0
        n = IdadeLimite-(Idade-DiferencaIdadePaisFilhos)+1
      } else {
        m = DiferencaIdadePaisFilhos-Idade
        n = IdadeLimite+1
      }
      
      #Calculando anuidade temporaria para o filho da idade x ate a idade limite para recebimento do beneficio diferida do tempo restante para nascimento do filho
      axnfill = (TabuaM$Nxfil[which(TabuaM$xfil==(IdadeFilho+m))]-TabuaM$Nxfil[which(TabuaM$xfil==(IdadeFilho+m+n))])/TabuaM$Dxfil[which(TabuaM$xfil==(IdadeFilho+m))]
      #Calculando seguro dotal puro da idade x ate a idade limite para recebimento do beneficio
      Axmfill = TabuaM$Dxconj[which(TabuaM$xconj==(Idade+m))]/TabuaM$Dxconj[which(TabuaM$xconj==Idade)]
      #Calculando anuidade temporaria para o filho da idade x ate a idade limite para recebimento do beneficio diferida do tempo restante para nascimento do filho
      maxnfill = axnfill*Axmfill
      
      #Calculando as funcoes de comutacao de multiplas vidas
      TabuaM$Dxy = TabuaM$vconj*TabuaM$lxconj*TabuaM$lxfil
      TabuaM$Dxy[!is.na(TabuaM$axnfil)&is.na(TabuaM$Dxy)] = 0
      TabuaM$Nxy = NA
      TabuaM$Nxy[which(!is.na(TabuaM$Dxy))] = rev(cumsum(rev(TabuaM$Dxy[which(!is.na(TabuaM$Dxy))])))
      TabuaM$axy = TabuaM$Nxy/TabuaM$Dxy
      
      #Calculando anuidade temporaria para o status de vida conjunta da idade x do filho ate a idade limite para recebimento do beneficio diferida do tempo restante para nascimento do filho
      axyn = (TabuaM$Nxy[which(TabuaM$xfil==(IdadeFilho+m))]-TabuaM$Nxy[which(TabuaM$xfil==(IdadeFilho+m+n))])/TabuaM$Dxy[which(TabuaM$xfil==(IdadeFilho+m))]
      #Calculando anuidade temporaria para o status de vida conjunta da idade x do filho ate a idade limite para recebimento do beneficio diferida do tempo restante para nascimento do filho
      maxynfill = axyn*Axmfill
      
      #Anuidade reversivel: o filho recebe uma anuidade somente depois da morte do pai/mae (conjuge pensionista)
      aconj_filn = maxnfill - maxynfill
      FundoGarantidor = (axConj + aconj_filn)*ValorBenefAnual
      return(FundoGarantidor)
    }
  }
  
  calculaProbUltimoSobrev = function (TabuaConjuge,TabuaFilho,IdadeConjuge,IdadeFilho,IdadeLimite) {
    py = 1 - TabuaConjuge$qx[TabuaConjuge$x==IdadeConjuge]
    pz = 1 - TabuaFilho$qx[TabuaFilho$x==IdadeFilho]
    if(IdadeFilho<0 | IdadeFilho>IdadeLimite | IdadeFilho>max(TabuaFilho$x)) {
      if(IdadeConjuge>max(TabuaConjuge$x)) {
        pyz = 0
      } else {
        pyz = py
      }
    } else if (IdadeConjuge>max(TabuaConjuge$x)){
      pyz = pz
    } else {
      pyz = py + pz - py*pz
    }
    return(pyz)
  }

  calculaFG = function(Estado,
                       ValorBenefAnual,
                       Idade,
                       Sexo,
                       TabuaInvalido,
                       TabuaConjuge,
                       TabuaFilho,
                       TaxaJuros,
                       IdadeLimite,
                       DiferencaIdadeConjuge,
                       DiferencaIdadePaisFilhos) {
    if(Estado==2|Estado==3) {
      FGAposentadorias = 0
      FGPensoes = calculaFGPensaoMorte(ValorBenefAnual,Idade,Sexo,Estado,TabuaConjuge,TabuaFilho,TaxaJuros,IdadeLimite,DiferencaIdadePaisFilhos)
      FGTotal = FGPensoes
      TabelaResultados = data.frame(FGAposentadorias=FGAposentadorias,FGPensoes=FGPensoes,FGTotal=FGTotal)
      return(TabelaResultados)
    } else if (Estado==1) {
      return(calculaFGInvalidez(ValorBenefAnual,Idade,Sexo,TabuaInvalido,TabuaConjuge,TabuaFilho,TaxaJuros,IdadeLimite,DiferencaIdadeConjuge,DiferencaIdadePaisFilhos))
    }
  }



