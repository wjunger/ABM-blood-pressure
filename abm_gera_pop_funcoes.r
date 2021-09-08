# funções para abm
# algumas funções
require(MASS)
gera_idade=function(fxetaria,limites)
{
    li=sapply(fxetaria,function(x)limites$li[limites$FE==x])
    ls=sapply(fxetaria,function(x)limites$ls[limites$FE==x])
    #idade=mapply(function(x1,x2)sample(seq(x1,x2),1,replace=TRUE),li,ls)
    idade=floor(runif(length(fxetaria),li,ls))
}

gera_imccat=function(fxetaria,sexo,probs,peso=NULL)
{
    do_draw=function(x,probs,peso)
	{
		# fxetaria : 1 e sexo : 2
		if(!is.null(peso))
			pr=probs[probs$FE==x['fxetaria'] & probs$sexo==x['sexo'],3:5]*peso[x['sexo'],x['fxetaria']]
		else
			pr=probs[probs$FE==x['fxetaria'] & probs$sexo==x['sexo'],3:5]
		draw=rmultinom(1,1,pr)
		imc=c('IMC01','IMC02','IMC03')[which(draw==1)]
		return(imc)
	}
	vars=data.frame(fxetaria,sexo)
    imc_todos=apply(vars,1,do_draw,probs,peso)
    return(imc_todos)
}

gera_histfam=function(id,probs)
{
	draw=sample(probs$pais,length(id),replace=TRUE,prob=probs$prob)
	return(draw)
}

gera_fxetaria_sexo=function(id,probs)
{
	draw=sample(probs$ident,length(id),replace=TRUE,prob=probs$prop)
	sexo=substring(draw,1,1)
	fxetaria=substring(draw,3,6)
	return(data.frame(fxetaria,sexo))
}

gera_imc=function(imccat,limites)
{
    li=sapply(imccat,function(x)limites$li[limites$imc==x])
    ls=sapply(imccat,function(x)limites$ls[limites$imc==x])
    imc=runif(length(imccat),li,ls)
}

gera_fumante=function(fxetaria,sexo,probs,peso=NULL)
{
    do_draw=function(x,probs,peso)
	{
		# fxetaria : 1 e sexo : 2
		if(!is.null(peso))
			probs=probs*peso
		pr=probs[x['sexo'],x['fxetaria']]
		fuma=rbinom(1,1,pr)
		return(fuma)
	}
	
	vars=data.frame(fxetaria,sexo)
    fuma_todos=apply(vars,1,do_draw,probs,peso)
    return(fuma_todos)
}

gera_np=function(fxetaria,sexo,np,corsd=0.75)
{
    do_draw=function(x,np,corsd)
	{
		# fxetaria : 1 e sexo : 2
		m=unlist(np[np$FE==x['fxetaria'] & np$sexo==x['sexo'],c('pas','pad')])
		dp=unlist(np[np$FE==x['fxetaria'] & np$sexo==x['sexo'],c('dp_pas','dp_pad')])
		covsd=dp[1]*dp[2]*corsd
		s=matrix(c(dp[1]^2,covsd,covsd,dp[2]^2),nrow=2,ncol=2,byrow=TRUE)
		pressao=mvrnorm(1,m,s)
		return(pressao)
	}
	
	vars=data.frame(fxetaria,sexo)
    pressao_todos=t(apply(vars,1,do_draw,np,corsd))
	has=ifelse(pressao_todos[,1]>=140 | pressao_todos[,2]>=90,9,0) # código 9 para hipertensos na geração da população
    return(data.frame(pressao_todos,has))
}

gera_id=function(n)
{
	id=seq(1,n)
	return(id)
}




