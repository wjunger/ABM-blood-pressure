# funções de atualização da simulação
atualiza_agentes=function(x,funcao,...)
# wrapper para apply
{
val= t(apply(x,1,funcao,...))
return(val)
}


sim_atualiza_idade=function(x,probs,idademax)
# "id"      "sexo"    "idade"   "imc"     "fumante" "histfam" "pas"     "pad"     "has"     "pas0"    "pad0"    "morto"   "ativfis"   "dieta"
{
	if(x['morto']==0)
	{
		# mata agente
		idad=ifelse(x['idade']>100,100,x['idade'])
		sex=ifelse(x['sexo']==1,'M','F')
		prob=with(probs,probs[idade==idad,sex])
		x['morto']=rbinom(1,1,prob)
		# atualiza idade dos sobreviventes
		if(x['morto']==0) 
			x['idade']=x['idade']+1
		if (x['idade']>idademax) # idade máxima
			x['morto']=1
	}

	return(x)
}


sim_atualiza_imc=function(x,coefs)
{
    # atualiza IMC
	if(x['morto']==0)
	{
		t=x['idade']-18
		betas=with(coefs,coefs[sexo==x['sexo'],c('Beta0','Beta1','Beta2','Beta3')])
		varimc=betas[[2]]+betas[[3]]+betas[[4]]+2*betas[[3]]*t+3*betas[[4]]*t+3*betas[[4]]*t^2
		x['imc']=x['imc']+varimc+rnorm(1,0,abs(varimc))
	}
	
	return(x)
}


sim_atualiza_np=function(x,coefs)
{
    # atualiza níveis pressóricos
	if(x['morto']==0)
	{
		t=x['idade']-60
		# sistólica
		beta_sis=with(coefs,coefs[sexo==x['sexo'] & grupo==x['grupo'] & PA=='SIS',c('Beta1','Beta2')])
		varsis=beta_sis[[1]]+beta_sis[[2]]+2*beta_sis[[2]]*t
		x['pas']=x['pas']+varsis+rnorm(1,0,abs(varsis))
		# diastólica
		beta_dia=with(coefs,coefs[sexo==x['sexo'] & grupo==x['grupo'] & PA=='DIA',c('Beta1','Beta2')])
		vardia=beta_dia[[1]]+beta_dia[[2]]+2*beta_dia[[2]]*t
		x['pad']=x['pad']+vardia+rnorm(1,0,abs(vardia))
		x['pad']=ifelse(x['pad']<45,45,x['pad'])  # garante que a PAD > 45
	}
	return(x)
}


sim_atualiza_risco=function(x,imcmin,tempo)
{
	if(x['morto']==0 & x['has']==0 & x['imc'] > imcmin)
	{
		x['risco']=1
	}
	else
	{
		if (x['imc'] < imcmin & x['tdiimc'] > 0 & x['tdiimc'] <= tempo)
		{
			x['risco']=1
			x['tdiimc']=x['tdiimc']+1
		}
		else
		{
			x['risco']=0
			x['tdiimc']=0
		}
	}
	
	return(x)
}

sim_gera_selecao=function(id,risco,meta)
{
	nr=sum(risco)
	nsel=round(nr*meta,0)
	idrisco=id[risco==1]
	sel=sample(idrisco,nsel)
	return(sel)
}


sim_atualiza_intervencao=function(x,pars,sel,probaf,probdieta,imcmin,t0,tini,tfim,wearoff)
{
	if((x['id'] %in% sel) & (t0>=tini) & (t0<=tfim))
	{
		
		# decide AF
		if(x['ativfis']==0)
		{
			x['ativfis']=rbinom(1,1,probaf)
		} 
		else
		{
			x['ativfis']=rbinom(1,1,2*probaf)
		}
		
		# decide dieta
		if(x['dieta']==0)
		{
			x['dieta']=rbinom(1,1,probdieta)
		} 
		else
		{
			x['dieta']=rbinom(1,1,2*probdieta)
		}

		# 2 semestres?
		aftodoano=rbinom(1,1,0.5)
		dtodoano=rbinom(1,1,0.5)
				
		# atualização do IMC
		if(x['ativfis']==1 & x['dieta']==1)
		{
			red=rnorm(1,pars[pars['interv']=='D+AF','media'],pars[pars['interv']=='D+AF','dp'])
			red2=rnorm(1,pars[pars['interv']=='D+AF','media'],pars[pars['interv']=='D+AF','dp'])*aftodoano*dtodoano
			x['imc']=x['imc']-red-red2
			x['ured']=red+red2
		}
		else if(x['ativfis']==1 & x['dieta']==0)
		{
			red=rnorm(1,pars[pars['interv']=='AF','media'],pars[pars['interv']=='AF','dp'])
			red2=rnorm(1,pars[pars['interv']=='AF','media'],pars[pars['interv']=='AF','dp'])*aftodoano
			x['imc']=x['imc']-red-red2
			x['ured']=red+red2
		}
		else if(x['ativfis']==0 & x['dieta']==1)
		{
			red=rnorm(1,pars[pars['interv']=='D','media'],pars[pars['interv']=='D','dp'])
			red2=rnorm(1,pars[pars['interv']=='D','media'],pars[pars['interv']=='D','dp'])*dtodoano
			x['imc']=x['imc']-red-red2
			x['ured']=red+red2
		}
		else
		{
			if (x['tdi']>0 & x['tdi']<=wearoff)
			{
				x['imc']=x['imc0']-x['ured']/x['tdi']
				x['tdi']=x['tdi']+1
			}
			
			if (x['tdi']>wearoff)
			{
				x['tdi']=0
			}
		}
		
		if(x['ativfis']==1 | x['dieta']==1)
		{
			x['cont']=x['cont']+1
			if (x['imc'] < imcmin & x['tdiimc']==0)
			{
				x['tdiimc']=1	
			}

			x['tdi']=1
			
			if (x['tdi']==1)
			{
				x['imc0']=x['imc']
			}
		}
	}
	
	return(x)
}


sim_atualiza_has=function(x)
{
    # atualiza níveis pressóricos
	if(x['morto']==0 & x['has']==0)
	{
		# define as variáveis
		sex=ifelse(x['sexo']==2,1,0)
		hf=ifelse(x['histfam']==0,0,1)
		# calcula o modelo
		eta=22.94954-0.15641*x['idade'] -0.20293*sex -0.19073*x['fumante'] -0.16612*hf -0.03388*x['imc'] -0.05933*x['pas'] -0.128468*x['pad'] +0.001624*x['pad']*x['idade']
		# calcula probabilidade
		prob=1-exp(-exp((log(1)-eta)/0.87692))
		# draw
		x['has']=rbinom(1,1,prob)
	}
	return(x)
}


traca_agente=function(x,agente,vars,iter,maxiter)
{
xl=c(0,maxiter)
nl=length(vars)
y=x[x[,'id']==agente,]
for(i in 1:nl)
{
	if(iter==1 & i==1)
		{
		close.screen(all=TRUE)
		split.screen(c(nl,1))
		# legend('top',legend=c('vivo','morto'),pch=c('-','+'),box.lty=0,horiz=TRUE)
		}
	screen(i,new=FALSE)
	if (iter==1)
	{
		par(mar=c(4,4,2,2))
		plot(iter+y['idade0'],y[vars[i]],xlim=xl+y['idade0'],ylab=vars[i],xlab='Idade',pch='-')
	}
	else
		{
		point=ifelse(y['morto']==0,'-','+')
		color=ifelse(y['morto']==0,'black','red')
		points(iter+y['idade0'],y[vars[i]],pch=point,col=color)
		}
}
Sys.sleep(0)
}


plot_agente=function(x,agente,vars,lim=NULL)
{
morto=x[x[,'id',1]==agente,'morto',]
idade0=x[x[,'id',1]==agente,'idade0',1]
xl=1:dim(x)[3]+idade0
nl=length(vars)
for(i in 1:nl)
{
y=x[x[,'id',1]==agente,vars[i],]
	if(i==1)
		{
		close.screen(all=TRUE)
		split.screen(c(nl,1))
		}
	screen(i,new=FALSE)	
	par(mar=c(4,4,2,2))
	if(is.null(lim))
		ylim=NULL
	else
		ylim=lim[[i]]
	plot(xl[morto==0],y[morto==0],col='black',ylim=ylim,xlim=c(min(xl),max(xl)),ylab=vars[i],xlab='Idade',type='l')
	points(xl[morto==1],y[morto==1],pch='+',col='red')
	# legend('top',legend=c('vivo','morto'),lty='solid',col=c('black','red'),box.lty=0,horiz=TRUE)	
}
}








