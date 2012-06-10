############### Kaggle Submission (take on the Quants) 	#################
############### Author: Corey Chivers June,2012		#################
## Update EURO2012

rm(list=ls())

#for(mean_total_score in c(0.1,0.5,1,2.57))
#{
ratings<-read.csv('data/team_ratings.csv',stringsAsFactors=FALSE)
teams_e<-ratings$ELO.Rating-min(ratings$ELO.Rating-1)  ## <- Works well with ELO ratings (can regenerate winning and loosing score distributions)
teams_e<-(teams_e)
teams_e<-teams_e/max(teams_e)

num_teams<-length(teams_e)


mean_total_score =  2.571429  	## from data on recent games at: http://www.eloratings.net/world.html
					## Teams score goals in matches according to a poison distribution with 
					## mean equal to their strength ratio (derived from ELO or FIFA rating)
					## times <mean_total_score>


teams<-teams_e

   group_match<-function(teams)
   {
	   ratio<-teams[1]/(teams[1]+teams[2])
	   strength<-1:2
	   strength[1]<-ratio*mean_total_score
	   strength[2]<-(1-ratio)*mean_total_score
	
	   score<-array(dim=2)
	   score[1]<-rpois(1,strength[1])
	   score[2]<-rpois(1,strength[2])
	

	    ##  1.Pld	2.W	3.D 	4.L 	5.GF 	6.GA 	7.GD 	8.Pts ##
	   stat<-array(1,dim=c(2,8))
	   if(score[1]>score[2])
	   {
		   stat[2,2]=0
		   stat[1,3]=0
		   stat[2,3]=0
		   stat[1,4]=0
		   stat[1,8]=3
		   stat[2,8]=0
	   }
	   else if(score[2]>score[1])
	   {
		   stat[1,2]=0
		   stat[1,3]=0
		   stat[2,3]=0
		   stat[2,4]=0
		   stat[2,8]=3
		   stat[1,8]=0
	   }
	   else ## Draw
	   {
		   stat[1,2]=0
		   stat[2,2]=0
		   stat[1,4]=0
		   stat[2,4]=0
		   stat[1,8]=1
		   stat[2,8]=1
	   }

	   #GF
		   stat[1,5]=score[1]
		   stat[2,5]=score[2]
	   #GA
		   stat[1,6]=score[2]
		   stat[2,6]=score[1]
	   #GD
		   stat[1,7]=stat[1,5]-stat[1,6]
		   stat[2,7]=stat[2,5]-stat[2,6]

	   return(stat)
   }

finals_match<-function(teams)
{
	ratio<-teams[1]/(teams[1]+teams[2])
	strength<-1:2
	strength[1]<-ratio*mean_total_score
	strength[2]<-(1-ratio)*mean_total_score
	
	score<-array(dim=2)
	max=100
	for(i in 1:max) ## Keep playing until someone wins ##
	{
		score[1]<-rpois(1,strength[1])
		score[2]<-rpois(1,strength[2])
	
		if(score[1]>score[2])
			return(1)
		if(score[2]>score[1])
			return(2)
	}
			print('Tie not broken!')
}


#### Simulate Tournament Outcomes #########
matches<-as.matrix(read.table('data/matches.dat'))
matches_in_group<-6
groups<-4
teams_in_group<-4
num_matches<-matches_in_group*groups
N=10000
winner_in_group<-array(dim=c(N,groups))
group_rank<-array(dim=c(N,groups,teams_in_group))
team_rank<-array(0,dim=c(N,num_teams))
top<-array(dim=c(N,groups/2))
bottom<-array(dim=c(N,groups/2))
quarters<-array(dim=c(N,groups))
semis<-array(dim=c(N,2))
final_game<-array(dim=N)


for(i in 1:N)
{

 m=1
 s<-1
 f<-1
 team_stats<-array(0,dim=c(num_teams,8))  ##  Pld	W	D 	L 	GF 	GA 	GD 	Pts

 for(g in 1:groups)
 {
   for(ma in 1:matches_in_group)
   {
    team_stats[matches[m,],]<-group_match(teams[matches[m,]])+team_stats[matches[m,],]
    m=m+1
   }
	f<-g*teams_in_group
	## ranking as per tie breaking rules in Group round
	group_rank[i,g,]<-(s-1) + order(team_stats[s:f,8],team_stats[s:f,7], team_stats[s:f,5] ,decreasing=TRUE)
	for(t in s:f)
		team_rank[i,t]<-which(group_rank[i,g,]==t)

	winner_in_group[i,g]<- group_rank[i,g,1]
	s<-f+1

 }	
	m=1
  if(F) #Straight to semis
   {
	   for(g in seq(1,groups-1,2))	## Round 16 (top)
	   {
		   cur_teams<-c(group_rank[i,g,1],group_rank[i,g+1,2])
		   cur_match<-teams[ cur_teams ]
		   top[i,m]<-cur_teams[finals_match(cur_match)]
		   m=m+1
	   }
	   m=1
	   for(g in seq(2,groups,2))	## (bottom)
	   {	
		   cur_teams<-c(group_rank[i,g,1],group_rank[i,g-1,2])
		   cur_match<-teams[ cur_teams ]
		   bottom[i,m]<-cur_teams[finals_match(cur_match)]
		   m=m+1
	   }
   }

	game=1				## Quarters
	for(g in c(1,3))
	{
		cur_teams<-c(group_rank[i,g,1],group_rank[i,g+1,2])
		cur_match<-teams[ cur_teams ]
		quarters[i,game]<-cur_teams[finals_match(cur_match)]
		game=game+1
	}
	for(g in c(1,3))
	{
		cur_teams<-c(group_rank[i,g+1,1],group_rank[i,g,2])
		cur_match<-teams[ cur_teams ]
		quarters[i,game]<-cur_teams[finals_match(cur_match)]
		game=game+1
	}
	game=1
	for(g in c(1,3))		## Semi-finals
	{
		cur_teams<-c(quarters[i,g],quarters[i,g+1])
		cur_match<-teams[ cur_teams ]
		semis[i,game]<-cur_teams[finals_match(cur_match)]
		game=game+1
	}
	
	cur_teams<-c(semis[i,1],semis[i,2])	## FINALS!!##
	cur_match<-teams[ cur_teams ]
	final_game[i]<-cur_teams[finals_match(cur_match)]
}




#################################### Summarize Predictions ######################################



	## Group Stage ##
	in_group<-array(0,dim=c(4))
	c_3<-array(0,dim=num_teams) ## confidence in 3rd in group or better
	c_2<-array(0,dim=num_teams) ## confidence in 2nd in group or better (final 16)
	eliminated<-NULL
	third_in_group<-NULL
	fourth_in_group<-NULL
	third_conf<-NULL

	for(g in 1:groups)
	{
		for(t in 1:4)
		{
			in_group[t]<-sum(team_rank[,teams_in_group*(g-1)+t])
			for(i in 1:N)
			{
				if(team_rank[i,teams_in_group*(g-1)+t] < 4) ## how often are they not last
					c_3[teams_in_group*(g-1)+t]<-c_3[teams_in_group*(g-1)+t]+1
		
				if(team_rank[i,teams_in_group*(g-1)+t] < 3) ## how ofter are they not in the last two
					c_2[teams_in_group*(g-1)+t]<-c_2[teams_in_group*(g-1)+t]+1
			}
		}
		## To get confidence in 3rd rank teams, calculate how often they are AT LEAST third in group
		##
		r<-sort(in_group,decreasing=TRUE,index.return=TRUE)$ix
		
		eliminated<-c(eliminated,r[1]+teams_in_group*(g-1),r[2]+teams_in_group*(g-1))

		third_in_group<-c(third_in_group,r[2]+teams_in_group*(g-1))
		fourth_in_group<-c(fourth_in_group,r[1]+teams_in_group*(g-1))
	
		third_conf<-c(third_conf,c_3[r[2]+teams_in_group*(g-1)]/N)
		
	}
if(F)
{	
	## Last 16 ## 
	h_16<-hist(c(top[,1:4],bottom[,1:4]),plot=FALSE,breaks=seq(0.5,32.5,1,))
	rank<-sort(h_16$counts,index.return=TRUE)$ix
	last_16<-array(dim=8)	
	last_16_conf<-array(dim=8)
	
	n=1
	i=1
	while(n<=8)
	{
		if(rank[i] %in% eliminated)
			i<-i+1
		else
		{
			last_16[n]<-rank[i]
			eliminated<-c(eliminated,last_16[n])
			last_16_conf[n]<-c_2[rank[i]]/N
			i<-i+1
			n<-n+1
		}
	}
			#print(ratings$Country[last_16])
			#print(last_16_conf)
}	

	## Quarters ##
	h_Q<-hist(c(quarters[,1:4]),plot=FALSE,breaks=seq(0.5,32.5,1,))
	rank<-sort(h_Q$counts,index.return=TRUE)$ix
	Qs<-array(dim=4)	
	Qs_conf<-array(dim=4)
	
	n=1
	i=1
	while(n<=4)
	{
		if(rank[i] %in% eliminated)
			i<-i+1
		else
		{
			Qs[n]<-rank[i]
			eliminated<-c(eliminated,Qs[n])
			
			Qs_conf[n]<-0 #h_16$counts[rank[i]]/N
			i<-i+1
			n<-n+1
		}
	}
			#print(ratings$Country[Qs])
			#print(Qs_conf)

	
	## Semis ##
	h_S<-hist(c(semis[,1:2]),plot=FALSE,breaks=seq(0.5,32.5,1,))
	rank<-sort(h_S$counts,index.return=TRUE)$ix
	Ss<-array(dim=2)	
	Ss_conf<-array(dim=2)
	
	n=1
	i=1
	while(n<=2)
	{
		if(rank[i] %in% eliminated)
			i<-i+1
		else
		{
			Ss[n]<-rank[i]
			eliminated<-c(eliminated,Ss[n])
			
			Ss_conf[n]<-h_Q$counts[rank[i]]/N
			i<-i+1
			n<-n+1
		}
	}
			#print('3rd and 4th')
			#print(ratings$Country[Ss])
			#print(Ss_conf)
	
	## FINALS ##
	h_F<-hist(final_game,plot=FALSE,breaks=seq(0.5,32.5,1,))
	rank<-sort(h_F$counts,index.return=TRUE)$ix
	Fs<-array(dim=1)	
	Fs_conf<-array(dim=1)
	
	n=1
	i=1
	while(n<=1)
	{
		if(rank[i] %in% eliminated)
			i<-i+1
		else
		{
			Fs[n]<-rank[i]
			eliminated<-c(eliminated,Fs[n])
			
			Fs_conf[n]<-h_S$counts[rank[i]]/N
			i<-i+1
			n<-n+1
		}
	}
			#print('Second')
			#print(ratings$Country[Fs])
			#print(Fs_conf)
	
	
	y<-hist(final_game,plot=FALSE,breaks=seq(0.5,32.5,1,))
	#print(paste('1st', ratings$Country[which.max(y$counts)],'conf:',length(which(final_game==which.max(y$counts)) ) /N) )





############################## Write Submission Files #########################################
Position<-c('Winner','2nd or better','3rd or better','4th or better',rep('Quarter final or better',4),rep('Last 16',8),rep('3rd in group or better',8),rep('4th in group or better',8))

predicted_teams<-c(which.max(y$counts), Fs, Ss, Qs, third_in_group, fourth_in_group)

Code<-array('t',dim=num_teams)

for(t in 1:num_teams)
	Code[t]<-ratings$Country[predicted_teams[t]]

Confidence<-(c(length(which(final_game==which.max(y$counts)) ) /N,Fs_conf,Ss_conf,Qs_conf,third_conf,rep(1,8)))*100

	print(cbind(Position,Code,Confidence))


file_name<-paste('output/Corey_Chivers_QuantsWORLDCUP','_',mean_total_score,'.csv',sep='')
write.csv(cbind(Position,Code,formatC(Confidence,digits=3)),file=file_name,row.names=FALSE)
################################################################################################















############################################################################
############################### PLOTS ######################################
############################################################################

par(mfrow=c(4,2))
for(g in 1:groups) ## Histograms of winning frequencies at the groups stage ##
	hist(winner_in_group[,g])
	


plot_file<-paste('output/preds','_',mean_total_score,'.pdf',sep='')
pdf(plot_file)

par(mfrow=c(2,1),mar=c(1,1,1,1),xaxt='n')
if(F)
{
for(i in seq(1,3,2))
{
	h<-hist(c(top[,i],top[,i+1],bottom[,i],bottom[,i+1]),breaks=seq(0.5,32.5,1,),xlab='Country',col='blue',main='Round of 16',probability=TRUE)
	points(teams/sum(teams),pch=14,cex=0.5)
	points(teams_f/sum(teams_f),pch=15,cex=0.5,col='green')
	for(t in 1:num_teams)
		text(t,h$density[t]+0.01,labels=ratings$Country[t],srt=75,cex=0.5)
}

}
par(mfrow=c(2,1),mar=c(1,1,1,1),xaxt='n')
for(i in c(1,2))
{	## combined forecasts of each side of the tournament 
	h<-hist(c(quarters[,i],quarters[,i+2]),breaks=seq(0.5,32.5,1,),xlab='Country',col='blue',main='Quarters',probability=TRUE)
	points(teams/sum(teams),pch=14,cex=0.5)
	#points(teams_f/sum(teams_f),pch=15,cex=0.5,col='green')
	for(t in 1:num_teams)
		text(t,h$density[t]+0.01,labels=ratings$Country[t],srt=75,cex=0.5)
}


par(mfrow=c(1,1),mar=c(1,1,1,1),mgp=c(1,10,10),xaxt='n')

	## combined forecasts of each side of the tournament 
	h<-hist(c(semis[,1],semis[,2]),breaks=seq(0.5,32.5,1,),xlab='Country',col='blue',main='Semi-Finals',probability=TRUE)
	points(teams/sum(teams),pch=14,cex=0.5)
	#points(teams_f/sum(teams_f),pch=15,cex=0.5,col='green')
	for(t in 1:num_teams)
		text(t,h$density[t]+0.01,labels=ratings$Country[t],srt=75,cex=0.5)



par(mfrow=c(1,1),mgp=c(1,10,10))
h<-hist(final_game,breaks=seq(0.5,32.5,1,),xlab='Country',col='blue',main='Final',probability=TRUE)
points(teams/sum(teams),pch=14,cex=0.5)
#points(teams_f/sum(teams_f),pch=15,cex=0.5,col='green')
for(t in 1:num_teams)
	text(t,h$density[t]+0.006,labels=ratings$Country[t],srt=75,cex=0.5)


dev.off()


#}
