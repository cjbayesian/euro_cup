############### Kaggle Submission (take on the Quants) 	#################
############### Author: Corey Chivers June,2012		#################
## Update EURO2012

rm(list=ls())


ratings<-read.csv('data/team_ratings.csv',stringsAsFactors=FALSE)
teams_e<-ratings$ELO.Rating-min(ratings$ELO.Rating-1)  ## <- Works well with ELO ratings (can regenerate winning and loosing score distributions)
teams_e<-(teams_e)
teams_e<-teams_e/max(teams_e)

num_teams<-length(teams_e)

mean_total_score =  2.57  	## from data on recent games at: http://www.eloratings.net/world.html
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

	
y<-hist(final_game,plot=FALSE,breaks=seq(0.5,16.5,1,))


png('output/pov.png')

par(cex=1.1,mar=c(4,4,2,1))
plot(y$density,ratings$ELO.Rating,
   pch=20,
   xlab='Probability of Victory',
   ylab='ELO Rating',
   xlim=c(0,0.35))

   text(y$density+0.035,ratings$ELO.Rating,labels=ratings$Country)
 
dev.off()


