ratings<-read.csv('team_ratings.csv',stringsAsFactors=FALSE)
teams_e<-ratings$ELO.Rating-min(ratings$ELO.Rating-1)  ## <- Works well with ELO ratings (can regenerate winning and loosing score distributions)
teams_e<-teams_e/max(teams_e)
teams_f<-ratings$FIFA..Rating-min(ratings$FIFA..Rating-1)
teams_f<-teams_f/max(teams_f)

w_elo<-0.75
w_fifa<-1-w_elo

teams_a<-((w_elo*teams_e)+(w_fifa*teams_f))/2		## Weighted Avg of fifa and ELO
num_teams<-length(teams_e)

mean_total_score =  1.8

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

sim_match<-function(team1,team2)
{
	N=1000
	sc<-array(dim=c(N,2))
	for(i in 1:N)
	{
		if(i%%2 ==0)
			teams<-teams_e
		else
			teams<-teams_f
	
		team_stats<-array(0,dim=c(num_teams,8))  ##  Pld	W	D 	L 	GF 	GA 	GD 	Pts
	
		team_stats[c(team1,team2),]<-group_match(teams[c(team1,team2)])
		print(team_stats[team1,c(5,6)])
		
		sc[i,]<-team_stats[team1,c(5,6)]
		
	}
	
	hist(sc[,1]-sc[,2])
	return(sc)
}


