
# this file has been copied by the R-package antaresXpansion
#
# this file should have been copied in the temporary folder of 
# current expansion optimisation, i.e.:
# study_path/user/expansion/temp/
#
# this file describes the master problem of the benders decomposition


#-------------
#--- SETS ----
#-------------

# set of MC years
set YEAR ;

# set of weeks
set WEEK ;

#set of benders iterations
set ITERATION ;

#set of simulated years
set SIMULATED_YEARS;

# set of investment candidates
set INV_CANDIDATE;

# set of average bender cuts
set AVG_CUT within {SIMULATED_YEARS, ITERATION} ;

# set of yearly bender cuts
set YEARLY_CUT within {SIMULATED_YEARS, ITERATION, YEAR} ;

# set of weekly bender cuts
set WEEKLY_CUT within {SIMULATED_YEARS, ITERATION, YEAR, WEEK} ;



#-------------------
#--- PARAMETERS ----
#-------------------

# investment candidates
param c_inv{INV_CANDIDATE};      	# investment costs 
param unit_size{INV_CANDIDATE};  	# unit of each investment step
param max_unit{INV_CANDIDATE};	 	# max number of units which can be invested
param relaxed{INV_CANDIDATE} symbolic ;	  # (true or false) is the investment made continuously, or with steps ?

param z0{SIMULATED_YEARS, ITERATION, INV_CANDIDATE} ;# invested capacity of each candidates for the given iteration

# average cut
param c0_avg{AVG_CUT} ;                 	# total costs (operation + investment) for the given iteration
param lambda_avg{AVG_CUT, INV_CANDIDATE} ;	#  rentability (average value over all MC years)

# yearly cut
param c0_yearly{YEARLY_CUT} ;    					# yearly total costs
param lambda_yearly{YEARLY_CUT, INV_CANDIDATE} ;    #  rentability (yearly values)

#weekly cut
param c0_weekly{WEEKLY_CUT} ;   					# weekly total costs
param lambda_weekly{WEEKLY_CUT, INV_CANDIDATE} ;    # rentability (weekly values)

# other
param prob{y in YEAR} := 1/card(YEAR) ; 	# probability of occurence of each MC year

#------------------
#--- VARIABLES ----
#------------------

var Invested_capacity{SIMULATED_YEARS, INV_CANDIDATE} >= 0;       # capacity invested
var N_invested{SIMULATED_YEARS, INV_CANDIDATE} integer >=0;  # number of units invested

var Theta{SIMULATED_YEARS, YEAR, WEEK};


#-----------
#--- LP ----
#-----------

# objective :
minimize master : sum{y in YEAR, s in SIMULATED_YEARS} ( prob[y] * sum{w in WEEK} Theta[s,y,w] )+sum{s in SIMULATED_YEARS, z in INV_CANDIDATE}c_inv[z]*Invested_capacity[s,z];

# description of invested capacity :
subject to bounds_on_invested_capacity_relaxed{s in SIMULATED_YEARS, z in INV_CANDIDATE : relaxed[z] == "true"} : Invested_capacity[s,z] <= max_unit[z] * unit_size[z]; 
		 
subject to bounds_on_invested_capacity_integer{s in SIMULATED_YEARS, z in INV_CANDIDATE : relaxed[z] != "true"} : N_invested[s,z] <= max_unit[z];
subject to integer_constraint{s in SIMULATED_YEARS, z in INV_CANDIDATE : relaxed[z] != "true"} : Invested_capacity[s,z] = unit_size[z] * N_invested[s,z];		 

# bender's cut :
subject to cut_avg{(s,c) in AVG_CUT} : sum{y in YEAR} ( prob[y] * sum{w in WEEK} Theta[s,y,w]) >=   c0_avg[s,c] - sum{z in INV_CANDIDATE}(lambda_avg[s,c,z] * (Invested_capacity[s,z] - z0[s,c,z])) ;

subject to cut_yearly{(s,c,y) in YEARLY_CUT} : sum{w in WEEK} Theta[s,y,w] >=  c0_yearly[s,c,y] - sum{z in INV_CANDIDATE} (lambda_yearly[s,c,y,z] * (Invested_capacity[s,z] - z0[s,c,z]));

subject to cut_weekly{(s,c,y,w) in WEEKLY_CUT} : Theta[s,y,w] >=  c0_weekly[s,c,y,w] - sum{z in INV_CANDIDATE} (lambda_weekly[s,c,y,w,z] * (Invested_capacity[s,z] - z0[s,c,z]));
