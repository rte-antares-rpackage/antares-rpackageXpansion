
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

# set of investment candidates
set INV_CANDIDATE ;

# set of MC years
set YEAR ;

# set of weeks
set WEEK ;

#set of benders iterations
set ITERATION ordered;

# set of average bender cuts
set AVG_CUT within {ITERATION} ;

# set of yearly bender cuts
set YEARLY_CUT within {ITERATION, YEAR} ;

# set of weekly bender cuts
set WEEKLY_CUT_ALL within {ITERATION, YEAR, WEEK} ;


#-------------
#--- SETS ----
#-------------

param prm_solver symbolic;  # solver


#-------------------
#--- PARAMETERS ----
#-------------------

# investment candidates
param c_inv{INV_CANDIDATE};      	# investment costs 
param unit_size{INV_CANDIDATE};  	# unit of each investment step
param max_unit{INV_CANDIDATE};	 	# max number of units which can be invested
param relaxed{INV_CANDIDATE} symbolic ;	  # (true or false) is the investment made continuously, or with steps ?

param z0{ITERATION, INV_CANDIDATE} ;# invested capacity of each candidates for the given iteratoin

# average cut
param c0_avg{AVG_CUT} ;                 	# total costs (operation + investment) for the given iteration
param lambda_avg{AVG_CUT, INV_CANDIDATE} ;	#  rentability (average value over all MC years)

# yearly cut
param c0_yearly{YEARLY_CUT} ;    					# yearly total costs
param lambda_yearly{YEARLY_CUT, INV_CANDIDATE} ;    #  rentability (yearly values)

#weekly cut
param c0_weekly{WEEKLY_CUT_ALL} ;   					# weekly total costs
param lambda_weekly{WEEKLY_CUT_ALL, INV_CANDIDATE} ;    # rentability (weekly values)

# other
param prob{YEAR}; 	# probability of occurence of each MC year



#-------------------------------------
#--- SETS of CUTS WITHOUT DOUBLON ----
#-------------------------------------

param tol := 1e-3;  # tolerance to identify doublons

param cut_in_zero{(c,y,w) in WEEKLY_CUT_ALL} := c0_weekly[c,y,w] - sum{z in INV_CANDIDATE} (lambda_weekly[c,y,w,z] * (0 - z0[c,z]));

param difference{c1 in ITERATION, c2 in ITERATION, y in YEAR, w in WEEK} := sum{z in INV_CANDIDATE} (lambda_weekly[c1,y,w,z] - lambda_weekly[c2,y,w,z])^2;
param isdoublon{(c1,y,w) in WEEKLY_CUT_ALL} := if ((min{(c2, y, w) in WEEKLY_CUT_ALL} difference[c1, c2, y, w]) < tol) then 1 else 0;
param deleteit1{(c1,y,w) in WEEKLY_CUT_ALL} := if ((isdoublon[c1,y,w]) == 1 and ((max{(c2, y, w) in WEEKLY_CUT_ALL : difference[c1, c2, y, w] < tol} cut_in_zero[c2,y,w]) > cut_in_zero[c1, y, w])) then 1 else 0;
param deleteit2{(c1,y,w) in WEEKLY_CUT_ALL} := if ((isdoublon[c1,y,w]) == 1 and (deleteit1[c1,y,w] == 0) and (card{(c2,y,w) in WEEKLY_CUT_ALL : (ord(c2, ITERATION) > ord(c1, ITERATION)) and (deleteit1[c2,y,w] == 0) and (difference[c1, c2, y, w] < tol)} >= 1)) then 1 else 0;

set WEEKLY_CUT := setof{(c, y, w) in WEEKLY_CUT_ALL : deleteit1[c,y,w] == 0 and deleteit2[c,y,w] ==0} (c, y, w);


#------------------
#--- VARIABLES ----
#------------------

var Invested_capacity{INV_CANDIDATE} >= 0;       # capacity invested
var N_invested{INV_CANDIDATE} integer >=0;  # number of units invested

var Theta{YEAR, WEEK};


#-----------
#--- LP ----
#-----------

# objective :
minimize master : sum{y in YEAR} ( prob[y] * sum{w in WEEK} Theta[y,w]) ;

# description of invested capacity :
subject to bounds_on_invested_capacity_relaxed{z in INV_CANDIDATE : relaxed[z] == "true"} : Invested_capacity[z] <= max_unit[z] * unit_size[z]; 
		 
subject to bounds_on_invested_capacity_integer{z in INV_CANDIDATE : relaxed[z] != "true"} : N_invested[z] <= max_unit[z];
subject to integer_constraint{z in INV_CANDIDATE : relaxed[z] != "true"} : Invested_capacity[z] = unit_size[z] * N_invested[z];		 

# bender's cut :
subject to cut_avg{c in AVG_CUT} : sum{y in YEAR} ( prob[y] * sum{w in WEEK} Theta[y,w]) >=   c0_avg[c] - sum{z in INV_CANDIDATE}(lambda_avg[c,z] * (Invested_capacity[z] - z0[c,z])) ;

subject to cut_yearly{(c,y) in YEARLY_CUT} : sum{w in WEEK} Theta[y,w] >=  c0_yearly[c,y] - sum{z in INV_CANDIDATE} (lambda_yearly[c,y,z] * (Invested_capacity[z] - z0[c,z]));

subject to cut_weekly{(c,y,w) in WEEKLY_CUT} : Theta[y,w] >=  c0_weekly[c,y,w] - sum{z in INV_CANDIDATE} (lambda_weekly[c,y,w,z] * (Invested_capacity[z] - z0[c,z]));

