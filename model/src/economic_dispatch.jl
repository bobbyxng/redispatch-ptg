"""
	EconomicDispatch(hours,
					 nodes::Nodes,
					 powerplants::PowerPlants,
					 renewables::Renewables,
					 )

Creates an economic dispatch model from power plants and renewables.
Solves using JuMP. Returns ED_mod, P_opt, P_R_opt, P_R_opt_dist, price.
"""
function EconomicDispatch(hours,
						  nodes::Nodes,
			   		      powerplants::PowerPlants,
						  renewables::Renewables,
			   			  )

	T = hours
	G = powerplants.unit
	R = renewables.unit
	J = nodes.id

	# Initialise JuMP model: Economic dispatch
	ED_mod = JuMP.Model(with_optimizer(Gurobi.Optimizer))

	# Variables
	@variable(ED_mod, P[T, G] >= 0)
	@variable(ED_mod, P_R[T, R] >= 0)

	# Constraints
	# Market clearing/power balance
	@constraint(ED_mod, MarketClearing[t = T],
	    sum(P[t, g] for g in G) +
			sum(P_R[t, r] for r in R) == nodes.systemload[t] -
			sum(nodes.exchange[j][t] for j in J));

	# Upper generation limit
	@constraint(ED_mod, GenerationLimitUp[g = G, t = T],
	    P[t, g] <= powerplants.pmax[g][t]);

	# Lower generation limit
	@constraint(ED_mod, GenerationLimitDown[g = G, t = T],
	    P[t, g] >= powerplants.pmin[g][t]);

	# Upper generation limit for renewables
	@constraint(ED_mod, ResGenerationLimitUp[r = R, t = T],
	    P_R[t, r] <= renewables.infeed[r][t]);

	# # Ramp-up limit
	# @constraint(ED_mod, RampUp[g = G, t = T; t > T[1]],
	# 	P[t, g] - P[t-1, g] <= powerplants.rup[g])
	#
	# # Ramp-down limit
	# @constraint(ED_mod, RampDown[g = G, t = T; t > T[1]],
	# 	-(P[t, g] - P[t-1, g]) <= powerplants.rdn[g])

	# Objective function
	@objective(ED_mod, Min,
		sum(powerplants.mc[g][1] * P[t, g] for g in G, t in T));

	# @objective(ED_mod, Min,
	#     sum(powerplants.mc[g][1] * P[t, g] for g in G, t in T) +
	# 		sum(0 * P_R[t, r] for r in R, t in T));

	# Initiate optimisation process
	JuMP.optimize!(ED_mod)

	# Export results
	P_opt = JuMP.value.(P)
	P_R_opt = JuMP.value.(P_R)
	price = JuMP.dual.(MarketClearing)

	# Correct RES dispatch
	# Total summed RES generation from dispatch for each t
	ResTotalGeneration = JuMP.Containers.DenseAxisArray(zeros(length(T)), T)

	for t in T
		ResTotalGeneration[t] = sum(P_R_opt[t, :])
	end

	# Maximum output possible
	ResMaxGeneration = JuMP.Containers.DenseAxisArray(zeros(length(T)), T)
	for t in T
		ResMaxGeneration[t] = sum(renewables.infeed[r][t] for r in R)
	end

	# Redistribution among RES
	ResUnitGeneration = JuMP.Containers.DenseAxisArray(zeros(length(T), length(R)), T, R)

	for t in T
		for r in R
			ResUnitGeneration[t, r] = (renewables.infeed[r][t] / ResMaxGeneration[t]) *
			ResTotalGeneration[t]
		end
	end

	# Distributed, corrected P_R_opt
	P_R_opt_dist = ResUnitGeneration

	return(ED_mod,
	       P_opt,
		   P_R_opt,
		   P_R_opt_dist,
		   price
	       )
end

"""
	EconomicDispatch(hours,
					 nodes::Nodes,
					 powerplants::PowerPlants,
					 renewables::Renewables,
					 storages::Storages,
					 )

Creates an economic dispatch model from power plants, renewables, and storages.
Initial storage level is fixed to zero. Solves using JuMP. Returns ED_mod,
P_opt, P_R_opt, P_R_opt_dist, P_S_opt, D_S_opt, L_S_opt, price.
"""
### Economic dispatch with renewable energy sources and storages
function EconomicDispatch(hours,
						  nodes::Nodes,
			   		      powerplants::PowerPlants,
						  renewables::Renewables,
						  storages::Storages,
			   			  )

	T = hours
	G = powerplants.unit
	R = renewables.unit
	S = storages.unit
	J = nodes.id

	# Initialise JuMP model: Economic dispatch
	ED_mod = JuMP.Model(with_optimizer(Gurobi.Optimizer))

	# Variables
	@variable(ED_mod, P[T, G] >= 0)
	@variable(ED_mod, P_R[T, R] >= 0)

	# Storage voriables
	@variable(ED_mod, P_S[T, S] >= 0)	# Power generation from storage unit
	@variable(ED_mod, D_S[T, S] >= 0)	# Electricity demand from storage unit
	@variable(ED_mod, L_S[T, S] >= 0)	# Storage level

	# Fix storage level in period 0 to zero
	for s in S
		fix(L_S[T[1], s], 0; force = true)
	end

	# Constraints
	# Market clearing/power balance
	@constraint(ED_mod, MarketClearing[t = T],
	    sum(P[t, g] for g in G) +
		sum(P_R[t, r] for r in R) +
		sum(P_S[t, s] for s in S) -
		sum(D_S[t, s] for s in S)
			== nodes.systemload[t] -
			sum(nodes.exchange[j][t] for j in J));

	# Upper generation limit
	@constraint(ED_mod, GenerationLimitUp[g = G, t = T],
	    P[t, g] <= powerplants.pmax[g][t]);

	# Lower generation limit
	@constraint(ED_mod, GenerationLimitDown[g = G, t = T],
	    P[t, g] >= powerplants.pmin[g][t]);

	# Upper generation limit for renewables
	@constraint(ED_mod, ResGenerationLimitUp[r = R, t = T],
	    P_R[t, r] <= renewables.infeed[r][t]);

	# # Ramp-up limit
	# @constraint(ED_mod, RampUp[g = G, t = T; t > T[1]],
	# 	P[t, g] - P[t-1, g] <= powerplants.rup[g])
	#
	# # Ramp-down limit
	# @constraint(ED_mod, RampDown[g = G, t = T; t > T[1]],
	# 	-(P[t, g] - P[t-1, g]) <= powerplants.rdn[g])

	### Storage constraints
	# Maximum storage power output
	@constraint(ED_mod, StoragePowerOutput[s = S, t = T],
	    P_S[t, s] <= storages.power[s]);

	# Maximum storage power input
	@constraint(ED_mod, StoragePowerInput[s = S, t = T],
	    D_S[t, s] <= storages.power[s]);

	# Generation limit determined by remaining storage level
	@constraint(ED_mod, StorageLevelGen[s = S, t = T],
	    P_S[t, s] <= L_S[t, s]);

	# Storace capacity limit
	@constraint(ED_mod, StorageLevelCap[s = S, t = T],
	    L_S[t, s] <= storages.storage[s]);

	# Storage
	@constraint(ED_mod, Storage[s = S, t = T; t > T[1]],
		L_S[t-1, s] - P_S[t-1, s] + storages.efficiency[s] * D_S[t-1, s] == L_S[t, s]);

	# Objective function
	@objective(ED_mod, Min,
	    sum(powerplants.mc[g][1] * P[t, g] for g in G, t in T));

	# @objective(ED_mod, Min,
	#     sum(powerplants.mc[g][1] * P[t, g] for g in G, t in T) +
	# 		sum(0 * P_R[t, r] for r in R, t in T));

	# Initiate optimisation process
	JuMP.optimize!(ED_mod)

	# Export results
	P_opt = JuMP.value.(P)
	P_R_opt = JuMP.value.(P_R)
	P_S_opt = JuMP.value.(P_S)
	D_S_opt = JuMP.value.(D_S)
	L_S_opt = JuMP.value.(L_S)
	price = JuMP.dual.(MarketClearing)

	# Correct RES dispatch
	# Total summed RES generation from dispatch for each t
	ResTotalGeneration = JuMP.Containers.DenseAxisArray(zeros(length(T)), T)

	for t in T
		ResTotalGeneration[t] = sum(P_R_opt[t, :])
	end

	# Maximum output possible
	ResMaxGeneration = JuMP.Containers.DenseAxisArray(zeros(length(T)), T)
	for t in T
		ResMaxGeneration[t] = sum(renewables.infeed[r][t] for r in R)
	end

	# Redistribution among RES
	ResUnitGeneration = JuMP.Containers.DenseAxisArray(zeros(length(T), length(R)), T, R)

	for t in T
		for r in R
			ResUnitGeneration[t, r] = (renewables.infeed[r][t] / ResMaxGeneration[t]) *
			ResTotalGeneration[t]
		end
	end

	# Distributed, corrected P_R_opt
	P_R_opt_dist = ResUnitGeneration

	return(ED_mod,
	       P_opt,
		   P_R_opt,
		   P_R_opt_dist,
		   P_S_opt,
		   D_S_opt,
		   L_S_opt,
		   price
	       )
end

"""
	EconomicDispatch(hours,
					 nodes::Nodes,
					 powerplants::PowerPlants,
					 renewables::Renewables,
					 storages::Storages,
					 P_S_opt_prev,
					 D_S_opt_prev,
					 L_S_opt_prev,
					 )

Creates an economic dispatch model from power plants, renewables, and storages.
Initial storage level, generation, and demand is initialised  with parameters
from the previous model run. Solves using JuMP. Returns ED_mod, P_opt, P_R_opt,
P_R_opt_dist, P_S_opt, D_S_opt, L_S_opt, price.
"""
function EconomicDispatch(hours,
						  nodes::Nodes,
			   		      powerplants::PowerPlants,
						  renewables::Renewables,
						  storages::Storages,
						  P_S_opt_prev,
						  D_S_opt_prev,
						  L_S_opt_prev,
			   			  )

	T = hours
	G = powerplants.unit
	R = renewables.unit
	S = storages.unit
	J = nodes.id

	# Initialise JuMP model: Economic dispatch
	ED_mod = JuMP.Model(with_optimizer(Gurobi.Optimizer))

	# Variables
	@variable(ED_mod, P[T, G] >= 0)
	@variable(ED_mod, P_R[T, R] >= 0)

	# Storage voriables
	@variable(ED_mod, P_S[T, S] >= 0)	# Power generation from storage unit
	@variable(ED_mod, D_S[T, S] >= 0)	# Electricity demand from storage unit
	@variable(ED_mod, L_S[T, S] >= 0)	# Storage level

	# Fix storage parameters in first hour to the last hour value of the
	# previous day slice.
	for s in S
		fix(P_S[T[1], s], P_S_opt_prev[(T[1]), s]; force = true)
		fix(D_S[T[1], s], D_S_opt_prev[(T[1]), s]; force = true)
		fix(L_S[T[1], s], L_S_opt_prev[(T[1]), s]; force = true)
	end

	# Constraints
	# Market clearing/power balance
	@constraint(ED_mod, MarketClearing[t = T],
	    sum(P[t, g] for g in G) +
		sum(P_R[t, r] for r in R) +
		sum(P_S[t, s] for s in S) -
		sum(D_S[t, s] for s in S)
			== nodes.systemload[t] -
			sum(nodes.exchange[j][t] for j in J));

	# Upper generation limit
	@constraint(ED_mod, GenerationLimitUp[g = G, t = T],
	    P[t, g] <= powerplants.pmax[g][t]);

	# Lower generation limit
	@constraint(ED_mod, GenerationLimitDown[g = G, t = T],
	    P[t, g] >= powerplants.pmin[g][t]);

	# Upper generation limit for renewables
	@constraint(ED_mod, ResGenerationLimitUp[r = R, t = T],
	    P_R[t, r] <= renewables.infeed[r][t]);

	# # Ramp-up limit
	# @constraint(ED_mod, RampUp[g = G, t = T; t > T[1]],
	# 	P[t, g] - P[t-1, g] <= powerplants.rup[g])
	#
	# # Ramp-down limit
	# @constraint(ED_mod, RampDown[g = G, t = T; t > T[1]],
	# 	-(P[t, g] - P[t-1, g]) <= powerplants.rdn[g])

	### Storage constraints
	# Maximum storage power output
	@constraint(ED_mod, StoragePowerOutput[s = S, t = T],
	    P_S[t, s] <= storages.power[s]);

	# Maximum storage power input
	@constraint(ED_mod, StoragePowerInput[s = S, t = T],
	    D_S[t, s] <= storages.power[s]);

	# Generation limit determined by remaining storage level
	@constraint(ED_mod, StorageLevelGen[s = S, t = T],
	    P_S[t, s] <= L_S[t, s]);

	# Storace capacity limit
	@constraint(ED_mod, StorageLevelCap[s = S, t = T],
	    L_S[t, s] <= storages.storage[s]);

	# Storage
	@constraint(ED_mod, Storage[s = S, t = T; t > T[1]],
		L_S[t-1, s] - P_S[t-1, s] + storages.efficiency[s] * D_S[t-1, s] == L_S[t, s]);

	# Objective function
	@objective(ED_mod, Min,
	    sum(powerplants.mc[g][1] * P[t, g] for g in G, t in T));

	# @objective(ED_mod, Min,
	#     sum(powerplants.mc[g][1] * P[t, g] for g in G, t in T) +
	# 		sum(0 * P_R[t, r] for r in R, t in T));

	# Initiate optimisation process
	JuMP.optimize!(ED_mod)

	# Export results
	P_opt = JuMP.value.(P)
	P_R_opt = JuMP.value.(P_R)
	P_S_opt = JuMP.value.(P_S)
	D_S_opt = JuMP.value.(D_S)
	L_S_opt = JuMP.value.(L_S)
	price = JuMP.dual.(MarketClearing)

	# Correct RES dispatch
	# Total summed RES generation from dispatch for each t
	ResTotalGeneration = JuMP.Containers.DenseAxisArray(zeros(length(T)), T)

	for t in T
		ResTotalGeneration[t] = sum(P_R_opt[t, :])
	end

	# Maximum output possible
	ResMaxGeneration = JuMP.Containers.DenseAxisArray(zeros(length(T)), T)
	for t in T
		ResMaxGeneration[t] = sum(renewables.infeed[r][t] for r in R)
	end

	# Redistribution among RES
	ResUnitGeneration = JuMP.Containers.DenseAxisArray(zeros(length(T), length(R)), T, R)

	for t in T
		for r in R
			ResUnitGeneration[t, r] = (renewables.infeed[r][t] / ResMaxGeneration[t]) *
			ResTotalGeneration[t]
		end
	end

	# Distributed, corrected P_R_opt
	P_R_opt_dist = ResUnitGeneration

	return(ED_mod,
	       P_opt,
		   P_R_opt,
		   P_R_opt_dist,
		   P_S_opt,
		   D_S_opt,
		   L_S_opt,
		   price
	       )
end



#### 80%
### Economic dispatch with renewable energy sources and storages
function EconomicDispatch80(hours,
						  nodes::Nodes,
			   		      powerplants::PowerPlants,
						  renewables::Renewables,
						  storages::Storages,
			   			  )

	T = hours
	G = powerplants.unit
	R = renewables.unit
	S = storages.unit
	J = nodes.id

	# Initialise JuMP model: Economic dispatch
	ED_mod = JuMP.Model(with_optimizer(Gurobi.Optimizer))

	# Variables
	@variable(ED_mod, P[T, G] >= 0)
	@variable(ED_mod, P_R[T, R] >= 0)

	# Storage voriables
	@variable(ED_mod, P_S[T, S] >= 0)	# Power generation from storage unit
	@variable(ED_mod, D_S[T, S] >= 0)	# Electricity demand from storage unit
	@variable(ED_mod, L_S[T, S] >= 0)	# Storage level

	# Fix storage level in period 0 to zero
	for s in S
		fix(L_S[T[1], s], 0; force = true)
	end

	# Constraints
	# Market clearing/power balance
	@constraint(ED_mod, MarketClearing[t = T],
	    sum(P[t, g] for g in G) +
		sum(P_R[t, r] for r in R) +
		sum(P_S[t, s] for s in S) -
		sum(D_S[t, s] for s in S)
			== nodes.systemload[t] -
			sum(nodes.exchange[j][t] for j in J));

	# Upper generation limit
	@constraint(ED_mod, GenerationLimitUp[g = G, t = T],
	    P[t, g] <= powerplants.pmax[g][t]);

	# Lower generation limit
	@constraint(ED_mod, GenerationLimitDown[g = G, t = T],
	    P[t, g] >= powerplants.pmin[g][t]);

	# Upper generation limit for renewables
	@constraint(ED_mod, ResGenerationLimitUp[r = R, t = T],
	    P_R[t, r] <= renewables.infeed[r][t]);

	# # Ramp-up limit
	# @constraint(ED_mod, RampUp[g = G, t = T; t > T[1]],
	# 	P[t, g] - P[t-1, g] <= powerplants.rup[g])
	#
	# # Ramp-down limit
	# @constraint(ED_mod, RampDown[g = G, t = T; t > T[1]],
	# 	-(P[t, g] - P[t-1, g]) <= powerplants.rdn[g])

	### Storage constraints
	# Maximum storage power output
	@constraint(ED_mod, StoragePowerOutput[s = S, t = T],
	    P_S[t, s] <= storages.power[s]);

	# Maximum storage power input
	@constraint(ED_mod, StoragePowerInput[s = S, t = T],
	    D_S[t, s] <= storages.power[s]);

	# Generation limit determined by remaining storage level
	@constraint(ED_mod, StorageLevelGen[s = S, t = T],
	    P_S[t, s] <= L_S[t, s]);

	# Storace capacity limit
	@constraint(ED_mod, StorageLevelCap[s = S, t = T],
	    L_S[t, s] <= 0.8*storages.storage[s]);

	# Storage
	@constraint(ED_mod, Storage[s = S, t = T; t > T[1]],
		L_S[t-1, s] - P_S[t-1, s] + storages.efficiency[s] * D_S[t-1, s] == L_S[t, s]);

	# Objective function
	@objective(ED_mod, Min,
	    sum(powerplants.mc[g][1] * P[t, g] for g in G, t in T));

	# @objective(ED_mod, Min,
	#     sum(powerplants.mc[g][1] * P[t, g] for g in G, t in T) +
	# 		sum(0 * P_R[t, r] for r in R, t in T));

	# Initiate optimisation process
	JuMP.optimize!(ED_mod)

	# Export results
	P_opt = JuMP.value.(P)
	P_R_opt = JuMP.value.(P_R)
	P_S_opt = JuMP.value.(P_S)
	D_S_opt = JuMP.value.(D_S)
	L_S_opt = JuMP.value.(L_S)
	price = JuMP.dual.(MarketClearing)

	# Correct RES dispatch
	# Total summed RES generation from dispatch for each t
	ResTotalGeneration = JuMP.Containers.DenseAxisArray(zeros(length(T)), T)

	for t in T
		ResTotalGeneration[t] = sum(P_R_opt[t, :])
	end

	# Maximum output possible
	ResMaxGeneration = JuMP.Containers.DenseAxisArray(zeros(length(T)), T)
	for t in T
		ResMaxGeneration[t] = sum(renewables.infeed[r][t] for r in R)
	end

	# Redistribution among RES
	ResUnitGeneration = JuMP.Containers.DenseAxisArray(zeros(length(T), length(R)), T, R)

	for t in T
		for r in R
			ResUnitGeneration[t, r] = (renewables.infeed[r][t] / ResMaxGeneration[t]) *
			ResTotalGeneration[t]
		end
	end

	# Distributed, corrected P_R_opt
	P_R_opt_dist = ResUnitGeneration

	return(ED_mod,
	       P_opt,
		   P_R_opt,
		   P_R_opt_dist,
		   P_S_opt,
		   D_S_opt,
		   L_S_opt,
		   price
	       )
end


function EconomicDispatch80(hours,
						  nodes::Nodes,
			   		      powerplants::PowerPlants,
						  renewables::Renewables,
						  storages::Storages,
						  P_S_opt_prev,
						  D_S_opt_prev,
						  L_S_opt_prev,
			   			  )

	T = hours
	G = powerplants.unit
	R = renewables.unit
	S = storages.unit
	J = nodes.id

	# Initialise JuMP model: Economic dispatch
	ED_mod = JuMP.Model(with_optimizer(Gurobi.Optimizer))

	# Variables
	@variable(ED_mod, P[T, G] >= 0)
	@variable(ED_mod, P_R[T, R] >= 0)

	# Storage voriables
	@variable(ED_mod, P_S[T, S] >= 0)	# Power generation from storage unit
	@variable(ED_mod, D_S[T, S] >= 0)	# Electricity demand from storage unit
	@variable(ED_mod, L_S[T, S] >= 0)	# Storage level

	# Fix storage parameters in first hour to the last hour value of the
	# previous day slice.
	for s in S
		fix(P_S[T[1], s], P_S_opt_prev[(T[1]), s]; force = true)
		fix(D_S[T[1], s], D_S_opt_prev[(T[1]), s]; force = true)
		fix(L_S[T[1], s], L_S_opt_prev[(T[1]), s]; force = true)
	end

	# Constraints
	# Market clearing/power balance
	@constraint(ED_mod, MarketClearing[t = T],
	    sum(P[t, g] for g in G) +
		sum(P_R[t, r] for r in R) +
		sum(P_S[t, s] for s in S) -
		sum(D_S[t, s] for s in S)
			== nodes.systemload[t] -
			sum(nodes.exchange[j][t] for j in J));

	# Upper generation limit
	@constraint(ED_mod, GenerationLimitUp[g = G, t = T],
	    P[t, g] <= powerplants.pmax[g][t]);

	# Lower generation limit
	@constraint(ED_mod, GenerationLimitDown[g = G, t = T],
	    P[t, g] >= powerplants.pmin[g][t]);

	# Upper generation limit for renewables
	@constraint(ED_mod, ResGenerationLimitUp[r = R, t = T],
	    P_R[t, r] <= renewables.infeed[r][t]);

	# # Ramp-up limit
	# @constraint(ED_mod, RampUp[g = G, t = T; t > T[1]],
	# 	P[t, g] - P[t-1, g] <= powerplants.rup[g])
	#
	# # Ramp-down limit
	# @constraint(ED_mod, RampDown[g = G, t = T; t > T[1]],
	# 	-(P[t, g] - P[t-1, g]) <= powerplants.rdn[g])

	### Storage constraints
	# Maximum storage power output
	@constraint(ED_mod, StoragePowerOutput[s = S, t = T],
	    P_S[t, s] <= storages.power[s]);

	# Maximum storage power input
	@constraint(ED_mod, StoragePowerInput[s = S, t = T],
	    D_S[t, s] <= storages.power[s]);

	# Generation limit determined by remaining storage level
	@constraint(ED_mod, StorageLevelGen[s = S, t = T],
	    P_S[t, s] <= L_S[t, s]);

	# Storace capacity limit
	@constraint(ED_mod, StorageLevelCap[s = S, t = T],
	    L_S[t, s] <= 0.8*storages.storage[s]);

	# Storage
	@constraint(ED_mod, Storage[s = S, t = T; t > T[1]],
		L_S[t-1, s] - P_S[t-1, s] + storages.efficiency[s] * D_S[t-1, s] == L_S[t, s]);

	# Objective function
	@objective(ED_mod, Min,
	    sum(powerplants.mc[g][1] * P[t, g] for g in G, t in T));

	# @objective(ED_mod, Min,
	#     sum(powerplants.mc[g][1] * P[t, g] for g in G, t in T) +
	# 		sum(0 * P_R[t, r] for r in R, t in T));

	# Initiate optimisation process
	JuMP.optimize!(ED_mod)

	# Export results
	P_opt = JuMP.value.(P)
	P_R_opt = JuMP.value.(P_R)
	P_S_opt = JuMP.value.(P_S)
	D_S_opt = JuMP.value.(D_S)
	L_S_opt = JuMP.value.(L_S)
	price = JuMP.dual.(MarketClearing)

	# Correct RES dispatch
	# Total summed RES generation from dispatch for each t
	ResTotalGeneration = JuMP.Containers.DenseAxisArray(zeros(length(T)), T)

	for t in T
		ResTotalGeneration[t] = sum(P_R_opt[t, :])
	end

	# Maximum output possible
	ResMaxGeneration = JuMP.Containers.DenseAxisArray(zeros(length(T)), T)
	for t in T
		ResMaxGeneration[t] = sum(renewables.infeed[r][t] for r in R)
	end

	# Redistribution among RES
	ResUnitGeneration = JuMP.Containers.DenseAxisArray(zeros(length(T), length(R)), T, R)

	for t in T
		for r in R
			ResUnitGeneration[t, r] = (renewables.infeed[r][t] / ResMaxGeneration[t]) *
			ResTotalGeneration[t]
		end
	end

	# Distributed, corrected P_R_opt
	P_R_opt_dist = ResUnitGeneration

	return(ED_mod,
	       P_opt,
		   P_R_opt,
		   P_R_opt_dist,
		   P_S_opt,
		   D_S_opt,
		   L_S_opt,
		   price
	       )
end
