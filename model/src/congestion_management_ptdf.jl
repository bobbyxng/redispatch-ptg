### Congestion management with renewable energy sources and storages
function CongestionManagement(hours,
							  nodes::Nodes,
							  lines::Lines,
							  powerplants::PowerPlants,
							  renewables::Renewables,
							  storages::Storages,
							  price,
							  P_opt,
							  P_R_opt,
							  P_S_opt,
							  D_S_opt,
						      )

	h,b = calc_h_b(lines, nodes)

	T = hours
	G = powerplants.unit
	R = renewables.unit
	S = storages.unit

	slack = nodes.slack
	Sbase = nodes.bmva # MVA

	###################
	# MODEL ###########
	###################

	# Create a subset of buses I and J
	J = nodes.id
	L = lines.id

	# Initialise JuMP model: Congestion management
	CM_mod = JuMP.Model(with_optimizer(Gurobi.Optimizer))

	# Variables
	@variable(CM_mod, ΔP_up[T, G] >= 0)
	@variable(CM_mod, ΔP_dn[T, G] >= 0)
	@variable(CM_mod, ΔP_R_up[T, R] == 0)
	@variable(CM_mod, ΔP_R_dn[T, R] >= 0)
	@variable(CM_mod, Θ[T, J])
	@variable(CM_mod, P_flow[T, L])
	@variable(CM_mod, P_load_lost[T, J] >= 0)
	@variable(CM_mod, P_gen_lost[T, J] >= 0)

	# Fix the voltage angle of the slack bus
	for t in T
		fix(Θ[t, slack], 0)
	end

	# Constraints
	# Market clearing/power balance
	# @constraint(CM_mod, MarketClearing[t = T],
	#     sum((P_opt[t, g]/Sbase + ΔP_up[t, g] - ΔP_dn[t, g]) for g in G) +
	# 	sum((P_R_opt[t, r]/Sbase + ΔP_R_up[t, r] - ΔP_R_dn[t, r]) for r in R) +
	# 	sum(P_S_opt[t, s]/Sbase for s in S) -
	# 	sum(D_S_opt[t, s]/Sbase for s in S)
	# 		== nodes.systemload[t]/Sbase);

	# Upper generation limit
	@constraint(CM_mod, GenerationLimitUp[g = G, t = T],
	    (P_opt[t, g]/Sbase + ΔP_up[t, g] - ΔP_dn[t, g]) <=
			powerplants.pmax[g][t]/Sbase);

	# Lower generation limit
	@constraint(CM_mod, GenerationLimitDown[g = G, t = T],
	    (P_opt[t, g]/Sbase + ΔP_up[t, g] - ΔP_dn[t, g]) >=
			powerplants.pmin[g][t]/Sbase);

	# Upper generation limit for renewables
	@constraint(CM_mod, ResGenerationLimitUp[r = R, t = T],
	    (P_R_opt[t, r]/Sbase + ΔP_R_up[t, r] - ΔP_R_dn[t, r]) <=
			renewables.infeed[r][t]/Sbase);

	# Lower generation limit for renewables
	@constraint(CM_mod, ResGenerationLimitDown[r = R, t = T],
	    (P_R_opt[t, r]/Sbase + ΔP_R_up[t, r] - ΔP_R_dn[t, r]) >=
			0);

	# # Ramp-up limit
	# @constraint(CM_mod, RampUp[g = G, t = T; t > T[1]],
	# 	(P_opt[t, g]/Sbase + ΔP_up[t, g] - ΔP_dn[t, g]) -
	# 		(P_opt[t-1, g]/Sbase + ΔP_up[t-1, g] - ΔP_dn[t-1, g]) <=
	# 		powerplants.rup[g]/Sbase)
	#
	# # Ramp-down limit
	# @constraint(CM_mod, RampDown[g = G, t = T; t > T[1]],
	# 	-((P_opt[t, g]/Sbase + ΔP_up[t, g] - ΔP_dn[t, g]) -
	# 		(P_opt[t-1, g]/Sbase + ΔP_up[t-1, g] - ΔP_dn[t-1, g])) <=
	# 		powerplants.rdn[g]/Sbase)

	### DC power flow constraints
	# Power injection balance
	@constraint(CM_mod, PowerInjectionBal[j = J, t = T],
		sum((P_opt[t, g]/Sbase + ΔP_up[t, g] - ΔP_dn[t, g])
			for g in getKeyVector(powerplants.node, j)) +
		sum((P_R_opt[t, r]/Sbase + ΔP_R_up[t, r] - ΔP_R_dn[t, r])
			for r in getKeyVector(renewables.node, j)) +
		sum((P_S_opt[t, s]/Sbase) for s in getKeyVector(storages.node, j)) -
		sum((D_S_opt[t, s]/Sbase) for s in getKeyVector(storages.node, j)) -
		P_gen_lost[t, j] +
		sum(b[j,jj] * Θ[t, jj] for jj in J) ==
		nodes.load[j][t]/Sbase - nodes.exchange[j][t]/Sbase - P_load_lost[t, j]);

		@constraint(CM_mod, LinePowerFlowMax[l = L, t = T],
			P_flow[t, l] <= lines.pmax[l]/Sbase);

		@constraint(CM_mod, LinePowerFlowMin[l = L, t = T],
			P_flow[t, l] >= -lines.pmax[l]/Sbase);

		@constraint(CM_mod, LineFlow[l = L, t = T],
			P_flow[t, l] == sum(h[l,j] * Θ[t,j] for j in J));

	# Objective function
	@objective(CM_mod, Min,
	    sum(powerplants.mc[g][1] * (ΔP_up[t, g]) * Sbase +
			(price[t] - powerplants.mc[g][1]) * ΔP_dn[t, g] *
				Sbase for g in G, t in T) +
		sum(0 * (ΔP_R_up[t, r]) * Sbase +
			(price[t] - 0) * ΔP_R_dn[t, r] *
				Sbase for r in R, t in T) +
		1000 * sum(P_load_lost[t, j] * Sbase for j in J, t in T) +
		1000 * sum(P_gen_lost[t, j] * Sbase for j in J, t in T));

	# Initiate optimisation process
	JuMP.optimize!(CM_mod)


	# Export results
	ΔP_up_opt = JuMP.value.(ΔP_up) * Sbase
	ΔP_dn_opt = JuMP.value.(ΔP_dn) * Sbase
	ΔP_R_up_opt = JuMP.value.(ΔP_R_up) * Sbase
	ΔP_R_dn_opt = JuMP.value.(ΔP_R_dn) * Sbase
	Θ_opt = (JuMP.value.(Θ)) * 360/(2π)
	P_flow_opt = JuMP.value.(P_flow)*Sbase
	price = JuMP.dual.(PowerInjectionBal)
	P_gen_lost_opt = JuMP.value.(P_gen_lost) * Sbase
	P_load_lost_opt = JuMP.value.(P_load_lost) * Sbase

	return(CM_mod,
	       ΔP_up_opt,
		   ΔP_dn_opt,
		   ΔP_R_up_opt,
		   ΔP_R_dn_opt,
	       Θ_opt,
	       P_flow_opt,
		   price,
		   P_gen_lost_opt,
		   P_load_lost_opt
	       )
end

function CongestionManagementPHS(hours,
								 nodes::Nodes,
								 lines::Lines,
								 powerplants::PowerPlants,
								 renewables::Renewables,
								 storages::Storages,
								 price,
								 P_opt,
								 P_R_opt,
								 P_S_opt,
								 D_S_opt,
								 L_S_opt,
							     )

	h,b = calc_h_b(lines, nodes)

	T = hours
	G = powerplants.unit
	R = renewables.unit
	S = storages.unit

	slack = nodes.slack
	Sbase = nodes.bmva # MVA

	###################
	# MODEL ###########
	###################

	# Create a subset of buses I and J
	J = nodes.id
	L = lines.id

	# Initialise JuMP model: Congestion management
	CM_mod = JuMP.Model(with_optimizer(Gurobi.Optimizer))

	# Variables
	@variable(CM_mod, ΔP_up[T, G] >= 0)
	@variable(CM_mod, ΔP_dn[T, G] >= 0)
	@variable(CM_mod, ΔP_R_up[T, R] == 0)
	@variable(CM_mod, ΔP_R_dn[T, R] >= 0)
	@variable(CM_mod, Θ[T, J])
	@variable(CM_mod, P_flow[T, L])
	@variable(CM_mod, P_load_lost[T, J] >= 0)
	@variable(CM_mod, P_gen_lost[T, J] >= 0)

	# Storage voriables
	@variable(CM_mod, P_S_up[T, S] >= 0)	# Power generation from storage unit
	# @variable(ED_mod, D_S[T, S] >= 0)	# Electricity demand from storage unit
	@variable(CM_mod, L_S[T, S] >= 0)

	# Fix the voltage angle of the slack bus
	for t in T
		fix(Θ[t, slack], 0)
	end

	# Fix storage level in period 0 to zero
	if hours[end] <= 24
		for s in S
			fix(L_S[T[1], s], 0; force = true)
		end
	else
		for s in S
			fix(L_S[T[1], s], L_S_opt[(T[1]), s]; force = true)
		end
	end

	# # Fix storage parameters in first hour to the last hour value of the
	# # previous day slice.
	# for s in S
	# 	fix(L_S[T[1], s], L_S_opt[(T[end]), s]/Sbase; force = true)
	# end


	# Constraints
	# Market clearing/power balance
	# @constraint(CM_mod, MarketClearing[t = T],
	#     sum((P_opt[t, g]/Sbase + ΔP_up[t, g] - ΔP_dn[t, g]) for g in G) +
	# 	sum((P_R_opt[t, r]/Sbase + ΔP_R_up[t, r] - ΔP_R_dn[t, r]) for r in R) +
	# 	sum(P_S_opt[t, s]/Sbase for s in S) -
	# 	sum(D_S_opt[t, s]/Sbase for s in S)
	# 		== nodes.systemload[t]/Sbase);

	# Upper generation limit
	@constraint(CM_mod, GenerationLimitUp[g = G, t = T],
	    (P_opt[t, g]/Sbase + ΔP_up[t, g] - ΔP_dn[t, g]) <=
			powerplants.pmax[g][t]/Sbase);

	# Lower generation limit
	@constraint(CM_mod, GenerationLimitDown[g = G, t = T],
	    (P_opt[t, g]/Sbase + ΔP_up[t, g] - ΔP_dn[t, g]) >=
			powerplants.pmin[g][t]/Sbase);

	# Upper generation limit for renewables
	@constraint(CM_mod, ResGenerationLimitUp[r = R, t = T],
	    (P_R_opt[t, r]/Sbase + ΔP_R_up[t, r] - ΔP_R_dn[t, r]) <=
			renewables.infeed[r][t]/Sbase);

	# Lower generation limit for renewables
	@constraint(CM_mod, ResGenerationLimitDown[r = R, t = T],
	    (P_R_opt[t, r]/Sbase + ΔP_R_up[t, r] - ΔP_R_dn[t, r]) >=
			0);

	# # Ramp-up limit
	# @constraint(CM_mod, RampUp[g = G, t = T; t > T[1]],
	# 	(P_opt[t, g]/Sbase + ΔP_up[t, g] - ΔP_dn[t, g]) -
	# 		(P_opt[t-1, g]/Sbase + ΔP_up[t-1, g] - ΔP_dn[t-1, g]) <=
	# 		powerplants.rup[g]/Sbase)
	#
	# # Ramp-down limit
	# @constraint(CM_mod, RampDown[g = G, t = T; t > T[1]],
	# 	-((P_opt[t, g]/Sbase + ΔP_up[t, g] - ΔP_dn[t, g]) -
	# 		(P_opt[t-1, g]/Sbase + ΔP_up[t-1, g] - ΔP_dn[t-1, g])) <=
	# 		powerplants.rdn[g]/Sbase)

	### DC power flow constraints
	# Power injection balance
	@constraint(CM_mod, PowerInjectionBal[j = J, t = T],
		sum((P_opt[t, g]/Sbase + ΔP_up[t, g] - ΔP_dn[t, g])
			for g in getKeyVector(powerplants.node, j)) +
		sum((P_R_opt[t, r]/Sbase + ΔP_R_up[t, r] - ΔP_R_dn[t, r])
			for r in getKeyVector(renewables.node, j)) +
		sum(((P_S_opt[t, s]/Sbase) + P_S_up[t, s]) for s in getKeyVector(storages.node, j)) -
		sum((D_S_opt[t, s]/Sbase) for s in getKeyVector(storages.node, j)) -
		P_gen_lost[t, j] +
		sum(b[j,jj] * Θ[t, jj] for jj in J) ==
		nodes.load[j][t]/Sbase - nodes.exchange[j][t]/Sbase - P_load_lost[t, j]);

		@constraint(CM_mod, LinePowerFlowMax[l = L, t = T],
			P_flow[t, l] <= lines.pmax[l]/Sbase);

		@constraint(CM_mod, LinePowerFlowMin[l = L, t = T],
			P_flow[t, l] >= -lines.pmax[l]/Sbase);

		@constraint(CM_mod, LineFlow[l = L, t = T],
			P_flow[t, l] == sum(h[l,j] * Θ[t,j] for j in J));

	### PHS constraints
	@constraint(CM_mod, StoragePowerOutput[s = S, t = T],
 		P_S_opt[t, s]/Sbase + P_S_up[t, s] <= storages.power[s]/Sbase);

	# Maximum storage power input
	# @constraint(CM_mod, StoragePowerInput[s = S, t = T],
	# 	D_S[t, s] <= storages.power[s]);

	# Generation CM_mod determined by remaining storage level
	@constraint(CM_mod, StorageLevelGen[s = S, t = T],
		P_S_opt[t, s]/Sbase + P_S_up[t, s] <= L_S[t, s]);

	# # Storace capacity limit
	@constraint(CM_mod, StorageLevelCap[s = S, t = T],
		L_S[t, s] <= storages.storage[s]/Sbase);

	# Storage
	@constraint(CM_mod, Storage[s = S, t = T; t > T[1]],
		L_S[t-1, s] - (P_S_opt[t-1, s]/Sbase + P_S_up[t-1, s]) + storages.efficiency[s] * D_S_opt[t-1, s]/Sbase == L_S[t, s]);

	# Objective function
	@objective(CM_mod, Min,
	    sum(powerplants.mc[g][1] * (ΔP_up[t, g]) * Sbase +
			(price[t] - powerplants.mc[g][1]) * ΔP_dn[t, g] *
				Sbase for g in G, t in T) +
		sum(0 * (ΔP_R_up[t, r]) * Sbase +
			(price[t] - 0) * ΔP_R_dn[t, r] *
				Sbase for r in R, t in T) +
		sum(5 * P_S_up[t, s] * Sbase for s in S, t in T) +
		1000 * sum(P_load_lost[t, j] * Sbase for j in J, t in T) +
		1000 * sum(P_gen_lost[t, j] * Sbase for j in J, t in T));

	# Initiate optimisation process
	JuMP.optimize!(CM_mod)


	# Export results
	ΔP_up_opt = JuMP.value.(ΔP_up) * Sbase
	ΔP_dn_opt = JuMP.value.(ΔP_dn) * Sbase
	ΔP_R_up_opt = JuMP.value.(ΔP_R_up) * Sbase
	ΔP_R_dn_opt = JuMP.value.(ΔP_R_dn) * Sbase
	Θ_opt = (JuMP.value.(Θ)) * 360/(2π)
	P_flow_opt = JuMP.value.(P_flow)*Sbase
	price = JuMP.dual.(PowerInjectionBal)
	P_gen_lost_opt = JuMP.value.(P_gen_lost) * Sbase
	P_load_lost_opt = JuMP.value.(P_load_lost) * Sbase

	P_S_up_opt = JuMP.value.(P_S_up) * Sbase
	L_S_opt_after_CM = JuMP.value.(L_S) * Sbase


	P_S_opt_after_CM = JuMP.Containers.DenseAxisArray(P_S_opt.data .+ P_S_up_opt.data, P_S_up_opt.axes[1], P_S_up_opt.axes[2])


	# if length(P_S_opt.axes[1]) <= 24
	# 	P_S_opt_after_CM = JuMP.Containers.DenseAxisArray(P_S_opt.data .+ P_S_up_opt.data, P_S_up_opt.axes[1], P_S_up_opt.axes[2])
	# 	# L_S_opt_after_CM = JuMP.Containers.DenseAxisArray(L_S_opt.data .- P_S_up_opt.data, P_S_up_opt.axes[1], P_S_up_opt.axes[2])
	# else
	# 	t_observed = 1:24
	# 	P_S_opt_after_CM = JuMP.Containers.DenseAxisArray(P_S_opt.data[t_observed, :] .+ P_S_up_opt.data, P_S_up_opt.axes[1], P_S_up_opt.axes[2])
	# 	# L_S_opt_after_CM = JuMP.Containers.DenseAxisArray(L_S_opt.data[t_observed, :] .- P_S_up_opt.data, P_S_up_opt.axes[1], P_S_up_opt.axes[2])
	# end

	return(CM_mod,
	       ΔP_up_opt,
		   ΔP_dn_opt,
		   ΔP_R_up_opt,
		   ΔP_R_dn_opt,
	       Θ_opt,
	       P_flow_opt,
		   price,
		   P_gen_lost_opt,
		   P_load_lost_opt,
		   P_S_opt_after_CM,
		   D_S_opt,
		   L_S_opt_after_CM
	       )
end

### Congestion management with renewable energy sources and storages
function CongestionManagementPHS80(hours,
							  nodes::Nodes,
							  lines::Lines,
							  powerplants::PowerPlants,
							  renewables::Renewables,
							  storages::Storages,
							  price,
							  P_opt,
							  P_R_opt,
							  P_S_opt,
							  D_S_opt,
							  L_S_opt,
						      )

	h,b = calc_h_b(lines, nodes)

	T = hours
	G = powerplants.unit
	R = renewables.unit
	S = storages.unit

	slack = nodes.slack
	Sbase = nodes.bmva # MVA

	###################
	# MODEL ###########
	###################

	# Create a subset of buses I and J
	J = nodes.id
	L = lines.id

	# Initialise JuMP model: Congestion management
	CM_mod = JuMP.Model(with_optimizer(Gurobi.Optimizer))

	# Variables
	@variable(CM_mod, ΔP_up[T, G] >= 0)
	@variable(CM_mod, ΔP_dn[T, G] >= 0)
	@variable(CM_mod, ΔP_R_up[T, R] == 0)
	@variable(CM_mod, ΔP_R_dn[T, R] >= 0)
	@variable(CM_mod, Θ[T, J])
	@variable(CM_mod, P_flow[T, L])
	@variable(CM_mod, P_load_lost[T, J] >= 0)
	@variable(CM_mod, P_gen_lost[T, J] >= 0)

	# Storage voriables
	@variable(CM_mod, P_S_up[T, S] >= 0)	# Power generation from storage unit
	# @variable(ED_mod, D_S[T, S] >= 0)	# Electricity demand from storage unit
	@variable(CM_mod, L_S[T, S] >= 0)


	# Fix the voltage angle of the slack bus
	for t in T
		fix(Θ[t, slack], 0)
	end

	for s in S
		fix(L_S[T[1], s], 0.2*storages.storage[s]/Sbase; force = true)
	end

	# Constraints
	# Market clearing/power balance
	# @constraint(CM_mod, MarketClearing[t = T],
	#     sum((P_opt[t, g]/Sbase + ΔP_up[t, g] - ΔP_dn[t, g]) for g in G) +
	# 	sum((P_R_opt[t, r]/Sbase + ΔP_R_up[t, r] - ΔP_R_dn[t, r]) for r in R) +
	# 	sum(P_S_opt[t, s]/Sbase for s in S) -
	# 	sum(D_S_opt[t, s]/Sbase for s in S)
	# 		== nodes.systemload[t]/Sbase);

	# Upper generation limit
	@constraint(CM_mod, GenerationLimitUp[g = G, t = T],
	    (P_opt[t, g]/Sbase + ΔP_up[t, g] - ΔP_dn[t, g]) <=
			powerplants.pmax[g][t]/Sbase);

	# Lower generation limit
	@constraint(CM_mod, GenerationLimitDown[g = G, t = T],
	    (P_opt[t, g]/Sbase + ΔP_up[t, g] - ΔP_dn[t, g]) >=
			powerplants.pmin[g][t]/Sbase);

	# Upper generation limit for renewables
	@constraint(CM_mod, ResGenerationLimitUp[r = R, t = T],
	    (P_R_opt[t, r]/Sbase + ΔP_R_up[t, r] - ΔP_R_dn[t, r]) <=
			renewables.infeed[r][t]/Sbase);

	# Lower generation limit for renewables
	@constraint(CM_mod, ResGenerationLimitDown[r = R, t = T],
	    (P_R_opt[t, r]/Sbase + ΔP_R_up[t, r] - ΔP_R_dn[t, r]) >=
			0);

	# # Ramp-up limit
	# @constraint(CM_mod, RampUp[g = G, t = T; t > T[1]],
	# 	(P_opt[t, g]/Sbase + ΔP_up[t, g] - ΔP_dn[t, g]) -
	# 		(P_opt[t-1, g]/Sbase + ΔP_up[t-1, g] - ΔP_dn[t-1, g]) <=
	# 		powerplants.rup[g]/Sbase)
	#
	# # Ramp-down limit
	# @constraint(CM_mod, RampDown[g = G, t = T; t > T[1]],
	# 	-((P_opt[t, g]/Sbase + ΔP_up[t, g] - ΔP_dn[t, g]) -
	# 		(P_opt[t-1, g]/Sbase + ΔP_up[t-1, g] - ΔP_dn[t-1, g])) <=
	# 		powerplants.rdn[g]/Sbase)

	### DC power flow constraints
	# Power injection balance
	@constraint(CM_mod, PowerInjectionBal[j = J, t = T],
		sum((P_opt[t, g]/Sbase + ΔP_up[t, g] - ΔP_dn[t, g])
			for g in getKeyVector(powerplants.node, j)) +
		sum((P_R_opt[t, r]/Sbase + ΔP_R_up[t, r] - ΔP_R_dn[t, r])
			for r in getKeyVector(renewables.node, j)) +
		sum((P_S_opt[t, s]/Sbase + P_S_up[t, s]) for s in getKeyVector(storages.node, j)) -
		sum((D_S_opt[t, s]/Sbase) for s in getKeyVector(storages.node, j)) -
		P_gen_lost[t, j] +
		sum(b[j,jj] * Θ[t, jj] for jj in J) ==
		nodes.load[j][t]/Sbase - nodes.exchange[j][t]/Sbase - P_load_lost[t, j]);

		@constraint(CM_mod, LinePowerFlowMax[l = L, t = T],
			P_flow[t, l] <= lines.pmax[l]/Sbase);

		@constraint(CM_mod, LinePowerFlowMin[l = L, t = T],
			P_flow[t, l] >= -lines.pmax[l]/Sbase);

		@constraint(CM_mod, LineFlow[l = L, t = T],
			P_flow[t, l] == sum(h[l,j] * Θ[t,j] for j in J));


			### PHS constraints
			@constraint(CM_mod, StoragePowerOutput[s = S, t = T],
		 		P_S_opt[t, s]/Sbase + P_S_up[t, s] <= storages.power[s]/Sbase);

			# Maximum storage power input
			# @constraint(CM_mod, StoragePowerInput[s = S, t = T],
			# 	D_S[t, s] <= storages.power[s]);

			# Generation CM_mod determined by remaining storage level
			@constraint(CM_mod, StorageLevelGen[s = S, t = T],
				P_S_up[t, s] <= L_S[t, s]);

			# Storage
			@constraint(CM_mod, Storage[s = S, t = T; t > T[1]],
				L_S[t-1, s] - P_S_up[t-1, s] == L_S[t, s]);



	# Objective function
	@objective(CM_mod, Min,
	    sum(powerplants.mc[g][1] * (ΔP_up[t, g]) * Sbase +
			(price[t] - powerplants.mc[g][1]) * ΔP_dn[t, g] *
				Sbase for g in G, t in T) +
		sum(0 * (ΔP_R_up[t, r]) * Sbase +
			(price[t] - 0) * ΔP_R_dn[t, r] *
				Sbase for r in R, t in T) +
		sum((price[t]/storages.efficiency[s]) * P_S_up[t, s] * Sbase for s in S, t in T) +
		1000 * sum(P_load_lost[t, j] * Sbase for j in J, t in T) +
		1000 * sum(P_gen_lost[t, j] * Sbase for j in J, t in T));

	# Initiate optimisation process
	JuMP.optimize!(CM_mod)


	# Export results
	ΔP_up_opt = JuMP.value.(ΔP_up) * Sbase
	ΔP_dn_opt = JuMP.value.(ΔP_dn) * Sbase
	ΔP_R_up_opt = JuMP.value.(ΔP_R_up) * Sbase
	ΔP_R_dn_opt = JuMP.value.(ΔP_R_dn) * Sbase
	Θ_opt = (JuMP.value.(Θ)) * 360/(2π)
	P_flow_opt = JuMP.value.(P_flow)*Sbase
	price = JuMP.dual.(PowerInjectionBal)
	P_gen_lost_opt = JuMP.value.(P_gen_lost) * Sbase
	P_load_lost_opt = JuMP.value.(P_load_lost) * Sbase
	P_S_up_opt = JuMP.value.(P_S_up) * Sbase

	return(CM_mod,
	       ΔP_up_opt,
		   ΔP_dn_opt,
		   ΔP_R_up_opt,
		   ΔP_R_dn_opt,
	       Θ_opt,
	       P_flow_opt,
		   price,
		   P_gen_lost_opt,
		   P_load_lost_opt,
		   P_S_up_opt
	       )
end
