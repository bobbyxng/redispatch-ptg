### Congestion management with renewable energy sources and storages
function CongestionManagementPHS80PtG(hours,
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
							  η_E,
							  η_M,
						      )

	h,b = calc_h_b(lines, nodes)

	T = hours
	G = powerplants.unit
	R = renewables.unit
	S = storages.unit

	slack = nodes.slack
	Sbase = nodes.bmva # MVA

	# Efficiencies
	# η_E = 0.76		# Electrolysis
	# η_M = 0.83		# Methanation

	# Subset of power plants: GFPP (Fuel = "NaturalGas")
	G_NG = getKeyVector(powerplants.fuel, "NaturalGas")
	G_other = setdiff(G, G_NG)
	R = renewables.unit
	R_SolarPV = getKeyVector(renewables.fuel, :SolarPV)
	R_WindOffshore = getKeyVector(renewables.fuel, :WindOffshore)
	R_WindOnshore = getKeyVector(renewables.fuel, :WindOnshore)

	R_PtG = [R_SolarPV; R_WindOffshore; R_WindOnshore]
	R_other = setdiff(R, R_PtG)

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

	# PtG extensions
	@variable(CM_mod, P_syn[T, G_NG] >= 0)	# Power generation from syngas
	@variable(CM_mod, D_PtG[T, R_PtG] >= 0)	# Electricity demand from PtG units
 	@variable(CM_mod, L_syn[T] >=0)

	# Fix the voltage angle of the slack bus
	for t in T
		fix(Θ[t, slack], 0)
	end

	for s in S
		fix(L_S[T[1], s], 0.2*storages.storage[s]/Sbase; force = true)
	end

	# Fix SNG parameters in first period
	fix(L_syn[T[1]], 0; force = true)

	# Upper generation limit
	@constraint(CM_mod, GenerationLimitUp[g = G_other, t = T],
	    (P_opt[t, g]/Sbase + ΔP_up[t, g] - ΔP_dn[t, g]) <=
			powerplants.pmax[g][t]/Sbase);

	# Lower generation limit
	@constraint(CM_mod, GenerationLimitDown[g = G_other, t = T],
	    (P_opt[t, g]/Sbase + ΔP_up[t, g] - ΔP_dn[t, g]) >=
			powerplants.pmin[g][t]/Sbase);

	# Upper generation limit for NG subset
	# @constraint(CM_mod, GenerationLimitUpNG[e = G_NG, t = T],
	#     (P_opt[t, e]/Sbase + ΔP_up[t, e] - ΔP_dn[t, e]) +
	# 	P_syn[t, e] <= powerplants.pmax[e][t]/Sbase);

	@constraint(CM_mod, GenerationLimitUpNG[e = G_NG, t = T],
		P_opt[t, e]/Sbase + ΔP_up[t, e] + P_syn[t, e] <= powerplants.pmax[e][t]/Sbase);

	# Lower generation limit for NG subset
	# @constraint(CM_mod, GenerationLimitDownNG[e = G_NG, t = T],
	# 	(P_opt[t, e]/Sbase + ΔP_up[t, e] - ΔP_dn[t, e]) +
	# 	P_syn[t, e] >= powerplants.pmin[e][t]/Sbase);

	@constraint(CM_mod, GenerationLimitDownNG[e = G_NG, t = T],
		P_opt[t, e]/Sbase - ΔP_dn[t, e]  >= powerplants.pmin[e][t]/Sbase);

	# Upper generation limit for renewables
	@constraint(CM_mod, ResGenerationLimitUp[r = R_other, t = T],
	    (P_R_opt[t, r]/Sbase + ΔP_R_up[t, r] - ΔP_R_dn[t, r]) <=
			renewables.infeed[r][t]/Sbase);

	# Lower generation limit for renewables
	@constraint(CM_mod, ResGenerationLimitDown[r = R_other, t = T],
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
		sum((D_S_opt[t, s]/Sbase) for s in getKeyVector(storages.node, j)) +
		sum(P_syn[t, e] for e in intersect(G_NG, getKeyVector(powerplants.node, j))) -
		sum(D_PtG[t, r] for r in intersect(R_PtG, getKeyVector(renewables.node, j))) -
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

		### PtG limits
		# @constraint(CM_mod, PowerToGasLimitUp[r = R_PtG, t = T],
		# 	D_PtG[t, r] + (P_R_opt[t, r]/Sbase + ΔP_R_up[t, r] - ΔP_R_dn[t, r]) <=
		# 	renewables.infeed[r][t]/Sbase);
		#
		# @constraint(CM_mod, PowerToGasLimitDown[r = R_PtG, t = T],
		# 	D_PtG[t, r] + (P_R_opt[t, r]/Sbase + ΔP_R_up[t, r] - ΔP_R_dn[t, r]) >=
		# 	0);

		@constraint(CM_mod, PowerToGasLimitUp[r = R_PtG, t = T],
			P_R_opt[t, r]/Sbase + ΔP_R_up[t, r] - ΔP_R_dn[t, r] - D_PtG[t, r] <=
			renewables.infeed[r][t]/Sbase);

		@constraint(CM_mod, PowerToGasLimitDown[r = R_PtG, t = T],
			P_R_opt[t, r]/Sbase + ΔP_R_up[t, r] - ΔP_R_dn[t, r] - D_PtG[t, r] >=
			0);

		@constraint(CM_mod, PtGStorageGen[e = G_NG, t = T],
			sum((1/powerplants.efficiency[e]) * P_syn[t, e] for e in G_NG) <= L_syn[t]);

		# Virtual synthetic methane storage
		@constraint(CM_mod, PtGStorage[t = T; t > T[1]],
			L_syn[t-1] - sum((1/powerplants.efficiency[e]) * P_syn[t-1, e] for e in G_NG) + η_E * η_M * sum(D_PtG[t-1, r] for r in R_PtG) == L_syn[t]);

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
		1000 * sum(P_gen_lost[t, j] * Sbase for j in J, t in T) +
		sum(price[t] * (1/(η_E*η_M)) * D_PtG[t, r] * Sbase for r in R_PtG, t in T) +
		sum(powerplants.varcost[e][1]* P_syn[t, e] * Sbase for e in G_NG, t in T));

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
	P_syn_opt = JuMP.value.(P_syn)*Sbase
	D_PtG_opt = JuMP.value.(D_PtG)*Sbase
	L_syn_opt = JuMP.value.(L_syn)*Sbase

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
		   P_S_up_opt,
		   P_syn_opt,
   		   D_PtG_opt,
   	       L_syn_opt
	       )
end

### Congestion management with renewable energy sources and storages
function CongestionManagementPHS80PtG(hours,
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
							  P_syn_opt_prev,
							  D_PtG_opt_prev,
							  L_syn_opt_prev,
							  η_E,
							  η_M,
						      )

	h,b = calc_h_b(lines, nodes)

	T = hours
	G = powerplants.unit
	R = renewables.unit
	S = storages.unit

	slack = nodes.slack
	Sbase = nodes.bmva # MVA

	# Efficiencies
	# η_E = 0.76		# Electrolysis
	# η_M = 0.83		# Methanation

	# Subset of power plants: GFPP (Fuel = "NaturalGas")
	G_NG = getKeyVector(powerplants.fuel, "NaturalGas")
	G_other = setdiff(G, G_NG)
	R = renewables.unit
	R_SolarPV = getKeyVector(renewables.fuel, :SolarPV)
	R_WindOffshore = getKeyVector(renewables.fuel, :WindOffshore)
	R_WindOnshore = getKeyVector(renewables.fuel, :WindOnshore)

	R_PtG = [R_SolarPV; R_WindOffshore; R_WindOnshore]
	R_other = setdiff(R, R_PtG)

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

	# PtG extensions
	@variable(CM_mod, P_syn[T, G_NG] >= 0)	# Power generation from syngas
	@variable(CM_mod, D_PtG[T, R_PtG] >= 0)	# Electricity demand from PtG units
 	@variable(CM_mod, L_syn[T] >=0)

	# Fix the voltage angle of the slack bus
	for t in T
		fix(Θ[t, slack], 0)
	end

	for s in S
		fix(L_S[T[1], s], 0.2*storages.storage[s]/Sbase; force = true)
	end

	# Fix PtG demand
	for r in R_PtG
		fix(D_PtG[T[1], r], D_PtG_opt_prev[(T[1]), r]/Sbase; force = true)
	end

	# Fix SNG generation through GfG power plants
	for e in G_NG
		fix(P_syn[T[1], e], P_syn_opt_prev[(T[1]), e]/Sbase; force = true)
	end

	# Fix SNG storage level
	fix(L_syn[T[1]], L_syn_opt_prev[T[1]]/Sbase; force = true)

	# Upper generation limit
	@constraint(CM_mod, GenerationLimitUp[g = G_other, t = T],
	    (P_opt[t, g]/Sbase + ΔP_up[t, g] - ΔP_dn[t, g]) <=
			powerplants.pmax[g][t]/Sbase);

	# Lower generation limit
	@constraint(CM_mod, GenerationLimitDown[g = G_other, t = T],
	    (P_opt[t, g]/Sbase + ΔP_up[t, g] - ΔP_dn[t, g]) >=
			powerplants.pmin[g][t]/Sbase);

	# Upper generation limit for NG subset
	# @constraint(CM_mod, GenerationLimitUpNG[e = G_NG, t = T],
	#     (P_opt[t, e]/Sbase + ΔP_up[t, e] - ΔP_dn[t, e]) +
	# 	P_syn[t, e] <= powerplants.pmax[e][t]/Sbase);

	@constraint(CM_mod, GenerationLimitUpNG[e = G_NG, t = T],
		P_opt[t, e]/Sbase + ΔP_up[t, e] + P_syn[t, e] <= powerplants.pmax[e][t]/Sbase);

	# Lower generation limit for NG subset
	# @constraint(CM_mod, GenerationLimitDownNG[e = G_NG, t = T],
	# 	(P_opt[t, e]/Sbase + ΔP_up[t, e] - ΔP_dn[t, e]) +
	# 	P_syn[t, e] >= powerplants.pmin[e][t]/Sbase);

	@constraint(CM_mod, GenerationLimitDownNG[e = G_NG, t = T],
		P_opt[t, e]/Sbase - ΔP_dn[t, e]  >= powerplants.pmin[e][t]/Sbase);

	# Upper generation limit for renewables
	@constraint(CM_mod, ResGenerationLimitUp[r = R_other, t = T],
	    (P_R_opt[t, r]/Sbase + ΔP_R_up[t, r] - ΔP_R_dn[t, r]) <=
			renewables.infeed[r][t]/Sbase);

	# Lower generation limit for renewables
	@constraint(CM_mod, ResGenerationLimitDown[r = R_other, t = T],
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
		sum((D_S_opt[t, s]/Sbase) for s in getKeyVector(storages.node, j)) +
		sum(P_syn[t, e] for e in intersect(G_NG, getKeyVector(powerplants.node, j))) -
		sum(D_PtG[t, r] for r in intersect(R_PtG, getKeyVector(renewables.node, j))) -
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

		### PtG limits
		# @constraint(CM_mod, PowerToGasLimitUp[r = R_PtG, t = T],
		# 	D_PtG[t, r] + (P_R_opt[t, r]/Sbase + ΔP_R_up[t, r] - ΔP_R_dn[t, r]) <=
		# 	renewables.infeed[r][t]/Sbase);
		#
		# @constraint(CM_mod, PowerToGasLimitDown[r = R_PtG, t = T],
		# 	D_PtG[t, r] + (P_R_opt[t, r]/Sbase + ΔP_R_up[t, r] - ΔP_R_dn[t, r]) >=
		# 	0);

		@constraint(CM_mod, PowerToGasLimitUp[r = R_PtG, t = T],
			P_R_opt[t, r]/Sbase + ΔP_R_up[t, r] - ΔP_R_dn[t, r] - D_PtG[t, r] <=
			renewables.infeed[r][t]/Sbase);

		@constraint(CM_mod, PowerToGasLimitDown[r = R_PtG, t = T],
			P_R_opt[t, r]/Sbase + ΔP_R_up[t, r] - ΔP_R_dn[t, r] - D_PtG[t, r] >=
			0);

		@constraint(CM_mod, PtGStorageGen[e = G_NG, t = T],
			sum((1/powerplants.efficiency[e]) * P_syn[t, e] for e in G_NG) <= L_syn[t]);

		# Virtual synthetic methane storage
		@constraint(CM_mod, PtGStorage[t = T; t > T[1]],
			L_syn[t-1] - sum((1/powerplants.efficiency[e]) * P_syn[t-1, e] for e in G_NG) + η_E * η_M * sum(D_PtG[t-1, r] for r in R_PtG) == L_syn[t]);

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
		1000 * sum(P_gen_lost[t, j] * Sbase for j in J, t in T) +
		sum(price[t] * (1/(η_E*η_M)) * D_PtG[t, r] * Sbase for r in R_PtG, t in T) +
		sum(powerplants.varcost[e][1]* P_syn[t, e] * Sbase for e in G_NG, t in T));

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
	P_syn_opt = JuMP.value.(P_syn)*Sbase
	D_PtG_opt = JuMP.value.(D_PtG)*Sbase
	L_syn_opt = JuMP.value.(L_syn)*Sbase

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
		   P_S_up_opt,
		   P_syn_opt,
   		   D_PtG_opt,
   	       L_syn_opt
	       )
end
