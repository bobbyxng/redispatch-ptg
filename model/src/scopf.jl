### DCOPF without renewable energy sources
function SCOPF(time,
			   nodes::Nodes,
			   lines::Lines,
			   powerplants::PowerPlants,
			   )

	T = time
	G = powerplants.unit

	slack = nodes.slack
	Sbase = nodes.bmva # MVA

	lines_pmax = zeros(length(nodes.id), length(nodes.id))

	for id in lines.id
		j = lines.from[id]
		k = lines.to[id]

		lines_pmax[j, k] = lines.pmax[id]
		lines_pmax[k, j] = lines.pmax[id]
	end

	# Create the B_prime matrix in one step
	B_prime = zeros(length(nodes.id), length(nodes.id))

	for id in lines.id
		j = lines.from[id]
		k = lines.to[id]

		X = lines.reactance[id]				# reactance

		B_prime[j, k] = 1/X					# Fill YBUS with susceptance
		B_prime[k, j] = 1/X					# Symmetric matrix
	end

	for j in 1:size(B_prime)[1]
		B_prime[j, j] = -sum(B_prime[j, :])
	end

	B_prime_s = zeros(length(nodes.id)-1, length(nodes.id)-1, length(lines.id))
	for s in lines.id
		B_temp = B_prime[setdiff(1:end, s), setdiff(1:end, s)]
		B_prime_s[:,:,s] = B_temp
	end


	###################
	# MODEL ###########
	###################

	# Create a subset of buses I and J
	J = nodes.id
	K = nodes.id
	S = lines.id

	# Initialise JuMP model: DCOPF
	SCOPF_mod = JuMP.Model(with_optimizer(Gurobi.Optimizer))

	# Variables
	@variable(SCOPF_mod, P[T, G] >= 0)
	@variable(SCOPF_mod, Θ[S, T, J])
	@variable(SCOPF_mod, P_inj[T, J])
	@variable(SCOPF_mod, P_flow[S, T, K, J])

	# Fix the voltage angle of the slack bus
	for s in S
		for t in T
			fix(Θ[s, t, slack], 0)
		end
	end

	# Constraints
	# Market clearing/power balance
	@constraint(SCOPF_mod, MarketClearing[t = T],
	    sum(P[t, g] for g in G) == nodes.systemload[t]/Sbase);

	# Upper generation limit
	@constraint(SCOPF_mod, GenerationLimitUp[g = G, t = T],
	    P[t, g] <= powerplants.pmax[g]/Sbase);

	# Lower generation limit
	@constraint(SCOPF_mod, GenerationLimitDown[g = G, t = T],
	    P[t, g] >= powerplants.pmin[g]/Sbase);

	# Ramp-up limit
	@constraint(SCOPF_mod, RampUp[g = G, t = T; t > 1],
		(P[t, g]) - (P[t-1, g]) <= powerplants.rup[g])

	# Ramp-down limit
	@constraint(SCOPF_mod, RampDown[g = G, t = T; t > 1],
		-((P[t, g]) - (P[t-1, g])) <= powerplants.rdn[g])

	### DC power flow constraints
	# Power injection balance
	@constraint(SCOPF_mod, PowerInjectionBal[j = J, t = T],
		sum(P[t, g] for g in getKeyVector(powerplants.node, j)) -	# all generation units connected to a node
			nodes.load[j][t]/Sbase == P_inj[t, j]);

	# Power injection angle
	@constraint(SCOPF_mod, PowerInjectionAng[j = J, t = T, s = S],
		sum(B_prime[j, k] * (Θ[s, t, j] - Θ[s, t, k]) for k in K)
			== P_inj[t, j]);

	@constraint(SCOPF_mod, LinePowerFlow[k = K, j = J, t = T, s = S; j != k &&
		k != lines.from[s] && j != lines.to[s]],
		B_prime[j, k] * (Θ[s, t, j] - Θ[s, t, k]) == P_flow[s, t, j, k]);


	@constraint(SCOPF_mod, LinePowerFlowMax[k = K, j = J, t = T, s = S;
		k != lines.from[s] && j != lines.to[s]],
		P_flow[s, t, j, k] <= lines_pmax[j, k]/Sbase);

	@constraint(SCOPF_mod, LinePowerFlowMin[k = K, j = J, t = T, s = S;
		k != lines.from[s] && j != lines.to[s]],
		P_flow[s, t, j, k] >= -lines_pmax[j, k]/Sbase);

	# Contingency: Line outage s = S
	@constraint(SCOPF_mod, Contingency[k = K, j = J, t = T, s = S;
		k == lines.from[s] && j == lines.to[s]],
		P_flow[s, t, j, k] == 0);

	# Objective function
	@objective(SCOPF_mod, Min,
	    sum(powerplants.mc[g] * P[t, g] * Sbase for g in G, t in T));


	# Initiate optimisation process
	JuMP.optimize!(SCOPF_mod)

	# Export results
	P_opt = JuMP.value.(P)*Sbase
	Θ_opt = (JuMP.value.(Θ)) * 360/(2π)
	P_flow_opt = JuMP.value.(P_flow)*Sbase
	P_inj_opt = JuMP.value.(P_inj)*Sbase

	return(SCOPF_mod,
	       P_opt,
	       Θ_opt,
	       P_inj_opt,
	       P_flow_opt
	       )
end
