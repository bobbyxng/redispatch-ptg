# This file contains the definition for the Lines struct.

"""
calc_lineparams(lines_df::DataFrame)
	Calculates line parameters with regard to voltage level, number of circuits,
	and Thermal Reliability Margin (TRM).
	Returns reactance (p.u.), resistance (p.u.), and pmax (MW)
"""
function calc_lineparams(lines_df::DataFrame, Sbase::Number, TRM::Number)

	zbase(voltage::Number) = (voltage*1E3)^2 / (Sbase*1E6)

	# L = Symbol.(lines_df[!, 1])
	L = lines_df[!, 1]
	zbase_vlevel = map(zbase, lines_df[!, :voltage])	# Calculate reactance in p.u. given voltage level

	reactance = Dict(zip(L, lines_df[!, :reactance] ./ lines_df[!, :circuits] ./ zbase_vlevel))
	resistance = Dict(zip(L,  lines_df[!, :resistance] ./ lines_df[!, :circuits] ./ zbase_vlevel))

	pmax = Dict(zip(L, lines_df[!, :pmax] .* lines_df[!, :circuits] .* (1-TRM) ))
	return reactance, resistance, pmax
end

mutable struct Lines
    id
    from
    to
	voltage
	reactance
	resistance
	pmax
	circuits

    function Lines(lines_df::DataFrame, Sbase::Number, TRM::Number)

		# L = Symbol.(lines_df[!, 1])
		L = lines_df[!, 1]
		reactance, resistance, pmax = calc_lineparams(lines_df, Sbase, TRM)                    # U = unit

		function lines_df_to_dict_with_id(df::DataFrame)
		    Dict(col[1] => Dict(zip(df[!, 1], col[2])) for col in eachcol(df, true))
		end

		lines_dict = lines_df_to_dict_with_id(lines_df)

        return new(L,
                   lines_dict[:from],
                   lines_dict[:to],
				   lines_dict[:voltage],
				   reactance,
				   resistance,
				   pmax,
				   lines_dict[:circuits]
				   )
    end
end

"""
    create_incidence(lines::Lines, nodes::Nodes)
Create the incidence matrix from 'lines' and 'nodes'.
"""
function create_incidence(lines::Lines, nodes::Nodes)
	incidence = zeros(Int, length(lines.id), length(nodes.id))
	for (i,l) in enumerate(lines.id)
	    incidence[i, findfirst(nodes.id .== lines.from[l])] = 1
	    incidence[i, findfirst(nodes.id .== lines.to[l])] = -1
	end
	return incidence
end

"""
    calc_h_b(lines::Lines, nodes::Nodes)
Calculate the H and B matrices from 'lines' and 'nodes'.
"""
function calc_h_b(lines::Lines, nodes::Nodes)

	incidence = create_incidence(lines, nodes)
	bvector = [lines.reactance[l] /
		(lines.reactance[l].^2 .+ lines.resistance[l].^2) for l in lines.id]
	h_matrix = bvector .* incidence
	b_matrix = h_matrix' *incidence

	h = Dict((l,n) => h_matrix[i,j]
		for (i,l) in enumerate(lines.id), (j,n) in enumerate(nodes.id))

	b = Dict((n,nn) => b_matrix[i,j]
		for (i,n) in enumerate(nodes.id), (j,nn) in enumerate(nodes.id))

	return h,b
end
