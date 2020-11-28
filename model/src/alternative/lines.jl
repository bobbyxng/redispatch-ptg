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
				   pmax
				   )
    end
end
