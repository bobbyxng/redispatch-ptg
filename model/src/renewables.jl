# This file contains the definition for the Renewables struct.

function get_res_dict(renewables_df::DataFrame)

   renewables_stacked = stack(renewables_df, Not(:node), variable_name=:fuel,
			           value_name=:capacity)

	renewables_stacked[!, :unit] = Symbol.(string.(renewables_stacked[!, :fuel]) .* "_"
		                      .* string.(renewables_stacked[!, :node]))

	renewables_stacked = renewables_stacked[:, [:unit, :node, :fuel, :capacity]]
	renewables_stacked = renewables_stacked[renewables_stacked[!, :capacity] .> 0, :]
	renewables_dict = df_to_dict_with_id(renewables_stacked)

	return renewables_stacked[!, :unit], renewables_dict
end

function get_avail_dict(renewables_avail_dict::Dict, renewables_dict)

	fuel = renewables_dict[:fuel]
	node = renewables_dict[:node]

	avail_dict = Dict{Symbol,Array{Float64, 1}}()
	for unit in keys(fuel)

		f = fuel[unit]
		n = Symbol(node[unit])

		if haskey(renewables_avail_dict, f)
			if Symbol(node[unit]) in names(renewables_avail_dict[f])
				avail_dict[unit] = renewables_avail_dict[f][!, n]
			else
				avail_dict[unit] = fill(0.0, 8760)
			end
		else
		 	avail_dict[unit] = renewables_avail_dict[:other][!, f]
		end
	end

	return avail_dict
end

function calc_infeed(cap_dict, avail_dict)

	infeed = map(collect(keys(cap_dict))) do unit
		cap = cap_dict[unit]
		avail = avail_dict[unit]

		unit => avail * cap
	end |> Dict

	return infeed
end

mutable struct Renewables
	unit
	fuel
	node
	capacity
	infeed

	function Renewables(renewables_df::DataFrame, renewables_avail_dict::Dict)

		units, renewables_dict = get_res_dict(renewables_df)
		avail_dict = get_avail_dict(renewables_avail_dict, renewables_dict)
		infeed = calc_infeed(renewables_dict[:capacity], avail_dict)

		return new(units, renewables_dict[:fuel], renewables_dict[:node], renewables_dict[:capacity], infeed)
	end
end
