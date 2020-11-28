function df_to_dict_with_id(df::DataFrame)
    Dict(col[1] => Dict(zip(Symbol.(df[!, 1]), col[2])) for col in eachcol(df, true))
end

function df_to_dict(df::DataFrame)
    Dict(Symbol.(col[1]) => col[2] for col in eachcol(df, true))
end

function dictzip(df::DataFrame, which::Tuple{Symbol, Symbol})
    return Dict(zip(df[which[1]], df[which[2]]))
end

function insert_default!(dict::Dict, default, keys)
	for k in keys
		haskey(dict, k) || (dict[k] = default)
	end
end
