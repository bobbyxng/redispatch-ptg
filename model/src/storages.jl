# This file contains the definition for the Storages struct.

mutable struct Storages
	unit
	node
	technology
	fuel
	power
	efficiency
	storage
	varcost

	function Storages(storages_df::DataFrame)

		S = Symbol.(storages_df[!, 1])
		storages_dict = df_to_dict_with_id(storages_df)

		return new(S,
				   storages_dict[:node],
				   storages_dict[:technology],
				   storages_dict[:fuel],
				   storages_dict[:power],
   				   storages_dict[:efficiency],
				   storages_dict[:storage],
				   storages_dict[:varcost])
	end

end
