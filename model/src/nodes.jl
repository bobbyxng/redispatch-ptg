# This file contains the definition for the nodes struct.

mutable struct Nodes
	id
	systemload
	load
	exchange
	bmva
	slack

	function Nodes(nodes_df::DataFrame,
				   load_df::DataFrame,
				   exchange_df::DataFrame,
				   bmva::Number,
				   slack::Number
				   )

		# Initialise dataframe, including all nodes with zeros
		load_full = DataFrame(zeros(length(load_df[!, 1]),
										   length(nodes_df[!, 1])
									)
							 , Symbol.(nodes_df[!, 1])
							 )

		systemload = load_df[!, 1]

		# Calculate the absolute load at a node
		for col in 2:ncol(load_df)
			load_df[!, col] = load_df[!, col] .* load_df[!, 1]
		end

		load_df = load_df[!, 2:end]

		for col in names(load_df)
			load_full[!, col] = load_df[!, col]
		end

		N = parse.(Int,String.(names(load_full)))
		load_dict = Dict(N[col] => load_full[!, col] for col in 1:length(N))

		# Exchange
		exchange_full = DataFrame(zeros(length(load_df[!, 1]),
										   length(nodes_df[!, 1])
									)
							 , Symbol.(nodes_df[!, 1])
							 )

		for col in names(exchange_df)
			exchange_full[!, col] = exchange_df[!, col]
		end

		N_exchange = parse.(Int,String.(names(exchange_full)))
		exchange_dict = Dict(N_exchange[col] => exchange_full[!, col] for col in 1:length(N_exchange))

		return new(N,
				   systemload,
				   load_dict,
				   exchange_dict,
				   bmva,
				   slack
				   )
	end
end
