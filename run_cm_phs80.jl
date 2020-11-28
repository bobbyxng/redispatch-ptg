using Pkg
if isfile("Project.toml") && isfile("Manifest.toml")
    Pkg.activate(".")
end

###################
# Terminal ########
###################

# Read from terminal user input
case =  ARGS[1]		# First argument sets case
timeperiod = ARGS[2]	# Second argument sets time period
trm = parse(Float64, ARGS[3])

timeperiod_str = replace(string(timeperiod), ":" => "-")

println()
println("Case >" * case * "< selected.")
println("Calculating for time period >" * timeperiod * "<.")
println()

###################
# Run CM ##########
###################

println("Loading required packages: ProgressMeter, CSV, DataFrames, Dates, JuMP, Gurobi.")
using ProgressMeter
using CSV
using DataFrames
using Dates
using JuMP
using Gurobi
println("Loading of packages complete.")

include("model/src/Sesam_ptdf.jl")
using .Sesam

###################
# Data ############
###################

println("Reading data from csv...")
prog = Progress(14)

# Read nodes and load from .csv
	nodes_df = CSV.read(joinpath("data", case, "nodes.csv"))
		next!(prog)
	load_df = CSV.read(joinpath("data", case, "load.csv"))
		next!(prog)
	exchange_df = CSV.read(joinpath("data", case, "exchange.csv"))
		next!(prog)
# Read lines from .csv
	lines_df = CSV.read(joinpath("data", case, "lines.csv"))
		next!(prog)
# Read power plants from .csv
	powerplants_df = CSV.read(joinpath("data", case, "powerplants.csv"))
		next!(prog)
	avail_powerplants_df = CSV.read(joinpath("data", case, "avail_powerplants.csv"), header = 2, skipto = 3)
		next!(prog)
	mustrun_powerplants_df = CSV.read(joinpath("data", case, "mustrun_powerplants.csv"), header = 2, skipto = 3)
		next!(prog)
# Read fuel costs from .csv
	fuelcost_df = CSV.read(joinpath("data", case, "fuelcost.csv"))
		next!(prog)
# Read renewables and availabilities from .csv
	renewables_df = CSV.read(joinpath("data", case, "renewables.csv"))
		next!(prog)
	avail_solar_pv_df = CSV.read(joinpath("data", case, "avail_solar_pv.csv"))
		next!(prog)
	avail_wind_offshore_df = CSV.read(joinpath("data", case, "avail_wind_offshore.csv"))
		next!(prog)
	avail_wind_onshore_df = CSV.read(joinpath("data", case, "avail_wind_onshore.csv"))
		next!(prog)
	avail_other_res_df = CSV.read(joinpath("data", case, "avail_other_res.csv"))
		next!(prog)
	renewables_avail_dict = Dict(:SolarPV => avail_solar_pv_df,
		:WindOffshore => avail_wind_offshore_df,
		:WindOnshore => avail_wind_onshore_df,
		:other => avail_other_res_df)
		next!(prog)

println("Reading of data complete.")
println()

###################
# Model setup #####
###################

println("Preparing model technologies, nodes, and grid...")
prog = Progress(6)

# Create set of nodes and lines
	nodes = Nodes(nodes_df, load_df, exchange_df, 1000, 1)
		next!(prog)
	lines = Lines(lines_df, 1000, trm)
		next!(prog)
# Create set of power plants	ED_L_S_opt, ED_p
	pp = PowerPlants(powerplants_df, avail_powerplants_df, mustrun_powerplants_df, fuelcost_df)
		next!(prog)
# Create set of renewables
	res = Renewables(renewables_df, renewables_avail_dict)
		next!(prog)
# Create a set of storages
	storages_df = CSV.read(joinpath("data", case, "storages.csv"))
		next!(prog)
	storages = Storages(storages_df)
		next!(prog)

println("Preparation complete.")
println()

session_time = Dates.now()
session = Dates.format(session_time, "yyyy-mm-dd__HH-MM-SS")*"__ptdf"


# Create session folder for output
if isdir("output")
	if !isdir(joinpath("output", session*"__"*case*"__"*timeperiod_str))
		mkdir(joinpath("output", session*"__"*case*"__"*timeperiod_str))
	end
else
	mkdir("output")
	mkdir(joinpath("output", session*"__"*case*"__"*timeperiod_str))
end

prog = Progress(length(dayslicer(timeperiod)))


### Parameter outputs
println("Exporting parameters.")
println("Exporting merit order...")
# Merit order
output_ED_merit_order_pp = DataFrame(unit=Symbol[],fuel=String[],capacity=Float64[],mc=Float64[])

try
	for g in pp.unit
			mc = pp.mc[g]
			fuel = pp.fuel[g]
			capacity = pp.capacity[g]
			push!(output_ED_merit_order_pp, [g fuel capacity mc])
	end

	CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "ED_merit_order_pp.csv"), output_ED_merit_order_pp)
catch e
	println("The marginal costs for power plants are not available.")
end

output_ED_merit_order_res = DataFrame(unit=Symbol[],fuel=Symbol[],capacity=Float64[],mc=Float64[])

try
	for r in res.unit
			mc = 0
			fuel = res.fuel[r]
			capacity = res.capacity[r]
			push!(output_ED_merit_order_res, [r fuel capacity mc])
	end

	CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "ED_merit_order_res.csv"), output_ED_merit_order_res)
catch e
	println("The marginal costs for renewables are not available.")
end

output_ED_merit_order_storages = DataFrame(unit=Symbol[],fuel=String[],capacity=Float64[],mc=Float64[])

try
	for s in storages.unit
			mc = 0
			fuel = storages.fuel[s]
			capacity = storages.power[s]
			push!(output_ED_merit_order_storages, [s fuel capacity mc])
	end

	CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "ED_merit_order_storages.csv"), output_ED_merit_order_storages)
catch e
	println("The marginal costs for renewables are not available.")
end

println("Export complete.")
println()

### Initial run of first dayslice
println("Initial run. Calculating first dayslice: " * string(dayslicer(timeperiod)[1]))
println()
println()

ED_mod, ED_P_opt, ED_P_R_opt, ED_P_R_opt_dist, ED_P_S_opt, ED_D_S_opt,
	ED_L_S_opt, ED_price = EconomicDispatch80(dayslicer(timeperiod)[1], nodes, pp, res, storages)

CM_mod, CM_ΔP_up_opt, CM_ΔP_dn_opt, CM_ΔP_R_up_opt, CM_ΔP_R_dn_opt,
	CM_Θ_opt, CM_P_flow_opt, CM_price, CM_P_gen_lost_opt, CM_P_load_lost_opt, CM_P_S_up_opt = CongestionManagementPHS80(dayslicer(timeperiod)[1],
	nodes, lines, pp, res, storages, ED_price, ED_P_opt, ED_P_R_opt_dist,
	ED_P_S_opt, ED_D_S_opt, ED_L_S_opt)

next!(prog)

	# Dispatchable power plants
	output_ED_P_opt = DataFrame(time=Int64[],unit=Symbol[],fuel=String[],output=Float64[],node=Int64[])

	try
		for t in dayslicer(timeperiod)[1]
			for g in pp.unit
				val = ED_P_opt[t, g]
				fuel =  pp.fuel[g]
				push!(output_ED_P_opt, [t g fuel val pp.node[g]])
			end
		end
		CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "ED_P.csv"), output_ED_P_opt)
	catch e
		println("ED_P_opt does not exist.")
	end

	# RES generation
	output_ED_P_R_opt = DataFrame(time=Int64[],unit=Symbol[],fuel=Symbol[],output=Float64[],node=Int64[])
	try
		for t in dayslicer(timeperiod)[1]
			for r in res.unit
				val = ED_P_R_opt[t, r]
				fuel =  res.fuel[r]
				push!(output_ED_P_R_opt, [t r fuel val res.node[r]])
			end
		end
		CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "ED_P_R.csv"), output_ED_P_R_opt)
	catch e
		println("ED_P_R_opt does not exist.")
	end

	# Maximum available RES infeed from input data
	output_MaxRES = DataFrame(time=Int64[],unit=Symbol[],fuel=Symbol[],output=Float64[],node=Int64[])
	try
		for t in dayslicer(timeperiod)[1]
			for r in res.unit
				output = res.infeed[r][t]
				fuel =  res.fuel[r]
				push!(output_MaxRES, [t r fuel output res.node[r]])
			end
		end
		CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "MaxRES.csv"), output_MaxRES)
	catch e
		println("res.infeed does not exist.")
	end

	# Generation from storages
	output_ED_P_S_opt = DataFrame(time=Int64[],unit=Symbol[],fuel=String[],output=Float64[],node=Int64[])
	try
		for t in dayslicer(timeperiod)[1]
			for s in storages.unit
				val = ED_P_S_opt[t, s]
				fuel =  storages.fuel[s]
				push!(output_ED_P_S_opt, [t s fuel val storages.node[s]])
			end
		end
		CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "ED_P_S.csv"), output_ED_P_S_opt)
	catch e
		println("ED_P_S_opt does not exist.")
	end

	# Electricity demand from storages
	output_ED_D_S_opt = DataFrame(time=Int64[],unit=Symbol[],fuel=String[],output=Float64[],node=Int64[])
	try
		for t in dayslicer(timeperiod)[1]
			for s in storages.unit
				val = -1*ED_D_S_opt[t, s]
				fuel =  storages.fuel[s]
				push!(output_ED_D_S_opt, [t s fuel val storages.node[s]])
			end
		end
		CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "ED_D_S.csv"), output_ED_D_S_opt)
	catch e
		println("ED_D_S_opt does not exist.")
	end

	# Storage level
	output_ED_L_S_opt = DataFrame(time=Int64[],unit=Symbol[],fuel=String[],storage=Float64[],node=Int64[])
	try
		for t in dayslicer(timeperiod)[1]
			for s in storages.unit
				val = ED_L_S_opt[t, s]
				fuel =  storages.fuel[s]
				push!(output_ED_L_S_opt, [t s fuel val storages.node[s]])
			end
		end
		CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "ED_L_S.csv"), output_ED_L_S_opt)
	catch e
		println("ED_L_S_opt does not exist.")
	end

	# Load
	output_load = DataFrame(time=Int64[],load=Float64[])
	try
		for t in dayslicer(timeperiod)[1]
			load = nodes.systemload[t]
			push!(output_load, [t load])
		end
		CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "load.csv"), output_load)
	catch e
		println("nodes.systemload does not exist.")
	end

	# Prices
	output_ED_price = DataFrame(time=Int64[],price=Float64[])
	try
		for t in dayslicer(timeperiod)[1]
			price =  ED_price[t]
			push!(output_ED_price, [t price])
		end
		CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "ED_price.csv"), output_ED_price)
	catch e
		println("ED_price does not exist.")
	end

	# Must-run
	output_pmin = DataFrame(time=Int64[],unit=Symbol[],fuel=String[],output=Float64[],node=Int64[])
	try
		for t in dayslicer(timeperiod)[1]
			for g in pp.unit
				val = pp.pmin[g][t]
				fuel =  pp.fuel[g]
				push!(output_pmin, [t g fuel val pp.node[g]])
			end
		end
		CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "pmin.csv"), output_pmin)
	catch e
		println("pp.pmin does not exist.")
	end

	### Model parameters
	output_ED_info = DataFrame(timeperiod=String[],
									   termination_status=String[],
									   primal_status=String[],
									   dual_status=String[],
									   objective_value=Float64[],
									   solve_time=Float64[])

	push!(output_ED_info, [string(dayslicer(timeperiod)[1]) string(termination_status(ED_mod)) string(primal_status(ED_mod)) string(dual_status(ED_mod)) objective_value(ED_mod) solve_time(ED_mod)])
	CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "ED_info.csv"), output_ED_info)

	# Nodal exchange
	output_exchange = DataFrame(time=Int64[],node=Int64[],output=Float64[])
	try
		for t in dayslicer(timeperiod)[1]
			for j in nodes.id
				push!(output_exchange, [t j nodes.exchange[j][t]])
			end
		end
		CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "exchange.csv"), output_exchange)
	catch e
		println("output_exchange does not exist.")
	end

	###################
	# CM ##############
	###################

	# Dispatchable power plants
	output_CM_ΔP_up_opt = DataFrame(time=Int64[],unit=Symbol[],fuel=String[],output=Float64[],node=Int64[])
	try
		for t in dayslicer(timeperiod)[1]
			for g in pp.unit
				val = CM_ΔP_up_opt[t, g]
				fuel =  pp.fuel[g]
				push!(output_CM_ΔP_up_opt, [t g fuel val pp.node[g]])
			end
		end
		CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "CM_P_up.csv"), output_CM_ΔP_up_opt)
	catch e
		println("CM_ΔP_up_opt does not exist.")
	end

	output_CM_ΔP_dn_opt = DataFrame(time=Int64[],unit=Symbol[],fuel=String[],output=Float64[],node=Int64[])
	try
		for t in dayslicer(timeperiod)[1]
			for g in pp.unit
				val = -1*CM_ΔP_dn_opt[t, g]
				fuel =  pp.fuel[g]
				push!(output_CM_ΔP_dn_opt, [t g fuel val pp.node[g]])
			end
		end
		CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "CM_P_dn.csv"), output_CM_ΔP_dn_opt)
	catch e
		println("CM_ΔP_dn_opt does not exist.")
	end

	# RES generation
	output_CM_ΔP_R_up_opt = DataFrame(time=Int64[],unit=Symbol[],fuel=Symbol[],output=Float64[],node=Int64[])
	try
		for t in dayslicer(timeperiod)[1]
			for r in res.unit
				val = CM_ΔP_R_up_opt[t, r]
				fuel =  res.fuel[r]
				push!(output_CM_ΔP_R_up_opt, [t r fuel val res.node[r]])
			end
		end
		CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "CM_P_R_up.csv"), output_CM_ΔP_R_up_opt)
	catch e
		println("CM_ΔP_R_up_opt does not exist.")
	end

	output_CM_ΔP_R_dn_opt = DataFrame(time=Int64[],unit=Symbol[],fuel=Symbol[],output=Float64[],node=Int64[])
	try
		for t in dayslicer(timeperiod)[1]
			for r in res.unit
				val = -1*CM_ΔP_R_dn_opt[t, r]
				fuel =  res.fuel[r]
				push!(output_CM_ΔP_R_dn_opt, [t r fuel val res.node[r]])
			end
		end
		CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "CM_P_R_dn.csv"), output_CM_ΔP_R_dn_opt)
	catch e
		println("CM_ΔP_R_dn_opt does not exist.")
	end

	### Model parameters
	output_CM_info = DataFrame(timeperiod=String[],
									   termination_status=String[],
									   primal_status=String[],
									   dual_status=String[],
									   objective_value=Float64[],
									   solve_time=Float64[])

	push!(output_CM_info, [string(dayslicer(timeperiod)[1]) string(termination_status(CM_mod)) string(primal_status(CM_mod)) string(dual_status(CM_mod)) objective_value(CM_mod) solve_time(CM_mod)])
	CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "CM_info.csv"), output_CM_info)

	### Power flow results
	output_CM_P_flow_opt = DataFrame(time=Int64[],
									 line=Int64[],
									 from=Int64[],
									 to=Int64[],
									 pmax=Float64[],
									 pflow=Float64[])

	 try
 		for t in dayslicer(timeperiod)[1]
 			for l in lines.id
				from = parse(Int, string(lines.from[l]))
				to = parse(Int, string(lines.to[l]))
				pmax = lines.pmax[l]
				pflow = CM_P_flow_opt[t, l]
 				push!(output_CM_P_flow_opt, [t l from to pmax pflow])
 			end
 		end
 		CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "CM_P_flow.csv"), output_CM_P_flow_opt)
 	catch e
 		println("CM_P_flow_opt does not exist.")
 	end

	# Lost generation
	output_CM_P_gen_lost = DataFrame(time=Int64[],node=Int64[],output=Float64[])
	try
		for t in dayslicer(timeperiod)[1]
			for j in nodes.id
				output = CM_P_gen_lost_opt[t, j]
				push!(output_CM_P_gen_lost, [t, j, output])
			end
		end
		CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "CM_P_gen_lost.csv"), output_CM_P_gen_lost)
	catch e
		println("CM_P_gen_lost does not exist.")
	end

	# Lost load
	output_CM_P_load_lost = DataFrame(time=Int64[],node=Int64[],output=Float64[])
	try
		for t in dayslicer(timeperiod)[1]
			for j in nodes.id
				output = CM_P_load_lost_opt[t, j]
				push!(output_CM_P_load_lost, [t, j, output])
			end
		end
		CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "CM_P_load_lost.csv"), output_CM_P_load_lost)
	catch e
		println("CM_P_load_lost does not exist.")
	end

	output_CM_price = DataFrame(time=Int64[],node=Int64[],price=Float64[])
	 try
		 for t in dayslicer(timeperiod)[1]
			 for j in nodes.id
				 price =  CM_price[j, t]
				 push!(output_CM_price, [t j price])
			 end
		 end
		 CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "CM_price.csv"), output_CM_price)
	 catch e
		 println("CM_price does not exist.")
	 end

	# PHS output increase
	output_CM_P_S_up_opt = DataFrame(time=Int64[],unit=Symbol[],fuel=String[],output=Float64[],node=Int64[])
	try
		for t in dayslicer(timeperiod)[1]
			for s in storages.unit
				val = CM_P_S_up_opt[t, s]
				fuel =  storages.fuel[s]
				push!(output_CM_P_S_up_opt, [t s fuel val storages.node[s]])
			end
		end
		CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "CM_P_S_up.csv"), output_CM_P_S_up_opt)
	catch e
		println("CM_P_S_up_opt does not exist.")
	end

### Remaining periods: Initialising first hour with last hour of previous day
# 	and append data

for hours in dayslicer(timeperiod)[2:end]
	hours_extended = [hours[1]-1; hours]

	println()
	println()
	println("Calculating economic dispatch: " * string(hours))

	global ED_mod, ED_P_opt, ED_P_R_opt, ED_P_R_opt_dist, ED_P_S_opt, ED_D_S_opt,
		ED_L_S_opt, ED_price = EconomicDispatch80(hours, nodes, pp, res, storages)

	    ###################
	    # ED ##############
	    ###################

	    # Dispatchable power plants
	    output_ED_P_opt = DataFrame(time=Int64[],unit=Symbol[],fuel=String[],output=Float64[],node=Int64[])

	    try
	    	for t in hours
	    		for g in pp.unit
	    			val = ED_P_opt[t, g]
	    			fuel =  pp.fuel[g]
	    			push!(output_ED_P_opt, [t g fuel val pp.node[g]])
	    		end
	    	end
	    	CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "ED_P.csv"), output_ED_P_opt, append = true)
	    catch e
	    	println("ED_P_opt does not exist.")
	    end

	    # RES generation
	    output_ED_P_R_opt = DataFrame(time=Int64[],unit=Symbol[],fuel=Symbol[],output=Float64[],node=Int64[])
	    try
	    	for t in hours
	    		for r in res.unit
	    			val = ED_P_R_opt[t, r]
	    			fuel =  res.fuel[r]
	    			push!(output_ED_P_R_opt, [t r fuel val res.node[r]])
	    		end
	    	end
	    	CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "ED_P_R.csv"), output_ED_P_R_opt, append = true)
	    catch e
	    	println("ED_P_R_opt does not exist.")
	    end

	    # Maximum available RES infeed from input data
	    output_MaxRES = DataFrame(time=Int64[],unit=Symbol[],fuel=Symbol[],output=Float64[],node=Int64[])
	    try
	    	for t in hours
	    		for r in res.unit
	    			output = res.infeed[r][t]
	    			fuel =  res.fuel[r]
	    			push!(output_MaxRES, [t r fuel output res.node[r]])
	    		end
	    	end
	    	CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "MaxRES.csv"), output_MaxRES, append = true)
	    catch e
	    	println("res.infeed does not exist.")
	    end

	    # Generation from storages
	    output_ED_P_S_opt = DataFrame(time=Int64[],unit=Symbol[],fuel=String[],output=Float64[],node=Int64[])
	    try
	    	for t in hours
	    		for s in storages.unit
	    			val = ED_P_S_opt[t, s]
	    			fuel =  storages.fuel[s]
	    			push!(output_ED_P_S_opt, [t s fuel val storages.node[s]])
	    		end
	    	end
	    	CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "ED_P_S.csv"), output_ED_P_S_opt, append = true)
	    catch e
	    	println("ED_P_S_opt does not exist.")
	    end

	    # Electricity demand from storages
	    output_ED_D_S_opt = DataFrame(time=Int64[],unit=Symbol[],fuel=String[],output=Float64[],node=Int64[])
	    try
	    	for t in hours
	    		for s in storages.unit
	    			val = -1*ED_D_S_opt[t, s]
	    			fuel =  storages.fuel[s]
	    			push!(output_ED_D_S_opt, [t s fuel val storages.node[s]])
	    		end
	    	end
	    	CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "ED_D_S.csv"), output_ED_D_S_opt, append = true)
	    catch e
	    	println("ED_D_S_opt does not exist.")
	    end

	    # Storage level
	    output_ED_L_S_opt = DataFrame(time=Int64[],unit=Symbol[],fuel=String[],storage=Float64[],node=Int64[])
	    try
	    	for t in hours
	    		for s in storages.unit
	    			val = ED_L_S_opt[t, s]
	    			fuel =  storages.fuel[s]
	    			push!(output_ED_L_S_opt, [t s fuel val storages.node[s]])
	    		end
	    	end
	    	CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "ED_L_S.csv"), output_ED_L_S_opt, append = true)
	    catch e
	    	println("ED_L_S_opt does not exist.")
	    end

	    # Load
	    output_load = DataFrame(time=Int64[],load=Float64[])
	    try
	    	for t in hours
	    		load = nodes.systemload[t]
	    		push!(output_load, [t load])
	    	end
	    	CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "load.csv"), output_load, append = true)
	    catch e
	    	println("nodes.systemload does not exist.")
	    end

	    # Prices
	    output_ED_price = DataFrame(time=Int64[],price=Float64[])
	    try
	    	for t in hours
	    		price =  ED_price[t]
	    		push!(output_ED_price, [t price])
	    	end
	    	CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "ED_price.csv"), output_ED_price, append = true)
	    catch e
	    	println("ED_price does not exist.")
	    end

	    # Must-run
	    output_pmin = DataFrame(time=Int64[],unit=Symbol[],fuel=String[],output=Float64[],node=Int64[])
	    try
	    	for t in hours
	    		for g in pp.unit
	    			val = pp.pmin[g][t]
	    			fuel =  pp.fuel[g]
	    			push!(output_pmin, [t g fuel val pp.node[g]])
	    		end
	    	end
	    	CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "pmin.csv"), output_pmin, append = true)
	    catch e
	    	println("pp.pmin does not exist.")
	    end

		### Model parameters
		output_ED_info = DataFrame(timeperiod=String[],
										   termination_status=String[],
										   primal_status=String[],
										   dual_status=String[],
										   objective_value=Float64[],
										   solve_time=Float64[])

		push!(output_ED_info, [string(hours) string(termination_status(ED_mod)) string(primal_status(ED_mod)) string(dual_status(ED_mod)) objective_value(ED_mod) solve_time(ED_mod)])
		CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "ED_info.csv"), output_ED_info, append = true)


		###################
		# CM ##############
		###################

		println()
		println()
		println("Calculating congestion management: " * string(hours))

			global CM_mod, CM_ΔP_up_opt, CM_ΔP_dn_opt, CM_ΔP_R_up_opt, CM_ΔP_R_dn_opt,
				CM_Θ_opt, CM_P_flow_opt, CM_price, CM_P_gen_lost_opt, CM_P_load_lost_opt, CM_P_S_up_opt = CongestionManagementPHS80(hours, nodes, lines, pp, res, storages, ED_price,
				ED_P_opt,  ED_P_R_opt_dist, ED_P_S_opt, ED_D_S_opt, ED_L_S_opt)

			# Dispatchable power plants
			output_CM_ΔP_up_opt = DataFrame(time=Int64[],unit=Symbol[],fuel=String[],output=Float64[],node=Int64[])
			try
				for t in hours
					for g in pp.unit
						val = CM_ΔP_up_opt[t, g]
						fuel =  pp.fuel[g]
						push!(output_CM_ΔP_up_opt, [t g fuel val pp.node[g]])
					end
				end
				CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "CM_P_up.csv"), output_CM_ΔP_up_opt, append = true)
			catch e
				println("CM_ΔP_up_opt does not exist.")
			end

			output_CM_ΔP_dn_opt = DataFrame(time=Int64[],unit=Symbol[],fuel=String[],output=Float64[],node=Int64[])
			try
				for t in hours
					for g in pp.unit
						val = -1*CM_ΔP_dn_opt[t, g]
						fuel =  pp.fuel[g]
						push!(output_CM_ΔP_dn_opt, [t g fuel val pp.node[g]])
					end
				end
				CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "CM_P_dn.csv"), output_CM_ΔP_dn_opt, append = true)
			catch e
				println("CM_ΔP_dn_opt does not exist.")
			end

			# RES generation
			output_CM_ΔP_R_up_opt = DataFrame(time=Int64[],unit=Symbol[],fuel=Symbol[],output=Float64[],node=Int64[])
			try
				for t in hours
					for r in res.unit
						val = CM_ΔP_R_up_opt[t, r]
						fuel =  res.fuel[r]
						push!(output_CM_ΔP_R_up_opt, [t r fuel val res.node[r]])
					end
				end
				CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "CM_P_R_up.csv"), output_CM_ΔP_R_up_opt, append = true)
			catch e
				println("CM_ΔP_R_up_opt does not exist.")
			end

			output_CM_ΔP_R_dn_opt = DataFrame(time=Int64[],unit=Symbol[],fuel=Symbol[],output=Float64[],node=Int64[])
			try
				for t in hours
					for r in res.unit
						val = -1*CM_ΔP_R_dn_opt[t, r]
						fuel =  res.fuel[r]
						push!(output_CM_ΔP_R_dn_opt, [t r fuel val res.node[r]])
					end
				end
				CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "CM_P_R_dn.csv"), output_CM_ΔP_R_dn_opt, append = true)
			catch e
				println("CM_ΔP_R_dn_opt does not exist.")
			end

			### Model parameters
			output_CM_info = DataFrame(timeperiod=String[],
											   termination_status=String[],
											   primal_status=String[],
											   dual_status=String[],
											   objective_value=Float64[],
											   solve_time=Float64[])

			push!(output_CM_info, [string(hours) string(termination_status(CM_mod)) string(primal_status(CM_mod)) string(dual_status(CM_mod)) objective_value(CM_mod) solve_time(CM_mod)])
			CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "CM_info.csv"), output_CM_info, append = true)

			### Power flow results
			output_CM_P_flow_opt = DataFrame(time=Int64[],
											 line=Int64[],
											 from=Int64[],
											 to=Int64[],
											 pmax=Float64[],
											 pflow=Float64[])

			try
			   for t in hours
				   for l in lines.id
					   from = parse(Int, string(lines.from[l]))
					   to = parse(Int, string(lines.to[l]))
					   pmax = lines.pmax[l]
					   pflow = CM_P_flow_opt[t, l]
					   push!(output_CM_P_flow_opt, [t l from to pmax pflow])
				   end
			   end
			   CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "CM_P_flow.csv"), output_CM_P_flow_opt, append = true)
		   catch e
			   println("CM_P_flow_opt does not exist.")
		   end

		   # Lost generation
		   output_CM_P_gen_lost = DataFrame(time=Int64[],node=Int64[],output=Float64[])
		   try
			   for t in hours
				   for j in nodes.id
					   output = CM_P_gen_lost_opt[t, j]
					   push!(output_CM_P_gen_lost, [t, j, output])
				   end
			   end
			   CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "CM_P_gen_lost.csv"), output_CM_P_gen_lost, append = true)
		   catch e
			   println("CM_P_gen_lost does not exist.")
		   end

		   # Lost load
		   output_CM_P_load_lost = DataFrame(time=Int64[],node=Int64[],output=Float64[])
		   try
			   for t in hours
				   for j in nodes.id
					   output = CM_P_load_lost_opt[t, j]
					   push!(output_CM_P_load_lost, [t, j, output])
				   end
			   end
			   CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "CM_P_load_lost.csv"), output_CM_P_load_lost, append = true)
		   catch e
			   println("CM_P_load_lost does not exist.")
		   end

		   # Noral Price
		   output_CM_price = DataFrame(time=Int64[],node=Int64[],price=Float64[])
	   	    try
	   	    	for t in hours
					for j in nodes.id
		   	    		price =  CM_price[j, t]
		   	    		push!(output_CM_price, [t j price])
					end
	   	    	end
	   	    	CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "CM_price.csv"), output_CM_price, append = true)
	   	    catch e
	   	    	println("CM_price does not exist.")
	   	    end

			output_CM_P_S_up_opt = DataFrame(time=Int64[],unit=Symbol[],fuel=String[],output=Float64[],node=Int64[])
			try
				for t in hours
					for s in storages.unit
						val = CM_P_S_up_opt[t, s]
						fuel =  storages.fuel[s]
						push!(output_CM_P_S_up_opt, [t s fuel val storages.node[s]])
					end
				end
				CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "CM_P_S_up.csv"), output_CM_P_S_up_opt, append = true)
			catch e
				println("CM_P_S_up_opt does not exist.")
			end

			# Nodal exchange
			output_exchange = DataFrame(time=Int64[],node=Int64[],output=Float64[])
			try
				for t in hours
					for j in nodes.id
						push!(output_exchange, [t j nodes.exchange[j][t]])
					end
				end
				CSV.write(joinpath("output", session*"__"*case*"__"*timeperiod_str, "exchange.csv"), output_exchange, append = true)
			catch e
				println("output_exchange does not exist.")
			end

	next!(prog)

end

println()
println("Calculation complete.")
