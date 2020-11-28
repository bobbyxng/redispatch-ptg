# This file contains the definition for the PowerPlants struct.

mutable struct PowerPlants

    unit
    node
    technology
    fuel
    capacity
    efficiency
    emission
    varcost
    mc
    mc_fuel
    mc_co2
    pmax
    pmin
    rup
    rdn

    function PowerPlants(powerplants_df::DataFrame, avail_powerplants_df::DataFrame, mustrun_powerplants_df::DataFrame, fuelcost_df::DataFrame)

        P = Symbol.(powerplants_df[!, 1])                                  # U = unit
        powerplants_dict = df_to_dict_with_id(powerplants_df)

        # Marginal generation cost from fuel cost, efficiency, variable cost
        # and CO2 price
        fuelcost_dict = df_to_dict(fuelcost_df)
        mc = Dict{Symbol, Array{Float64, 1}}()
        mc_fuel = Dict{Symbol, Array{Float64, 1}}()
        mc_co2 = Dict{Symbol, Array{Float64, 1}}()

        for p in P
            f = Symbol(powerplants_dict[:fuel][p])
            fuelcost = fuelcost_dict[f]
            η = powerplants_dict[:efficiency][p]
            co_price = fuelcost_dict[:CO2]
            emission = powerplants_dict[:emission][p]
            vc = powerplants_dict[:varcost][p]

            mc[p] = fuelcost / η + co_price * emission .+ vc
            mc_fuel[p] = fuelcost / η
            mc_co2[p] = co_price * emission
        end

        # Maximum power output based on capacity and availability
        pmax_df = copy(avail_powerplants_df)

        for p in P
            pmax_df[!, p] = avail_powerplants_df[!, p] .* powerplants_dict[:capacity][p]
        end

        pmax_dict = df_to_dict(pmax_df)

        # Must-run obligation/pmin based on time series
        pmin_df = copy(mustrun_powerplants_df)

        for p in P
            pmin_df[!, p] = avail_powerplants_df[!, p] .* mustrun_powerplants_df[!, p] .* powerplants_dict[:capacity][p]
        end

        pmin_dict = df_to_dict(pmin_df)

        return new(P,
                   powerplants_dict[:node],
                   powerplants_dict[:technology],
                   powerplants_dict[:fuel],
                   powerplants_dict[:capacity],
                   powerplants_dict[:efficiency],
                   powerplants_dict[:emission],
                   powerplants_dict[:varcost],
                   mc,
                   mc_fuel,
                   mc_co2,
                   pmax_dict,
                   pmin_dict,
                   powerplants_dict[:rup],
                   powerplants_dict[:rdn],
                   )
    end
end
