function dayslicer(hours::UnitRange{Int64})
	if (length(hours) < 24)
		return hours
	end

	daylasthour = filter(t -> t%24 == 0, hours)

	hours_split = Vector{UnitRange{Int64}}()
	append!(hours_split, [first(hours):daylasthour[1]])

	for d in daylasthour[2:end]
	    start_int = last(last(hours_split)) + 1
	    end_int = d
	    append!(hours_split, [start_int:end_int])
	end

	if (last(hours) > last(daylasthour))
	    append!(hours_split, [(hours_split[end][end]+1):hours[end]])
	end

	return hours_split
end

function dayslicer(hours_string::String)
	sepidx = findfirst(":", hours_string)
	hour_first = SubString(hours_string, 1, sepidx[1]-1) |>
				 x -> parse(Int, x)
	hour_last = SubString(hours_string, sepidx[1]+1, lastindex(hours_string)) |>
				 x -> parse(Int, x)

	hours = hour_first:hour_last
	return dayslicer(hours)
end


function hoursInWeek(x::Int64)
    if x <= 52
        return (24*7*x-167:24*7*x)
    elseif x == 53
        return (24*7*52+1):8760
    else
        return hoursInWeek(x-53)
    end
end

function hoursInDay(x::Int64)
	if x <= 365
        return (24*x-23:24*x)
    else
        return hoursInDay(x-365)
    end
end

function hoursInHalfDay(x::Int64)
	if x <= 730
        return (12*x-11:12*x)
    else
        return hoursInHalfDay(x-730)
    end
end

function has(x, y)
	x == y
end

function getKeyVector(df::Dict, value)
	key_value = [k for (k,v) in df if has(v, value)]
	return key_value
end
