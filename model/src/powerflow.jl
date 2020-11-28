function linespmax(nodes::Nodes, lines::Lines)
    lines_pmax = JuMP.Containers.DenseAxisArray(zeros(length(nodes.id), length(nodes.id)), nodes.id, nodes.id)

    for id in lines.id
        j = lines.from[id]
        k = lines.to[id]

        lines_pmax[j, k] = lines.pmax[id]
        lines_pmax[k, j] = lines.pmax[id]
    end

    return lines_pmax
end

function bprime(nodes::Nodes, lines::Lines)
    # Create the B_prime matrix in one step
    B_prime = JuMP.Containers.DenseAxisArray(zeros(length(nodes.id), length(nodes.id)), nodes.id, nodes.id)

    for id in lines.id
        j = lines.from[id]
        k = lines.to[id]

        X = lines.reactance[id]				# reactance

        B_prime[j, k] = 1/X					# Fill YBUS with susceptance
        B_prime[k, j] = 1/X					# Symmetric matrix
    end

    for j in nodes.id
        B_prime[j, j] = -sum(B_prime[j, :])
    end

    return B_prime
end
