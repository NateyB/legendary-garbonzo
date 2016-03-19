-- Q-learning in Figure 8.9
-- Procedure and OpenCV
math.randomseed(os.time() + os.clock())

scramble = function(set)
    local copy = {}
    for i = 1, #set do
        copy[((i + math.random(#set)) % #set) + 1] = set[i]
    end
    return copy
end

argmax = function(set, func)
    if #set < 1 then return nil end
    set = scramble(set)
    local max
    for i, v in pairs(set) do
        max = (max == nil and v) or (func(max) < func(v) and v) or max
    end
    return max
end

max = function(set, func)
    if #set < 1 then return nil end
    set = scramble(set)
    local max
    for i, v in pairs(set) do
        max = (max == nil and func(v)) or (max < func(v) and func(v)) or max
    end
    return max
end

sum = function(set)
    local sum = 0
    for i, v in pairs(set) do
        sum = sum + v
    end
    return sum
end

map = function(set, func)
    local new = {}
    for i, v in pairs(set) do
        new[i] = func(v, i)
    end
    return new
end

function generateGaussian(mean, stdDev, min)
    mean = mean or 0
    stdDev = stdDev or 1
    local x, w
    repeat
        x = 2*math.random() - 1
    until x*x < .5

    w = x*x
    w = math.sqrt(-2*math.log(w)/w)*x*stdDev + mean
    return (not min and w) or (min < w and w or min)
end


function vectorForm(vector)
    return setmetatable(vector, {
                        __call = function(this, arg)
                            return vector[arg]
                        end,
                        __mul = function(this, other)
                            local new = {}
                            if (type(this) == "number") then
                                return other*this
                            end
                            if (type(other) == "number") then
                                for i = 1, #this do
                                    new[i] = this[i]*other
                                end
                            else
                                for i = 1, #this do
                                    new[i] = this[i]*other[i]
                                end
                            end
                            return vectorForm(new)
                        end,
                        __add = function(this, other)
                            local new = {}
                            for i = 1, #this do
                                new[i] = this[i] + other[i]
                            end
                            return vectorForm(new)
                        end
                        })
end

function displayGrid(grid, displayFunc)
    displayFunc = displayFunc or (function(val) return val end)
    local output = "{"
    for i, v in pairs(grid) do
        local new = {}
        for j, k in pairs(v) do
            new[j] = displayFunc(k)
        end
        output = output .. "{" .. table.concat(new, ", "):sub(1, -1) .. "}\n"
    end
    return output:sub(1, -2) .. "}"
end

function getPolicy(world, thetas, tilings)
    local function getActivations(state, tilings, action)
        local phi
        for _, tiling in pairs(tilings) do
            local activations = vectorForm(tiling:getActivations(state, action))
            phi = (phi and (phi + activations)) or 1*activations
        end

        return phi
    end

    local function getBestAction(state)
        local qSubA = vectorForm({})
        local actions = world:getActions(state)

        for i, action in pairs(actions) do
            local activations = getActivations(state, tilings, action)
            qSubA[action] = sum(activations*thetas)
        end

        return argmax(actions, qSubA)
    end

    local policy = {}
    for row = 1, #world do
        policy[row] = {}
        for col = 1, #world[row] do
            policy[row][col] = getBestAction({row, col})
        end
    end
    return policy
end

function indexByValue(table)
    return setmetatable(table, {
                        __index = function(t, k)
                            for i, v in pairs(t) do
                                local result = v
                                for j, q in pairs(k) do
                                    if (type(q) == "table") then
                                        for l, r in pairs(q) do
                                            if i[j][l] ~= r then
                                                result = nil
                                            end
                                        end
                                    elseif i[j] ~= q then
                                        result = nil
                                    end
                                end
                                if result ~= nil then return result end
                            end
                        end,
                        __newindex = function(t, k, nV)
                            found = false
                            for i, v in pairs(t) do
                                local result = v
                                for j, q in pairs(k) do
                                    if (type(q) == "table") then
                                        for l, r in pairs(q) do
                                            if i[j][l] ~= r then
                                                result = nil
                                            end
                                        end
                                    elseif i[j] ~= q then
                                        result = nil
                                    end
                                end
                                if result ~= nil then
                                    t[i] = nV
                                    found = true
                                end
                            end
                            if (not found) then
                                rawset(t,k,nV)
                            end
                        end
                        })
end

--------------------------------------------Tuning Factors------------------------------------------
numIterations = 3600
lambda = 10
numTilings = 1
alpha = 1/(numTilings*10) -- 1/(m*10)
gamma = .85
epsilon = 1
epsilonGrowthRate = (numIterations - 1 > 0 and numIterations - 1 or 1)/numIterations
typicalReward = -.04
----------------------------------------------------------------------------------------------------

-- Generates a tiling of the tuple (x, y, depth, availableActions)
function tiling(world, boundaries, depth)
    for i, v in pairs(boundaries) do
        for j, k in pairs(v) do
            -- boundaries, depth, actions
            boundaries[i][j] = {boundaries[i][j][1], boundaries[i][j][2], depth, world:getActions({i,j})}
        end
    end
    return setmetatable(boundaries, {__index = {getActivations = function(this, state, action)
                        local r, c = state[1], state[2]
                        local activations = {}

                        for i = 1, #this do
                            local row = this[i]
                            for j = 1, #row do
                                local tile = row[j]
                                for q = 1, #tile[4] do
                                    if (r <= tile[1] and r > this[i - 1][j][1]) and (c <= tile[2] and c > this[i][j - 1][2]) and (action == nil or action == tile[4][q]) then -- action == nil or
                                        activations[#activations + 1] = 1
                                    else
                                        activations[#activations + 1] = 0
                                    end
                                end
                            end
                        end

                        return activations
                        end}})
end

function qLearning(world, theta, tilings)
    local function getActivations(state, tilings, action)
        local phi
        for _, tiling in pairs(tilings) do
            local activations = vectorForm(tiling:getActivations(state, action))
            phi = vectorForm((phi and (phi + activations)) or 1*activations)
        end

        return phi
    end

    theta = vectorForm(theta)

    local function runEpisode(state, theta, tilings)
        local eligibilityTrace = vectorForm(theta*0)
        local reward = 0

        local function getQSubA(world, state, theta, tilings)
            local qSubA = {}

            for _, action in pairs(world:getActions(state)) do
                local activations = getActivations(state, tilings, action)
                qSubA[action] = sum(activations*theta)
            end

            return vectorForm(qSubA)
        end

        while (world:get(state).type ~= "terminal") do
            local activations = vectorForm(getActivations(state, tilings))
            eligibilityTrace = eligibilityTrace + activations

            state, reward = world:applyAction(state, action)
            local delta = reward - sum(activations)

            local qSubA = getQSubA(world, state, theta, tilings)
            delta = delta + gamma*max(world:getActions(state), qSubA)
            eligibilityTrace = eligibilityTrace + getActivations(state, tilings)--, action)
            theta = theta + delta*alpha*(eligibilityTrace*activations)

            if math.random() > epsilon then
                action = argmax(world:getActions(state), getQSubA(world, state, theta, tilings))
                eligibilityTrace = lambda*gamma*eligibilityTrace
            else
                local possibleActions = world:getActions(state)
                action = possibleActions[math.random(#possibleActions)]
                eligibilityTrace = 0*eligibilityTrace
            end
        end

        return theta
    end

    local elapsedTime = os.time()

    local function displayIter(iter, epsilon, theta)
        local function condition(iter)
            return iter == numIterations or iter % math.floor(numIterations/10) == 0 or iter == 1
        end
        if (condition(iter)) then
            print(string.format("Iteration %" .. #tostring(numIterations) .. "d complete. Took %4d seconds. New epsilon: %f", iter, os.time() - elapsedTime, epsilon))
            -- print(sum(theta) == theta[33]+theta[34]+theta[35]+theta[36] and "Damnit." or "Working?")
            elapsedTime = os.time()
        end
    end

    for iter = 1, numIterations do
        local initState
        repeat
            local initRow = math.random(#world)
            local initCol = math.random(#world[initRow])
            initState = {initRow, initCol}
        until not (world:get(initState).type == "terminal")

        -- local resultThetas = runEpisode(initState, theta, tilings)
        theta = runEpisode(initState, theta, tilings)
        epsilon = epsilon*epsilonGrowthRate

        -- theta = vectorForm(map(resultThetas*(1/sum(resultThetas)), function(newTheta, at) return (iter == 1 and newTheta) or (theta[at]*(1 - 1/iter) + newTheta/iter) end))
        displayIter(iter, epsilon, theta)
    end

    return theta
end

function generateNTilings(world, n)
    local result = {}
    local xOffset, yOffset = 0, 0
    for i = 1, n do
        local thisTiling = map(world, function(val, row) local x = map(val, function(val, col) return {(row - 1) + yOffset, (col - 1) + xOffset} end) table.insert(x, {row - 1 + yOffset, #val + xOffset}) return x end)
        table.insert(thisTiling, map(thisTiling[#thisTiling], function(val, col) return {#thisTiling + yOffset, col - 1 + xOffset} end))
        thisTiling = tiling(world, thisTiling, i)
        table.insert(result, thisTiling)

        xOffset = math.random()
        yOffset = math.random()
    end

    return result
end

function getThetas(world, tiling)
    local thetas = {}
    for i = 1, sum(map(tiling, function(row, ind) return sum(map(row, function(col, ind2) return 4 end)) end)) do
        thetas[i] = -math.random()--0--math.random()
    end
    return thetas
end

function panel(type, reward)
    return setmetatable({}, {
                        __index = {type = type:lower(), reward = reward or typicalReward}
                        })
end

function make2DMaze(panels)
    return setmetatable(panels, {
        __index = {
            makeWithinBounds = function(this, state, action)
                local row, col = state[1], state[2]
                if row < 1 then
                    row = 1
                elseif row > #this then
                    row = #this
                end
                if col < 1 then
                    col = 1
                elseif col > #this[row] then
                    col = #this[row]
                end
                if this:get({row, col}).type == "wall" then
                    if action == "north" then
                        row = row + 1
                    elseif action == "west" then
                        col = col + 1
                    elseif action == "east" then
                        col = col - 1
                    elseif action == "south" then
                        row = row - 1
                    end
                end
                return {row, col}
            end,
            getActions = function(this, state)
                if (not state) then return {"north", "south", "west", "east"} end
                local row, col = state[1], state[2]
                local actions = {}
                if (row > #this) or (col > #this[row]) then
                    return {"north", "south", "west", "east"}
                end
                if (row > 1) and (this:get{row - 1, col}) and (not (this:get({row - 1, col}).type == "wall")) then
                    table.insert(actions, "north")
                end
                if (row < #this) and ((this:get({row + 1, col}) and (not (this:get({row + 1, col}).type == "wall")))) then
                    table.insert(actions, "south")
                end
                if (col > 1) and (this:get{row, col - 1}) and (not (this:get({row, col - 1}).type == "wall")) then
                    table.insert(actions, "west")
                end
                if (col < #this[row]) and (this:get{row, col + 1}) and (not (this:get({row, col + 1}).type == "wall")) then
                    table.insert(actions, "east")
                end

                return actions
            end,
            applyAction = function(this, state, action)
                local new = {}
                for i, v in pairs(state) do
                    new[i] = v
                end
                if action == "north" then
                    new[1] = new[1] - 1
                elseif action == "west" then
                    new[2] = new[2] - 1
                elseif action == "east" then
                    new[2] = new[2] + 1
                elseif action == "south" then
                    new[1] = new[1] + 1
                end
                new = this:makeWithinBounds(new, action)
                return new, this:get(new).reward
            end,
            get = function(this, state)
                return this[math.ceil(state[1])][math.ceil(state[2])]
            end
        }
    })
end

-- Initialization
function main(testWorlds)
    local numTests = #testWorlds
    for i, scenario in pairs(testWorlds) do
        local world = scenario.world
        local features = scenario.features
        print(string.format("Completing world %0" .. #tostring(numTests) .. "d of %d: ", i, numTests))
        print(displayGrid(world, function(panel) return panel.type end))
        print()
        thetas = qLearning(world, getThetas(world, features[1]), features)
        print("\nDerived policy:")
        print(displayGrid(getPolicy(world, thetas, features), function(action)
                          if action == "north" then
                            return "^"
                          elseif action == "west" then
                            return "<"
                          elseif action == "east" then
                            return ">"
                          elseif action == "south" then
                            return "v"
                          else
                            return "?"
                          end
                        end))
        print()
    end
end

function makeRadial(world)
    local features = {}
    for i, action in pairs(world:getActions()) do
        for row = 1, #world do
            for col = 1, #world[row] do
                table.insert(features, {row, col, 0, action})
            end
        end
    end

    features = setmetatable(features, {
                            __index = {
                                    getActivations = function(this, state, action)
                                        local y, x = state[1], state[2]
                                        local sqrt = math.sqrt
                                        local activations = {}

                                        local function contains(table, val)
                                            for i, v in pairs(table) do
                                                if v == val then
                                                    return true
                                                end
                                            end

                                            return false
                                        end

                                        for i = 1, #this do
                                            if (not action) or (contains(this[i][4], action)) then
                                                activations[i] = math.exp(-((y - this[i][1])^2 + (x - this[i][2])^2)/2)
                                            else
                                                activations[i] = 0
                                            end
                                        end
                                        return activations
                                    end
                                    }
                            })

    return {features}
end


testWorld = make2DMaze({
    {panel("open"), panel("open"), panel("open")},
    {panel("open"), panel("terminal", 1), panel("open")},
    {panel "open", panel "open", panel "open"}
                       })

russellWorld = make2DMaze({
    {panel("open"), panel("open"), panel("open"), panel("terminal", 1)},
    {panel("open"), panel("wall"), panel("open"), panel("terminal", -1)},
    {panel("open"), panel("open"), panel("open"), panel("open")}
})

radialFeatures = makeRadial(russellWorld)

testWorlds = {
    {world = russellWorld,
     features = radialFeatures},
    {world = testWorld,
     features = generateNTilings(testWorld, numTilings)}
}


main(testWorlds)