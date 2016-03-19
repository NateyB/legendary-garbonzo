-- Q-learning in Figure 8.9
math.randomseed(os.time() + os.clock())

argmax = function(set, func)
    if #set < 1 then return nil end
    local max
    for i, v in pairs(set) do
        max = (max == nil and v) or (func(max) < func(v) and v) or max
    end
    return max
end

max = function(set, func)
    if #set < 1 then return nil end
    local max
    for i, v in pairs(set) do
        max = (max == nil and func(v)) or (max < func(v) and func(v)) or max
    end
    return max
end

function sum(set)
    local sum = 0
    for i, v in pairs(set) do
        sum = sum + v
    end
    return sum
end

function map(set, func)
    new = {}
    for i, v in pairs(set) do
        new[i] = func(v, i)
    end
    return new
end

--------------------------------------------Tuning Factors------------------------------------------
lambda = .8
alpha = 1/200 -- 1/(m*10)
gamma = .15
epsilon = 1
epsilonDecreaseRate = .999
typicalReward = -.04
numIterations = 18000
----------------------------------------------------------------------------------------------------

panel = function(type, reward)
    return setmetatable({}, {
                        __index = {type = type:lower(), reward = reward or typicalReward}
                        })
end

russellWorld = setmetatable({
    {panel("open"), panel("open"), panel("open"), panel("terminal", 1)},
    {panel("open"), panel("Wall"), panel("open"), panel("terminal", -1)},
    {panel("open"), panel("open"), panel("open"), panel("open")}
}, {
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
            if this[row][col].type == "wall" then
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
        getActions = function(this)
            return {"north", "west", "east", "south"}
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
            return new, this[new[1]][new[2]].reward
        end,
        get = function(this, state)
            return this[state[1]][state[2]]
        end
    }
})

function tiling(boundaries)
    globalTilingCounter = (globalTilingCounter and (globalTilingCounter + 1)) or 0
    local depth = globalTilingCounter
    for i, v in pairs(boundaries) do
        for j, k in pairs(v) do
            boundaries[i][j] = {boundaries[i][j], depth}
        end
    end
    return setmetatable(boundaries, {__index = {getFeatures = function(this, world, state, action)
                        state = world:applyAction(state, action)
                        local y, x = state[1] - 1, state[2] - 1
                        local row, col
                        local location = 0

                        for i = 1, #this do
                            if y < this[i][1][1] then
                                row = i - 1
                                break
                            elseif y == this[i][1][1] then
                                row = i
                                break
                            end
                        end

                        for i = 1, #this[row] do
                            if x < (this[row][i][1] - this[row][1][1]) then
                                col = i - 1
                                break
                            elseif x == this[row][i][1] - this[row][1][1] then
                                col = i
                                break
                            end
                        end
                        for i = 1, row-1 do
                            location = location + #this[row]
                        end
                        return {location + col} -- Do not subtract 1 from this value because that would render it 0-indexed
                    end}})
end

-- function indexByValue(table)
--     return setmetatable(table, {
--                         __index = function(t, k)
--                             for i, v in pairs(t) do
--                                 local result = v
--                                 for j, q in pairs(k) do
--                                     if (type(q) == "table") then
--                                         for l, r in pairs(q) do
--                                             if i[j][l] ~= r then
--                                                 result = nil
--                                             end
--                                         end
--                                     elseif i[j] ~= q then
--                                         result = nil
--                                     end
--                                 end
--                                 if result ~= nil then return result end
--                             end
--                         end,
--                         __newindex = function(t, k, nV)
--                             found = false
--                             for i, v in pairs(t) do
--                                 local result = v
--                                 for j, q in pairs(k) do
--                                     if (type(q) == "table") then
--                                         for l, r in pairs(q) do
--                                             if i[j][l] ~= r then
--                                                 result = nil
--                                             end
--                                         end
--                                     elseif i[j] ~= q then
--                                         result = nil
--                                     end
--                                 end
--                                 if result ~= nil then
--                                     t[i] = nV
--                                     found = true
--                                 end
--                             end
--                             if (not found) then
--                                 rawset(t,k,nV)
--                             end
--                         end
--                         })
-- end


local function qLearning(world, theta, tilings)
    theta = setmetatable(theta, {
                         __call = function(this, arg)
                            return theta[arg]
                        end})
    local function getFeatures(world, state, action, tilings)
        local allFeatures = {}
        for _, tiling in pairs(tilings) do
            local features = tiling:getFeatures(world, state, action)
            for i, v in pairs(features) do
                table.insert(allFeatures, v)
            end
        end

        return allFeatures
    end

    local function runEpisode(state, action, tilings)
        local eligibilityTrace = map(theta, function() return 0 end)
        local features = getFeatures(world, state, action, tilings)
        local reward

        repeat
            map(features, function(val) eligibilityTrace[val] = eligibilityTrace[val] + 1 end)
            local qSubA = setmetatable({}, {__call = function(this, arg) return this[arg] end})
            state, reward = world:applyAction(state, action)
            local delta = reward - sum(map(features, theta))

            for _, a in pairs(world:getActions()) do
                local features = getFeatures(world, state, action, tilings)
                qSubA[a] = sum(map(features, theta))
            end

            delta = delta + gamma*max(world:getActions(), qSubA)
            theta = setmetatable(map(theta, function(val, ind) return val + alpha*delta*eligibilityTrace[ind] end), {__call = function(this, arg) return this[arg] end})

            if (math.random() > epsilon) then
                action = argmax(world:getActions(), function(action) return sum(map(getFeatures(world, state, action, tilings), theta)) end)
                eligibilityTrace = map(eligibilityTrace, function(item) return gamma*lambda*item end)
            else
                action = world:getActions()[math.random(#world:getActions())]
                eligibilityTrace = map(eligibilityTrace, function() return 0 end)
            end
        until world:get(state).type == "terminal"

        return theta
    end

    local function displayIter(iter)
        local function condition(iter)
            return iter == numIterations or iter % math.floor(numIterations/10) == 0 or iter == 1
        end
        if (condition(iter)) then
            print("Iteration " .. iter .. " complete. Epsilon: " .. epsilon)
        end
    end

    for iter = 1, numIterations do
        local initRow = math.random(#world)
        local initCol = math.random(#world[initRow])
        local initState = {initRow, initCol}
        local possibleActions = world:getActions()

        local resultThetas = runEpisode(initState, possibleActions[math.random(#possibleActions)], tilings)
        theta = setmetatable(map(resultThetas, function(parameter, at) return (iter == 1 and parameter) or (parameter/(1 - 1/iter) + theta[at]/iter) end), {
                         __call = function(this, arg)
                            return theta[arg]
                        end})
        displayIter(iter, epsilon)
        epsilon = epsilon*epsilonDecreaseRate
    end

    return theta
end

local tiling1 = tiling{
    {0, 1, 2, 3, 4},
    {1, 2, 3, 4, 5},
    {2, 3, 4, 5, 6},
    {3, 4, 5, 6, 7}
}

local tiling2 = {}

local offset = math.random() -- TODO Offset by the same amount in both directions?

for i, v in pairs(tiling1) do
    tiling2[i] = {}
    for j, k in pairs(v) do
        tiling2[i][j] = k[1] - offset
    end
end

tiling2 = tiling(tiling2)

function displayGrid(grid, displayFunc)
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


local thetas = {}
for i = 1, sum(map(tiling1, function(row) return #row end)) do
    thetas[i] = math.random()
end
map(thetas, function(val, ind) print(string.format("Theta %2d: %15.8f", ind, val)) end)
print()

thetas = qLearning(russellWorld, thetas, {tiling1, tiling2})
local function getPolicy(world, thetas, tilings)
    local function getFeatures(world, state, action, tilings) -- TODO implement
        local allFeatures = {}
        for _, tiling in pairs(tilings) do
            local features = tiling:getFeatures(world, state, action)
            for i, v in pairs(features) do
                table.insert(allFeatures, v)
            end
        end

        return allFeatures
    end

    function getBestAction(state, thetas)
        local qSubA = {}

        for _, a in pairs(world:getActions()) do
            local features = getFeatures(world, state, a, tilings)
            qSubA[a] = sum(map(features, thetas))
        end

        return argmax(world:getActions(), function(act) return qSubA[act] end)
    end

    local policy = {}
    for row = 1, 3 do
        policy[row] = {}
        for col = 1, 4 do
            policy[row][col] = getBestAction({row, col}, thetas)
        end
    end
    return policy
end

print()
map(thetas, function(val, ind) print(string.format("Theta %2d: %15.8f", ind, val)) end)
print("\nDerived policy:\n")
print(displayGrid(getPolicy(russellWorld, thetas, {tiling1, tiling2}), function(action)
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