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

------------------------------------ Tuning factors, I suppose -------------------------------------
alpha = .125
gamma = .95
epsilon = .05
typicalReward = -.04
numIterations = 100
----------------------------------------------------------------------------------------------------

panel = function(type, reward)
    return setmetatable({}, {
                        __index = {type = type:lower(), reward = reward or typicalReward}
                        })
end

russellWorld = setmetatable({
    {panel("Open"), panel("Open"), panel("Open"), panel("Terminal", 1)},
    {panel("Open"), panel("Wall"), panel("Open"), panel("Terminal", -1)},
    {panel("Open"), panel("Open"), panel("Open"), panel("Open")}
}, {
    __index = {
        makeWithinBounds = function(this, state, action)
            if state[1] < 1 then
                state[1] = 1
            elseif state[1] > #this then
                state[1] = #this
            end
            if state[2] < 1 then
                state[2] = 1
            elseif state[2] > #this[state[1]] then
                state[2] = #this[state[1]]
            end
            if this[state[1]][state[2]].type == "wall" then
                if action == "north" then
                    state[1] = state[1] + 1
                elseif action == "west" then
                    state[2] = state[2] + 1
                elseif action == "east" then
                    state[2] = state[2] - 1
                elseif action == "south" then
                    state[1] = state[1] - 1
                end
            end
            return state
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
            return new
        end,
        get = function(this, state)
            return this[state[1]][state[2]]
        end
    }
})

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

function qLearning(thisWorld, explorationFunction)
    local qTable = indexByValue {} -- Initialize to zero
    local NSubSA = indexByValue {} -- Frequency for State-Action pairs, initially zero
    local prevState, prevAction, prevReward

    local function getQ(thing)
        return (tostring(qTable[thing]) ~= "nan" and qTable[thing] or 0)
    end

    local function qLearningAgent(percept)
        local cState, rPrime = percept[1], percept[2]
        local qFunction = function(state)
            return function(action)
                return getQ{state,action}
            end
        end
        if prevState and prevState.type == "terminal" then qTable[{prevState, "none"}] = rPrime end
        if prevState ~= null then
            local stateActionPair = {prevState, prevAction}
            NSubSA[stateActionPair] = (NSubSA[stateActionPair] or 0) + 1
            qTable[stateActionPair] = getQstateActionPair + alpha*NSubSA[stateActionPair]*(prevReward + gamma*max(thisWorld.getActions(), qFunction(cState)) - (qTable[stateActionPair] or 0))
        end
        prevState = cState
        prevAction = explorationFunction(thisWorld, cState, action, qTable, NSubSA)
        prevReward = rPrime
        return prevAction
    end

    for i = 1, numIterations do
        local initRow = math.random(#thisWorld)
        local initCol = math.random(#thisWorld[initRow])
        local initState = {initRow, initCol}
        local percept = {initState, thisWorld:get(initState).reward}
        local nState, nReward
        repeat
            qLearningAgent(percept)
            nState = thisWorld:applyAction(prevState, prevAction)
            nReward = thisWorld:get(nState).reward
            percept = {nState, nReward}
            --print(unpack(nState))
        until thisWorld:get(nState).type == "terminal"
        qTable[{nState, "none"}] = thisWorld:get(nState).reward
        print("Iteration " .. i .. " complete.")
    end

    return qTable, NSubSA
end

function naiveExploration(world, state, action, qVals, frequencies)
    if (math.random() > epsilon) then
        local thing = {state, action}
        return argmax(world:getActions(), function(action) return (tostring(qVals[thing]) ~= "nan" and qVals[thing] or 0) end)
    else
        return world:getActions()[math.random(#world:getActions())]
    end
end

local qVals, nSubs = qLearning(russellWorld, naiveExploration)

for i, v in pairs(qVals) do
    print(string.format("(%1d, %1d)-%-6s: Q: %10.6f; Frequency: %4d", i[1][1], i[1][2], i[2], v, nSubs[i] or 0))
end


-- ei <- ei + phi(i)
-- or ei <- phi(i)
-- Radial Basis Function; Q-learning using argmax a