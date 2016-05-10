-- Reads the file into a string so that it can be parsed properly
function readFile(filename)
    newString = ""
    file = io.open(filename, "r")
    for line in file:lines() do
        newString = newString .. line ..  "\n"
    end
    return newString
end

-- Builds the n-gram dictionary (a hashmap where the n-gram points to a frequency)
function nGramBuilder(toParse, gramLength, isCharacters)
    local dictionary = {}
    local sequence = {}
    local toMatch = isCharacters and "." or "%a+"

    for gram in string.gmatch(toParse, toMatch) do
        table.insert(sequence, gram)
        if (#sequence == gramLength) then
            local entry = table.concat(sequence, " ")
            dictionary[entry] = (dictionary[entry] or 0) + 1
            table.remove(sequence, 1)
        end
    end

    return setmetatable(dictionary, {
                        __index = {length = (function()
                            count = 0
                            for i, v in pairs(dictionary) do
                                count = count + v
                            end
                            return count
                        end)()
                    }})
end

-- The result of the n-gram builder; used for testing & analysis purposes.
function nGramOutput(filename, n, threshold, useCharacters)
    for i, v in pairs(nGramBuilder(readFile(filename), n, useCharacters)) do
        if (v >= threshold) then
            print(string.format("%3d: %s", v, i))
        end
    end
end

-- Classifies a document as either file a or file b
function classify(aTrainingFile, bTrainingFile, testSet, nGram, useCharacters)
    local aDictionary = nGramBuilder(readFile(aTrainingFile), nGram, useCharacters)
    local bDictionary = nGramBuilder(readFile(bTrainingFile), nGram, useCharacters)
    local toMatch = useCharacters and "." or "%a+"

    local probA = 0
    local probB = 0

    local testDictionary = nGramBuilder(readFile(testSet), nGram, useCharacters)
    local sequence = {}
    for gram, frequency in pairs(testDictionary) do
        table.insert(sequence, gram)
        if (#sequence == nGram) then
            probA = probA + math.log((aDictionary[table.concat(sequence, " ")]) or (1/aDictionary.length))
            probB = probB + math.log((bDictionary[table.concat(sequence, " ")]) or (1/bDictionary.length))
            table.remove(sequence, 1)
        end
    end

    local sum = (math.exp(probA) + math.exp(probB) ~= 0 and math.exp(probA) + math.exp(probB)) or 1

    print(probA, math.exp(probA)/sum)
    print(probB, math.exp(probB)/sum)

    print(probA > probB and "File A" or "File B")
end
--[[
classify("gamespotTraining.dat", "gamesradarTraining.dat", "gamespotTest.dat", 1)
classify("gamespotTraining.dat", "gamesradarTraining.dat", "gamespotTest.dat", 2)
classify("gamespotTraining.dat", "gamesradarTraining.dat", "gamespotTest.dat", 3)

classify("gamespotTraining.dat", "gamesradarTraining.dat", "gamesradarTest.dat", 1)
classify("gamespotTraining.dat", "gamesradarTraining.dat", "gamesradarTest.dat", 2)
classify("gamespotTraining.dat", "gamesradarTraining.dat", "gamesradarTest.dat", 3)

classify("gamespotTraining.dat", "gamesradarTraining.dat", "gamespotTest.dat", 1, true)
classify("gamespotTraining.dat", "gamesradarTraining.dat", "gamespotTest.dat", 2, true)
classify("gamespotTraining.dat", "gamesradarTraining.dat", "gamespotTest.dat", 3, true)

classify("gamespotTraining.dat", "gamesradarTraining.dat", "gamesradarTest.dat", 1, true)
classify("gamespotTraining.dat", "gamesradarTraining.dat", "gamesradarTest.dat", 2, true)
classify("gamespotTraining.dat", "gamesradarTraining.dat", "gamesradarTest.dat", 3, true)
]]

-- Selects a random value based off of the frequencies for the n-gram table
function selectRandom(nGramTable)
    local val = math.random()
    local sum = 0
    for i, v in pairs(nGramTable) do
        sum = sum + v/nGramTable.length
        if (val <= sum) then
            return i
        end
    end
end

-- Matches the sequence to the beginning part of the entries of an n-gram dictionary (so that all n-grams
-- that start with say, two of three words, can be found)
function matchBeginning(nGramTable, sequence)
    sequence = sequence or ""
    local results = {}
    local sum = 0
    for i, v in pairs(nGramTable) do
        if string.match(i, sequence .. "%a+") then
            --print(i, v)
            results[i] = v
            sum = sum + v
        end
    end

    return setmetatable(results, {
                        __index = {length = sum}
                        })
end

-- Generates text based on the training file, n-gram to be used, and wordcount (also character count)
function generate(aTrainingFile, nGram, wordCount, useCharacters)
    local dictionary = nGramBuilder(readFile(aTrainingFile), nGram, useCharacters)
    local result = ""
    local current = selectRandom(dictionary)
    local toMatch = useCharacters and "." or "%a+"

    result = result .. current

    for i = 1, (wordCount - nGram) do
        local basis = string.match(result, ".+ (" .. toMatch .. " " .. toMatch .. ")")
        local nextStep = selectRandom(matchBeginning(dictionary, basis))
        if (not nextStep) then
            nextStep = string.match(selectRandom(dictionary), toMatch)
        end
        result = result .. " " .. nextStep
    end
    local final = ""
    local count = 1
    for i in string.gmatch(result, toMatch) do
        final = final .. i .. " "
        count = count + 1
        if count >= wordCount then
            print(final)
            return final
        end
    end
    print(result)
    return result
end

-- Calculates the perplexity of a string given the dictionary, the n-grams used, and whether or not it's characters
function calculatePerplexity(nGramDictionary, parse, nGram, useCharacters)
    local perp = 0
    local dictionary = nGramBuilder(parse, nGram, useCharacters)

    local sequence = {}
    for gram, frequency in pairs(dictionary) do
        table.insert(sequence, gram)
        if (#sequence == nGram) then
            -- Again, using logarithms to reduce underflow
            perp = perp + math.log((nGramDictionary[table.concat(sequence, " ")]) or (1/nGramDictionary.length))
            table.remove(sequence, 1)
        end
    end

    perp = -1/(#parse)*perp
    print("Perplexity: " .. math.exp(perp))
end

-- Performs a perplexity analysis on a file given an n-gram, a length of text to generate, and whether or not to use characters.
function doPerplexityAnalysis(file, nGram, length, useCharacters)
    calculatePerplexity(nGramBuilder(readFile(file), nGram, useCharacters), generate(file, nGram, length, useCharacters), nGram,useCharacters)
end

--[[
doPerplexityAnalysis("gamesradarTraining.dat", 2, 100, true)
doPerplexityAnalysis("gamesradarTraining.dat", 3, 100)
doPerplexityAnalysis("gamesradarTraining.dat", 2, 100)
doPerplexityAnalysis("gamespotTraining.dat", 3, 100)
doPerplexityAnalysis("gamespotTraining.dat", 1, 100)
]]