const {
    arithmeticOperatorPattern,
    comparisonOperatorPattern,
    logicalOperatorPattern,
    stringFunctionsPattern,
    aggregateFunctionsPattern,
} = require('../utils/regex')

const containsArithmeticOperator = (input) => {
    return arithmeticOperatorPattern.test(input)
}

const containsComparisonOperator = (input) => {
    return comparisonOperatorPattern.test(input)
}

const containsLogicalOperator = (input) => {
    return logicalOperatorPattern.test(input)
}

const containsStringFunctionsPattern = (input) => {
    return stringFunctionsPattern.test(input)
}

const containsAggregateFunctionsPattern = (input) => {
    return aggregateFunctionsPattern.test(input)
}

const containsFunctions = [
    containsArithmeticOperator,
    containsComparisonOperator,
    containsLogicalOperator,
    containsStringFunctionsPattern,
    containsAggregateFunctionsPattern,
]

const containsOperator = (input) => {
    return containsFunctions.some((f) => f(input))
}

module.exports = module.exports = module.exports = module.exports = {
    containsArithmeticOperator,
    containsComparisonOperator,
    containsLogicalOperator,
    containsStringFunctionsPattern,
    containsAggregateFunctionsPattern,
    containsOperator,
}
