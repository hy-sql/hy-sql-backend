const {
    arithmeticOperatorPattern,
    comparisonOperatorPattern,
    logicalOperatorsNamePattern,
    stringFunctionsNamePattern,
    aggregateFunctionsNamePattern,
} = require('../utils/regex')

const containsArithmeticOperator = (input) => {
    return arithmeticOperatorPattern.test(input)
}

const containsComparisonOperator = (input) => {
    return comparisonOperatorPattern.test(input)
}

const containsLogicalOperator = (input) => {
    return logicalOperatorsNamePattern.test(input)
}

const containsStringFunctionsPattern = (input) => {
    return stringFunctionsNamePattern.test(input)
}

const containsAggregateFunctionsPattern = (input) => {
    return aggregateFunctionsNamePattern.test(input)
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
