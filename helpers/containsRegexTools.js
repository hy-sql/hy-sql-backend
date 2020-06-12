const {
    containsArithmeticOperatorPattern,
    comparisonOperatorPattern,
    logicalOperatorsNamePattern,
    stringFunctionsNamePattern,
    aggregateFunctionsNamePattern,
} = require('../helpers/regex')

const containsArithmeticOperator = (input) => {
    return containsArithmeticOperatorPattern.test(input)
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

const containsOperator = (input) => {
    return containsFunctions.some((f) => f(input))
}

const containsFunctions = [
    containsArithmeticOperator,
    containsComparisonOperator,
    containsLogicalOperator,
    containsStringFunctionsPattern,
    containsAggregateFunctionsPattern,
]

module.exports = {
    containsArithmeticOperator,
    containsComparisonOperator,
    containsLogicalOperator,
    containsStringFunctionsPattern,
    containsAggregateFunctionsPattern,
    containsOperator,
}
