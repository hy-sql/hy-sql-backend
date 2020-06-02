const {
    arithmeticOperatorPattern,
    comparisonOperatorPattern,
    logicalOperatorPattern,
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

const containsFunctions = [
    containsArithmeticOperator,
    containsComparisonOperator,
    containsLogicalOperator,
]

const containsOperator = (input) => {
    return containsFunctions.some((f) => f(input))
}

module.exports = module.exports = module.exports = module.exports = {
    containsOperator,
    containsLogicalOperator,
    containsComparisonOperator,
    containsArithmeticOperator,
}
