const arithmeticOperator = RegExp('[+|-|*|/|%]', 'g')
const comparisonOperator = RegExp('[=|>|<|>=|<=|<>]', 'g')
const logicalOperator = RegExp(
    '[ALL|AND|ANY|BETWEEN|EXISTS|IN|LIKE|NOT|OR|SOME]',
    'g'
)

const containsArithmeticOperator = (input) => {
    return arithmeticOperator.test(input)
}

const containsComparisonOperator = (input) => {
    return comparisonOperator.test(input)
}

const containsLogicalOperator = (input) => {
    return logicalOperator.test(input)
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
