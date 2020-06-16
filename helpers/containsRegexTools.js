const {
    arithmeticOperatorPattern,
    comparisonOperatorPattern,
    logicalOperatorsNamePattern,
    stringFunctionsNamePattern,
    aggregateFunctionsNamePattern,
} = require('../helpers/regex')

/** Tests whether a string contains an arithmetic operator.
 * @param {String} input string to test
 */
const containsArithmeticOperator = (input) => {
    return arithmeticOperatorPattern.test(input)
}

/** Tests whether a string contains a comparison operator.
 * @param {String} input string to test
 */
const containsComparisonOperator = (input) => {
    return comparisonOperatorPattern.test(input)
}

/** Tests whether a string contains a logical operator.
 * @param {String} input string to test
 */
const containsLogicalOperator = (input) => {
    return logicalOperatorsNamePattern.test(input)
}

/** Tests whether a string contains a string function.
 * @param {String} input string to test
 */
const containsStringFunctionsPattern = (input) => {
    return stringFunctionsNamePattern.test(input)
}

/** Tests whether a string contains an aggregate function.
 * @param {String} input string to test
 */
const containsAggregateFunctionsPattern = (input) => {
    return aggregateFunctionsNamePattern.test(input)
}

/** Tests whether a string contains at least one of the following:
 *    - arithmetic operator
 *    - comparison operator
 *    - logical operator
 *    - string function
 *    - aggregate function
 * @param {String} input string to test
 */
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
