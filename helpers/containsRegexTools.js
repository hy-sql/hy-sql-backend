const {
    containsAggregateFunctionPattern,
    containsArithmeticExpressionPattern,
    containsArithmeticOperatorPattern,
    containsArithmeticOperatorWithWhiteSpacePattern,
    containsComparisonOperatorPattern,
    containsComparisonOperatorWithWhiteSpacePattern,
    containsFunctionPattern,
    containsFunctionWithWhiteSpacesPattern,
    containsStringFunctionPattern,
    logicalOperatorsNamePattern,
} = require('../helpers/regex')

/**
 * Tests whether a string contains an aggregate function.
 * @param {String} input string to test
 */
const containsAggregateFunction = (input) => {
    return containsAggregateFunctionPattern.test(input)
}

/**
 * Tests whether a string contains an arithmetic expression.
 * @param {String} input string to test
 */
const containsArithmeticExpression = (input) => {
    return containsArithmeticExpressionPattern.test(input)
}

/**
 * Tests whether a string contains an arithmetic operator.
 * @param {String} input string to test
 */
const containsArithmeticOperator = (input) => {
    return containsArithmeticOperatorPattern.test(input)
}

/**
 * Tests whether a string contains an arithmetic operator allowing whitespace.
 * @param {String} input string to test
 */
const containsArithmeticOperatorWithWhiteSpace = (input) => {
    return containsArithmeticOperatorWithWhiteSpacePattern.test(input)
}

/**
 * Tests whether a string contains a comparison operator.
 * @param {String} input string to test
 */
const containsComparisonOperator = (input) => {
    return containsComparisonOperatorPattern.test(input)
}

/**
 * Tests whether a string contains a comparison operator allowing whitespace.
 * @param {String} input string to test
 */
const containsComparisonOperatorWithWhiteSpace = (input) => {
    return containsComparisonOperatorWithWhiteSpacePattern.test(input)
}

/**
 * Tests whether a string contains a string function or an aggregate function.
 * @param {String} input string to test
 */
const containsFunction = (input) => {
    containsFunctionPattern.test(input)
}

/**
 * Tests whether a string contains a string function or an aggregate function
 * allowing whitespace.
 * @param {String} input string to test
 */
const containsFunctionWithWhiteSpaces = (input) => {
    return containsFunctionWithWhiteSpacesPattern.test(input)
}

/**
 * Tests whether a string contains a string function.
 * @param {String} input string to test
 */
const containsStringFunction = (input) => {
    return containsStringFunctionPattern.test(input)
}

/**
 * Tests whether a string contains a logical operator.
 * Not in use
 * @param {String} input string to test
 */
const containsLogicalOperator = (input) => {
    return logicalOperatorsNamePattern.test(input)
}

module.exports = {
    containsAggregateFunction,
    containsArithmeticExpression,
    containsArithmeticOperator,
    containsArithmeticOperatorWithWhiteSpace,
    containsComparisonOperator,
    containsComparisonOperatorWithWhiteSpace,
    containsFunction,
    containsFunctionWithWhiteSpaces,
    containsStringFunction,
    containsLogicalOperator,
}
