const {
    aggregateFunctionPattern,
    arithmeticExpressionPattern,
    arithmeticOperatorPattern,
    distinctKeywordPattern,
    fieldsSplitByCommaPattern,
    functionPattern,
    modifiedArithmeticOperatorPattern,
    selectAllPattern,
    sortOrderKeywordPattern,
    stringFunctionPattern,
    textTypeInputPattern,
} = require('./regex')

const isAggregateFunction = (input) => aggregateFunctionPattern.test(input)

const isArithmeticExpression = (input) =>
    arithmeticExpressionPattern.test(input)

const isArithmeticOperator = (input) => arithmeticOperatorPattern.test(input)

const isDistinctKeyword = (input) => distinctKeywordPattern.test(input)

const isFunction = (input) => functionPattern.test(input)

const isModifiedArithmeticOperator = (input) =>
    modifiedArithmeticOperatorPattern.test(input)

const isSortOrderKeyword = (input) => sortOrderKeywordPattern.test(input)

const isStringFunction = (input) => stringFunctionPattern.test(input)

const isTextTypeInput = (input) => textTypeInputPattern.test(input)

const fieldsSplitByComma = (input) => fieldsSplitByCommaPattern.test(input)

const selectAll = (input) => selectAllPattern.test(input)

module.exports = {
    isAggregateFunction,
    isArithmeticExpression,
    isArithmeticOperator,
    isDistinctKeyword,
    isFunction,
    isModifiedArithmeticOperator,
    isSortOrderKeyword,
    isStringFunction,
    isTextTypeInput,
    fieldsSplitByComma,
    selectAll,
}
