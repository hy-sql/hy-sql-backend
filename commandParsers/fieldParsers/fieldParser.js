const {
    isArithmeticOperator,
    arithmeticExpressionPattern,
    stringFunctionPattern,
    aggregateFunctionPattern,
} = require('../../helpers/regex')
const parseExpression = require('./expressionParser')
const {
    parseParameterFromStringFunction,
    parseParameterFromAggregateFunction,
} = require('../parserTools/parseParameterFromFunction')

const parseField = (field) => {
    switch (true) {
        case stringFunctionPattern.test(field):
            return {
                type: 'stringFunction',
                name: field.split('(')[0].toUpperCase(),
                value: field,
                param: parseParameterFromStringFunction(field),
            }
        case aggregateFunctionPattern.test(field):
            return {
                type: 'aggregateFunction',
                name: field.split('(')[0].toUpperCase(),
                value: field,
                param: parseParameterFromAggregateFunction(field),
            }
        case /^\*$/.test(field):
            return {
                type: 'all',
                value: field,
            }
        case arithmeticExpressionPattern.test(field):
            return {
                type: 'expression',
                value: parseExpression(field),
                stringValue: field,
            }
        case /^'\w+'/.test(field):
            return {
                type: 'string',
                value: field.replace(/'/g, ''),
            }
        case !isNaN(field):
            return {
                type: 'integer',
                value: Number(field),
            }
        case isArithmeticOperator.test(field):
            return {
                type: 'operator',
                value: field,
            }
        default:
            return {
                type: 'column',
                value: field,
            }
    }
}

module.exports = parseField
