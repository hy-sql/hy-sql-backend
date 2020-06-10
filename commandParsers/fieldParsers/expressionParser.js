const _ = require('lodash')
const parseField = require('./fieldParser')
const {
    arithmeticOperatorPattern,
    containsFunctionPattern,
    // isArithmeticOperator,
    // stringFunctionPattern,
    // aggregateFunctionPattern,
} = require('../../helpers/regex')
// const {
//     parseParameterFromStringFunction,
//     parseParameterFromAggregateFunction,
// } = require('../parserTools/parseParameterFromFunction')

const parseExpression = (expression) => {
    const splitExpression = _.flatten(
        expression
            .replace(containsFunctionPattern, ' $1 ')
            .split(' ')
            .filter(Boolean)
            .map((e) =>
                containsFunctionPattern.test(e)
                    ? e
                    : e.split(arithmeticOperatorPattern).filter(Boolean)
            )
            .filter(Boolean)
    )

    // return splitExpression.map((e) => parseExpressionFields(e))
    return splitExpression.map((e) => parseField(e))
}

// const parseExpressionFields = (expressionElement) => {
//     switch (true) {
//         case stringFunctionPattern.test(expressionElement):
//             return {
//                 type: 'stringFunction',
//                 name: expressionElement.split('(')[0].toUpperCase(),
//                 value: expressionElement,
//                 param: parseParameterFromStringFunction(expressionElement),
//             }
//         case aggregateFunctionPattern.test(expressionElement):
//             return {
//                 type: 'aggregateFunction',
//                 name: expressionElement.split('(')[0].toUpperCase(),
//                 value: expressionElement,
//                 param: parseParameterFromAggregateFunction(expressionElement),
//             }
//         case /^'\w+'/.test(expressionElement):
//             return {
//                 type: 'string',
//                 value: expressionElement.replace(/'/g, ''),
//             }
//         case !isNaN(expressionElement):
//             return {
//                 type: 'integer',
//                 value: Number(expressionElement),
//             }
//         case isArithmeticOperator.test(expressionElement):
//             return {
//                 type: 'operator',
//                 value: expressionElement,
//             }
//         default:
//             return {
//                 type: 'column',
//                 value: expressionElement,
//             }
//     }
// }

module.exports = parseExpression
