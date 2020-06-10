const _ = require('lodash')
const {
    arithmeticExpressionPattern,
    comparisonOperatorPattern,
    stringFunctionPattern,
} = require('../../helpers/regex')
const {
    parseParameterFromStringFunction,
} = require('../parserTools/parseParameterFromFunction')
const prepareConditionsForParsing = require('../parserTools/prepareConditionsForParsing')
const findIndexOfClosingBracket = require('../parserTools/findIndexOfClosingBracket')
const parseExpression = require('./expressionParser')

const parseConditions = (slicedCommandArray) => {
    const conditionArray = prepareConditionsForParsing(slicedCommandArray)

    const conditions = { AND: [], OR: [] }

    const indexOfAnd = _.indexOf(conditionArray, 'AND')
    const indexOfOr = _.indexOf(conditionArray, 'OR')

    let AndOrSwitch = 'AND'

    if (indexOfAnd < 0 && indexOfOr >= 0) {
        AndOrSwitch = 'OR'
    } else if (indexOfAnd >= 0 && indexOfOr < 0) {
        AndOrSwitch = 'AND'
    } else {
        AndOrSwitch = indexOfAnd <= indexOfOr ? 'AND' : 'OR'
    }

    for (let i = 0; i < conditionArray.length; i++) {
        if (conditionArray[i] === '(') {
            const indexOfClosingBracket = findIndexOfClosingBracket(
                conditionArray,
                i
            )
            conditions[AndOrSwitch].push(
                parseConditions(
                    conditionArray.slice(i + 1, indexOfClosingBracket)
                )
            )
            i = indexOfClosingBracket + 1
        } else if (conditionArray[i] === 'AND') {
            AndOrSwitch = 'AND'
        } else if (conditionArray[i] === 'OR') {
            AndOrSwitch = 'OR'
        } else {
            const splitExpression = conditionArray[i].split(
                comparisonOperatorPattern
            )

            const condition = {
                left: parseConditionPart(splitExpression[0]),
                operator: splitExpression[1],
                right: parseConditionPart(splitExpression[2]),
            }
            conditions[AndOrSwitch].push(condition)
        }
    }

    return conditions
}

const parseConditionPart = (parsedField) => {
    switch (true) {
        case arithmeticExpressionPattern.test(parsedField):
            return {
                type: 'expression',
                value: parseExpression(parsedField),
                stringValue: parsedField,
            }
        case stringFunctionPattern.test(parsedField):
            return {
                type: 'stringFunction',
                name: parsedField.split('(')[0].toUpperCase(),
                value: parsedField,
                param: parseParameterFromStringFunction(parsedField),
            }
        case /^'\w+'/.test(parsedField):
            return {
                type: 'string',
                value: parsedField.replace(/'/g, ''),
            }
        case !isNaN(parsedField):
            return {
                type: 'integer',
                value: Number(parsedField),
            }
        default:
            return {
                type: 'column',
                value: parsedField,
            }
    }
}

module.exports = parseConditions
