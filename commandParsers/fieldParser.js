const _ = require('lodash')
const {
    isArithmeticOperator,
    arithmeticExpressionPattern,
    stringFunctionPattern,
    aggregateFunctionPattern,
    comparisonOperatorPattern,
    arithmeticOperatorPattern,
    containsFunctionPattern,
    stringFunctionsNamePattern,
    aggregateFunctionsNamePattern,
} = require('../helpers/regex')
const prepareConditionsForParsing = require('./parserTools/prepareConditionsForParsing')
const findIndexOfClosingBracket = require('./parserTools/findIndexOfClosingBracket')

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
                left: parseField(splitExpression[0]),
                operator: splitExpression[1],
                right: parseField(splitExpression[2]),
            }
            conditions[AndOrSwitch].push(condition)
        }
    }

    return conditions
}

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

    return splitExpression.map((e) =>
        e === '*' ? parseField('**') : parseField(e)
    )
}

const parseFields = (fieldArray) => {
    const fieldObjects = fieldArray
        .join('')
        .split(',')
        .filter(Boolean)
        .map((f) => {
            return parseField(f)
        })

    return fieldObjects
}

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
                value: field === '**' ? '*' : field,
            }
        default:
            return {
                type: 'column',
                value: field,
            }
    }
}

const parseParameterFromStringFunction = (functionAsString) => {
    return parseField(
        functionAsString
            .replace(stringFunctionsNamePattern, '')
            .replace('(', '')
            .replace(')', '')
    )
}

const parseParameterFromAggregateFunction = (functionAsString) => {
    return parseField(
        functionAsString
            .replace(aggregateFunctionsNamePattern, '')
            .replace('(', '')
            .replace(')', '')
    )
}

module.exports = { parseConditions, parseExpression, parseFields, parseField }
