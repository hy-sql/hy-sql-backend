const _ = require('lodash')
const {
    aggregateFunctionPattern,
    aggregateFunctionsNamePattern,
    arithmeticExpressionPattern,
    containsArithmeticOperatorPattern,
    comparisonOperatorPattern,
    containsFunctionPattern,
    modifiedArithmeticOperator,
    sortOrderKeywordPattern,
    stringFunctionPattern,
    stringFunctionsNamePattern,
    textInputPattern,
} = require('../helpers/regex')
const prepareConditionsForParsing = require('./parserTools/prepareConditionsForParsing')
const findIndexOfClosingBracket = require('./parserTools/findIndexOfClosingBracket')

const parseConditions = (slicedCommandArray) => {
    const conditionArray = prepareConditionsForParsing(slicedCommandArray)

    const conditions = { AND: [], OR: [] }

    const indexOfAnd = _.indexOf(conditionArray, 'AND')
    const indexOfOr = _.indexOf(conditionArray, 'OR')

    let AndOrSwitch = indexOfOr < (indexOfAnd >= 0) ? 'AND' : 'OR'

    for (let i = 0; i < conditionArray.length; i++) {
        switch (conditionArray[i]) {
            case '(': {
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
                break
            }
            case 'AND':
                AndOrSwitch = 'AND'
                break
            case 'OR':
                AndOrSwitch = 'OR'
                break
            default: {
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
                    : e.split(containsArithmeticOperatorPattern).filter(Boolean)
            )
            .filter(Boolean)
    )

    return splitExpression.map((e) =>
        e === '*' ? parseField('**') : parseField(e)
    )
}

const parseSelectFields = (fieldArray) => {
    const selectFields = fieldArray
        .join('')
        .split(',')
        .filter(Boolean)
        .map((f) => {
            return parseField(f)
        })

    return selectFields
}

const parseOrderByFields = (fieldArray) => {
    const orderByFields = fieldArray
        .join(' ')
        .trim()
        .split(',')
        .map((f) => f.trim())
        .map((f) => f.split(' '))
        .map((f) => {
            const column = parseField(f[0])
            column.order = parseField(f[1])

            return column
        })

    return orderByFields
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
        case textInputPattern.test(field):
            return {
                type: 'text',
                value: field.replace(/'/g, ''),
            }
        case !isNaN(field):
            return {
                type: 'integer',
                value: Number(field),
            }
        case modifiedArithmeticOperator.test(field):
            return {
                type: 'operator',
                value: field === '**' ? '*' : field,
            }
        case sortOrderKeywordPattern.test(field):
            return {
                type: 'order',
                value: field ? field.toLowerCase() : 'asc',
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

module.exports = {
    parseConditions,
    parseExpression,
    parseSelectFields,
    parseOrderByFields,
    parseField,
}
