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
    distinctKeywordPattern,
    stringFunctionPattern,
    stringFunctionsNamePattern,
    textInputPattern,
} = require('../helpers/regex')
const findIndexOfClosingBracket = require('./parserTools/findIndexOfClosingBracket')
const {
    transformSplitConditionsIntoConditionsArray,
    transformOrderByInputArrayIntoOrderByFieldsArray,
} = require('./parserTools/arrayTransformationTools')

/**
 * Handles parsing of conditions from the given array.
 * @param {string[]} slicedCommandArray array containing the conditions
 */
const parseConditions = (slicedCommandArray) => {
    const conditionArray = transformSplitConditionsIntoConditionsArray(
        slicedCommandArray
    )

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

                conditions[AndOrSwitch] = conditions[AndOrSwitch].concat(
                    parseConditions(
                        conditionArray.slice(i + 1, indexOfClosingBracket)
                    )
                )
                i = indexOfClosingBracket
                break
            }
            case ')':
                break
            case 'AND':
                AndOrSwitch = 'AND'
                break
            case 'OR':
                AndOrSwitch = 'OR'
                break
            default: {
                if (comparisonOperatorPattern.test(conditionArray[i])) {
                    const splitExpression = conditionArray[i].split(
                        comparisonOperatorPattern
                    )

                    const condition = {
                        left: parseField(splitExpression[0]),
                        operator: splitExpression[1],
                        right: parseField(splitExpression[2]),
                    }
                    conditions[AndOrSwitch] = conditions[AndOrSwitch].concat(
                        condition
                    )
                } else {
                    conditions[AndOrSwitch] = conditions[AndOrSwitch].concat(
                        conditionArray[i]
                    )
                }
            }
        }
    }

    return conditions
}

/**
 * Handles parsing of an expression from the given string.
 * @param {String} expression expression as string
 */
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

/**
 * Handles parsing of fields in SELECT.
 * @param {string[]} fieldArray array containing the field information
 */
const parseSelectFields = (fieldArray) => {
    if (distinctKeywordPattern.test(fieldArray[0])) {
        return parseParametersFromDistinct(fieldArray.slice(1))
    }

    const selectFields = fieldArray
        .join('')
        .split(',')
        .filter(Boolean)
        .map((f) => {
            return parseField(f)
        })

    return selectFields
}

/**
 * Returns fieldArray with distinct columns parsed in format
 * { type: 'distinct', value: [ { type: 'column', value: ... }, { type: 'column', value: ... } ] }
 * @param {*} fieldArray array without DISTINCT keyword, containing only columns separated with comma (,)
 */
const parseParametersFromDistinct = (fieldArray) => {
    return [
        {
            type: 'distinct',
            value: parseSelectFields(fieldArray),
        },
    ]
}

/*
 * Handles parsing of fields in ORDER BY.
 * @param {string[]} fieldArray array containing the field information
 */
const parseOrderByFields = (fieldArray) => {
    const orderByFields = transformOrderByInputArrayIntoOrderByFieldsArray(
        fieldArray
    )

    return orderByFields.map((f) => {
        const column = parseField(f[0])

        if (column) column.order = f[1] ? parseField(f[1]) : parseField('asc')

        return column
    })
}

/**
 * Handles parsing of a field into field object.
 * @param {String} field field as string
 * @returns {object} field object
 */
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
                expressionParts: parseExpression(field),
                value: field,
            }
        case textInputPattern.test(field):
            return {
                type: 'text',
                value: field.replace(/'/g, ''),
            }
        case !isNaN(field):
            return field
                ? {
                      type: 'integer',
                      value: Number(field),
                  }
                : null
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

/**
 * Handles parsing of a parameter from a string function.
 * @param {string[]} functionAsString array containing the string function
 */
const parseParameterFromStringFunction = (functionAsString) => {
    return parseField(
        functionAsString
            .replace(stringFunctionsNamePattern, '')
            .replace('(', '')
            .replace(')', '')
    )
}

/**
 * Handles parsing of a parameter from a aggregate function.
 * @param {string[]} functionAsString array containing the aggregate function
 */
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
