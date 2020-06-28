const {
    aggregateFunctionsNamePattern,
    stringFunctionsNamePattern,
} = require('../helpers/regex')

const {
    isAggregateFunction,
    isArithmeticExpression,
    isDistinctKeyword,
    isModifiedArithmeticOperator,
    isSortOrderKeyword,
    isStringFunction,
    isTextTypeInput,
    selectAll,
} = require('../helpers/isRegexTools')
const {
    transformSelectInputArrayIntoFieldsArray,
    transformOrderByInputArrayIntoOrderByFieldsArray,
} = require('./parserTools/arrayTransformationTools')
const splitExpressionIntoArray = require('./parserTools/splitExpressionIntoArray')

/**
 * Handles parsing of a field into field object.
 * @param {String} field field as string
 * @returns {object} field object
 */
const parseField = (field) => {
    switch (true) {
        case selectAll(field):
            return {
                type: 'all',
                value: field,
            }
        case isStringFunction(field):
            return {
                type: 'stringFunction',
                name: field.split('(')[0].toUpperCase(),
                value: field,
                param: parseParameterFromStringFunction(field),
            }
        case isAggregateFunction(field):
            return {
                type: 'aggregateFunction',
                name: field.split('(')[0].toUpperCase(),
                value: field,
                param: parseParameterFromAggregateFunction(field),
            }
        case isArithmeticExpression(field):
            return {
                type: 'expression',
                expressionParts: parseExpression(field),
                value: field,
            }
        case isTextTypeInput(field):
            return {
                type: 'text',
                value: field.replace(/'/g, ''),
            }
        case isModifiedArithmeticOperator(field):
            return {
                type: 'operator',
                value: field === '**' ? '*' : field,
            }
        case isSortOrderKeyword(field):
            return {
                type: 'order',
                value: field ? field.toLowerCase() : 'asc',
            }
        case !isNaN(field):
            return field
                ? {
                      type: 'integer',
                      value: Number(field),
                  }
                : null
        default:
            return {
                type: 'column',
                value: field,
            }
    }
}

/**
 * Handles parsing of an expression from the given string.
 * Switches multiply sign from * to ** because of conflict with all * sign (e.g. select *)
 * @param {String} expression expression as string
 */
const parseExpression = (expression) => {
    const splitExpression = splitExpressionIntoArray(expression)

    return splitExpression.map((e) =>
        e === '*' ? parseField('**') : parseField(e)
    )
}

/**
 * Handles parsing of fields in SELECT.
 * @param {string[]} fieldArray array containing the field information
 */
const parseInputFields = (fieldInputArray) => {
    if (isDistinctKeyword(fieldInputArray[0])) {
        return parseParametersFromDistinct(fieldInputArray.slice(1))
    }

    const fieldsArray = transformSelectInputArrayIntoFieldsArray(
        fieldInputArray
    )

    const parsedFields = fieldsArray.map((f) => {
        return parseField(f)
    })

    return parsedFields
}

/**
 * Returns fieldArray with distinct columns parsed in format
 * { type: 'distinct', value: [ { type: 'column', value: ... }, { type: 'column', value: ... } ] }
 * @param {*} fieldArray array without DISTINCT keyword, containing only columns separated with comma (,)
 */
const parseParametersFromDistinct = (selectInputArray) => {
    return [
        {
            type: 'distinct',
            value: parseInputFields(selectInputArray),
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
    parseExpression,
    parseInputFields,
    parseOrderByFields,
    parseField,
}
