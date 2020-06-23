const {
    aggregateFunctionPattern,
    aggregateFunctionsNamePattern,
    arithmeticExpressionPattern,
    modifiedArithmeticOperator,
    sortOrderKeywordPattern,
    distinctKeywordPattern,
    stringFunctionPattern,
    stringFunctionsNamePattern,
    textInputPattern,
} = require('../helpers/regex')
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
const parseSelectFields = (selectInputArray) => {
    if (distinctKeywordPattern.test(selectInputArray[0])) {
        return parseParametersFromDistinct(selectInputArray.slice(1))
    }

    const fieldsArray = transformSelectInputArrayIntoFieldsArray(
        selectInputArray
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
            value: parseSelectFields(selectInputArray),
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
    parseSelectFields,
    parseOrderByFields,
    parseField,
}
