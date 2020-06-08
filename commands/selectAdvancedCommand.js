const util = require('util')
const {
    SelectAdvancedSchema,
    SelectAdvancedWhereSchema,
    SelectAdvancedOrderBySchema,
    SelectAdvancedWhereOrderBySchema,
} = require('../models/SelectAdvancedSchema')
const { parseWhereAdvanced } = require('./whereAdvancedCommand')
const { queryContainsWhereKeyword } = require('./whereCommand')
const {
    parseOrderBy,
    hasOrderByKeywords,
    hasWhereOrderByKeywords,
} = require('./orderByCommand')
const {
    arithmeticOperatorPattern,
    arithmeticExpressionPattern,
    stringFunctionPattern,
    aggregateFunctionPattern,
    containsAggregateFunctionPattern,
} = require('../utils/regex')
const {
    parseColumnFromStringFunction,
    parseColumnFromAggregateFunction,
} = require('../utils/parseColumnFromFunction')

const parseCommand = (fullCommandAsStringArray) => {
    if (hasWhereOrderByKeywords(fullCommandAsStringArray)) {
        return parseWhereOrderBy(fullCommandAsStringArray)
    } else if (hasOrderByKeywords(fullCommandAsStringArray)) {
        return parseSelectOrderBy(fullCommandAsStringArray)
    }
    if (queryContainsWhereKeyword(fullCommandAsStringArray)) {
        return parseWhere(fullCommandAsStringArray)
    }

    return parseSelectAdvancedCommand(fullCommandAsStringArray)
}

const parseBaseCommand = (fullCommandAsStringArray) => {
    const indexOfFrom = fullCommandAsStringArray.findIndex(
        (c) => c.toUpperCase() === 'FROM'
    )

    const parsedCommand = {
        name: 'SELECT ADVANCED',
        fields: parseFields(fullCommandAsStringArray.slice(1, indexOfFrom)),
        from: fullCommandAsStringArray[indexOfFrom],
        tableName: fullCommandAsStringArray[indexOfFrom + 1],
        finalSemicolon:
            fullCommandAsStringArray[fullCommandAsStringArray.length - 1],
    }

    return parsedCommand
}

const parseSelectAdvancedCommand = (fullCommandAsStringArray) => {
    const parsedBaseCommand = parseBaseCommand(fullCommandAsStringArray)

    const validatedParsedCommand = SelectAdvancedSchema.validate(
        parsedBaseCommand
    )

    console.log(validatedParsedCommand)

    return validatedParsedCommand
}

const parseWhere = (fullCommandAsStringArray) => {
    const indexOfWhere = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'WHERE'
    )

    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)

    parsedCommand.where = parseWhereAdvanced(
        fullCommandAsStringArray.slice(indexOfWhere)
    )

    const validatedCommand = SelectAdvancedWhereSchema.validate(parsedCommand)

    return validatedCommand
}

const parseSelectOrderBy = (fullCommandAsStringArray) => {
    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'ORDER'
    )

    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)
    parsedCommand.orderBy = parseOrderBy(
        fullCommandAsStringArray.slice(
            indexOfOrder,
            fullCommandAsStringArray.length - 1
        )
    )

    const validationResult = SelectAdvancedOrderBySchema.validate(parsedCommand)

    return validationResult
}

const parseWhereOrderBy = (fullCommandAsStringArray) => {
    const indexOfWhere = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'WHERE'
    )

    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'ORDER'
    )

    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)

    parsedCommand.where = parseWhereAdvanced(
        fullCommandAsStringArray.slice(indexOfWhere)
    )

    parsedCommand.orderBy = parseOrderBy(
        fullCommandAsStringArray.slice(
            indexOfOrder,
            fullCommandAsStringArray.length - 1
        )
    )

    const validationResult = SelectAdvancedWhereOrderBySchema.validate(
        parsedCommand
    )

    return validationResult
}

const parseFields = (fieldArray) => {
    console.log(fieldArray)

    const fieldObjects = fieldArray
        .join('')
        .split(',')
        .filter(Boolean)
        .map((f) => {
            return parseQueryField(f)
        })

    console.log(
        util.inspect(fieldObjects, false, null, true /* enable colors */)
    )

    return fieldObjects
}

const parseQueryField = (parsedField) => {
    switch (true) {
        case stringFunctionPattern.test(parsedField):
            return {
                type: 'stringFunction',
                name: parsedField.split('(')[0].toUpperCase(),
                value: parsedField,
                column: parseColumnFromStringFunction(parsedField),
            }
        case aggregateFunctionPattern.test(parsedField):
            return {
                type: 'aggregateFunction',
                name: parsedField.split('(')[0].toUpperCase(),
                value: parsedField,
                column: parseColumnFromAggregateFunction(parsedField),
            }
        case /^\*$/.test(parsedField):
            return {
                type: 'all',
                value: parsedField,
            }
        case arithmeticExpressionPattern.test(parsedField):
            return {
                type: 'expression',
                value: parseExpression(parsedField),
                stringValue: parsedField,
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
        case /^[+\-*%]$/.test(parsedField):
            return parsedField
        default:
            return {
                type: 'column',
                value: parsedField,
            }
    }
}

const parseExpression = (expression) => {
    console.log(expression)

    if (containsAggregateFunctionPattern.test(expression)) {
        console.log(expression.split(aggregateFunctionPattern))
    }
    const splitExpression = expression.split(arithmeticOperatorPattern)

    return splitExpression.map((e) => parseExpressionFields(e))
}

const parseExpressionFields = (expressionElement) => {
    switch (true) {
        case stringFunctionPattern.test(expressionElement):
            return {
                type: 'stringFunction',
                name: expressionElement.split('(')[0].toUpperCase(),
                value: expressionElement,
                column: parseColumnFromStringFunction(expressionElement),
            }
        case aggregateFunctionPattern.test(expressionElement):
            return {
                type: 'aggregateFunction',
                name: expressionElement.split('(')[0].toUpperCase(),
                value: expressionElement,
                column: parseColumnFromAggregateFunction(expressionElement),
            }
        case /^'\w+'/.test(expressionElement):
            return {
                type: 'string',
                value: expressionElement.replace(/'/g, ''),
            }
        case !isNaN(expressionElement):
            return {
                type: 'integer',
                value: Number(expressionElement),
            }
        case /^[+\-*%]$/.test(expressionElement):
            return {
                type: 'operator',
                value: expressionElement,
            }
        default:
            return {
                type: 'column',
                value: expressionElement,
            }
    }
}

module.exports = { parseCommand }
