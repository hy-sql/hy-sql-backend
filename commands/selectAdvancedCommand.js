const _ = require('lodash')
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
    containsFunctionPattern,
} = require('../utils/regex')
const {
    parseParameterFromStringFunction,
    parseParameterFromAggregateFunction,
} = require('../utils/parseParameterFromFunction')

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
    const fieldObjects = fieldArray
        .join('')
        .split(',')
        .filter(Boolean)
        .map((f) => {
            return parseQueryField(f)
        })

    return fieldObjects
}

const parseQueryField = (parsedField) => {
    switch (true) {
        case stringFunctionPattern.test(parsedField):
            return {
                type: 'stringFunction',
                name: parsedField.split('(')[0].toUpperCase(),
                value: parsedField,
                param: parseParameterFromStringFunction(parsedField),
            }
        case aggregateFunctionPattern.test(parsedField):
            return {
                type: 'aggregateFunction',
                name: parsedField.split('(')[0].toUpperCase(),
                value: parsedField,
                param: parseParameterFromAggregateFunction(parsedField),
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

    return splitExpression.map((e) => parseExpressionFields(e))
}

const parseExpressionFields = (expressionElement) => {
    switch (true) {
        case stringFunctionPattern.test(expressionElement):
            return {
                type: 'stringFunction',
                name: expressionElement.split('(')[0].toUpperCase(),
                value: expressionElement,
                param: parseParameterFromStringFunction(expressionElement),
            }
        case aggregateFunctionPattern.test(expressionElement):
            return {
                type: 'aggregateFunction',
                name: expressionElement.split('(')[0].toUpperCase(),
                value: expressionElement,
                param: parseParameterFromAggregateFunction(expressionElement),
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
