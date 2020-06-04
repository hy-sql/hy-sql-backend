const {
    SelectAdvancedSchema,
    SelectAdvancedWhereSchema,
} = require('../models/SelectAdvancedSchema')
const { parseWhereAdvanced } = require('./whereAdvancedCommand')
const { queryContainsWhereKeyword } = require('./whereCommand')
const {
    arithmeticExpressionPattern,
    stringFunctionExpressionPattern,
    aggregateFunctionExpressionPattern,
} = require('../utils/regex')
const {
    parseColumnsFromExpression,
} = require('../utils/parseColumnsFromExpression')
const {
    parseColumnFromStringFunction,
    parseColumnFromAggregateFunction,
} = require('../utils/parseColumnFromFunction')

const parseCommand = (fullCommandAsStringArray) => {
    if (queryContainsWhereKeyword(fullCommandAsStringArray)) {
        return parseWhereAdvancedCommand(fullCommandAsStringArray)
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

const parseWhereAdvancedCommand = (fullCommandAsStringArray) => {
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

const parseFields = (fieldArray) => {
    const fieldObjects = fieldArray
        .join('')
        .split(',')
        .filter(Boolean)
        .map((f) => parseQueryField(f))

    return fieldObjects
}

const parseQueryField = (parsedField) => {
    switch (true) {
        case arithmeticExpressionPattern.test(parsedField):
            return {
                type: 'expression',
                value: parsedField,
                columns: parseColumnsFromExpression(parsedField),
            }
        case stringFunctionExpressionPattern.test(parsedField):
            return {
                type: 'stringFunction',
                name: parsedField.split('(')[0].toUpperCase(),
                value: parsedField,
                column: parseColumnFromStringFunction(parsedField),
            }
        case aggregateFunctionExpressionPattern.test(parsedField):
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
        default:
            return {
                type: 'column',
                value: parsedField,
            }
    }
}

module.exports = { parseCommand }
