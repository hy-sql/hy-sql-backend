const { SelectAdvancedSchema } = require('../models/SelectAdvancedSchema')
const {
    arithmeticExpressionPattern,
    stringFunctionExpressionPattern,
    aggregateFunctionExpressionPattern,
} = require('../utils/regex')

const parseCommand = (fullCommandAsStringArray) => {
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

const parseFields = (fieldArray) => {
    const fieldObjects = fieldArray
        .join('')
        .split(',')
        .filter(Boolean)
        .map((f) => createFieldObject(f))

    return fieldObjects
}

const createFieldObject = (parsedField) => {
    switch (true) {
        case arithmeticExpressionPattern.test(parsedField):
            return {
                type: 'expression',
                value: parsedField,
            }
        case stringFunctionExpressionPattern.test(parsedField):
            return {
                type: 'stringFunction',
                name: parsedField.split('(')[0].toUpperCase(),
                value: parsedField,
            }
        case aggregateFunctionExpressionPattern.test(parsedField):
            return {
                type: 'aggregateFunction',
                name: parsedField.split('(')[0].toUpperCase(),
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
