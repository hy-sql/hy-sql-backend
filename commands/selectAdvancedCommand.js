const { SelectAdvancedSchema } = require('../models/SelectAdvancedSchema')
const arithmeticPattern = /^\w+(( )?[+-/*%]{1}( )?\w+)+$/
const functionPattern = /^LENGTH\(\w+\)$/

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
        case arithmeticPattern.test(parsedField):
            return {
                type: 'expression',
                value: parsedField,
            }
        case functionPattern.test(parsedField):
            return {
                type: 'function',
                name: 'LENGTH',
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
