const Joi = require('@hapi/joi')
const { InsertIntoSchema } = require('../schemas/InsertIntoSchema')
const { parseInputFields } = require('./fieldsParser')
const findIndexOfClosingBracket = require('./parserTools/findIndexOfClosingBracket')
const SQLError = require('../models/SQLError')

/**
 * Parses and validates an INSERT INTO command object from the given array.
 * Returns the validated command object or throws a validation error if the
 * object fails validation.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {object} command object
 */
const parseCommand = (fullCommandAsStringArray) => {
    const indexOfValuesKeyword = fullCommandAsStringArray.findIndex(
        (e) => e.toUpperCase() === 'VALUES'
    )

    if (indexOfValuesKeyword < 0) {
        throw new SQLError(
            'INSERT INTO needs a VALUES keyword before the actual values to be inserted'
        )
    }

    const parsedCommand = {
        name: fullCommandAsStringArray.slice(0, 2).join(' '),
        tableName: fullCommandAsStringArray[2],
        columns: parseColumns(
            fullCommandAsStringArray.slice(0, indexOfValuesKeyword)
        ),
        valuesKeyword: fullCommandAsStringArray[indexOfValuesKeyword],
        values: parseValues(
            fullCommandAsStringArray.slice(indexOfValuesKeyword + 1)
        ),
        finalSemicolon:
            fullCommandAsStringArray[fullCommandAsStringArray.length - 1],
    }

    const validatedCommand = Joi.attempt(parsedCommand, InsertIntoSchema)

    return validatedCommand
}

const parseColumns = (fullCommandAsStringArray) => {
    const indexOfOpeningBracket = fullCommandAsStringArray.findIndex(
        (e) => e === '('
    )

    const indexOfClosingBracket = findIndexOfClosingBracket(
        fullCommandAsStringArray,
        indexOfOpeningBracket
    )

    if (indexOfOpeningBracket < 0 || indexOfOpeningBracket < 0) {
        throw new SQLError('columns must be surrounded by brackets')
    }

    if (indexOfOpeningBracket < 3) {
        throw new SQLError('Table name missing')
    }

    return parseInputFields(
        fullCommandAsStringArray.slice(
            indexOfOpeningBracket + 1,
            indexOfClosingBracket
        )
    )
}

const parseValues = (slicedStringArray) => {
    if (slicedStringArray.length < 2) {
        throw new SQLError('Values missing')
    }

    const indexOfOpeningBracket = slicedStringArray.findIndex((e) => e === '(')

    const indexOfClosingBracket = findIndexOfClosingBracket(
        slicedStringArray,
        indexOfOpeningBracket
    )

    if (indexOfOpeningBracket < 0 || indexOfOpeningBracket < 0) {
        throw new SQLError('values must be surrounded by brackets')
    }

    return parseInputFields(
        slicedStringArray.slice(
            indexOfOpeningBracket + 1,
            indexOfClosingBracket
        )
    )
}

module.exports = { parseCommand }
