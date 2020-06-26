const Joi = require('@hapi/joi')
const { DeleteSchema, DeleteWhereSchema } = require('../schemas/DeleteSchema')
const { parseWhere } = require('./whereParser')
const { queryContainsWhereKeyword } = require('./parserTools/queryContains')
const { checkForAdditionalAtEnd } = require('./parserTools/checkForAdditional')
const SQLError = require('../models/SQLError')

/**
 * Handles passing the given command as string array to the correct function
 * to be parsed into and validated as a DELETE command object. Returns the
 * validated command object if no error was thrown.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {object} command object
 */
const parseCommand = (fullCommandAsStringArray) => {
    if (queryContainsWhereKeyword(fullCommandAsStringArray)) {
        return parseDeleteWhere(fullCommandAsStringArray)
    }

    return parseDelete(fullCommandAsStringArray)
}

/**
 * Handles parsing of the base DELETE command from the given array.
 * @param {string[]} fullCommandAsStringArray command as string array
 */
const parseBaseCommand = (fullCommandAsStringArray) => {
    const parsedBaseCommand = {
        name: fullCommandAsStringArray[0],
        from: fullCommandAsStringArray[1],
        tableName: fullCommandAsStringArray[2],
        finalSemicolon:
            fullCommandAsStringArray[fullCommandAsStringArray.length - 1] ===
            ';'
                ? ';'
                : undefined,
    }

    return parsedBaseCommand
}

/**
 * Parses and validates a DELETE command object from the given string array.
 * Returns the validated command object or throws a validation error if the
 * object fails validation or contains something additional at the end.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {object} command object
 */
const parseDelete = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)

    const validatedCommand = Joi.attempt(parsedCommand, DeleteSchema)

    checkForAdditionalAtEnd(fullCommandAsStringArray, 4)

    return validatedCommand
}

/**
 * Parses and validates a DELETE command containing WHERE from the given array.
 * Returns the validated command object or throws a validation error if the
 * object fails validation.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {object} command object
 */
const parseDeleteWhere = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)

    const whereIndex = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'WHERE'
    )

    parsedCommand.where = parseWhere(
        fullCommandAsStringArray.slice(
            whereIndex,
            fullCommandAsStringArray.length - 1
        )
    )

    if (whereIndex !== 3) {
        throw new SQLError('WHERE should be directly after the table name')
    }

    const validatedCommand = Joi.attempt(parsedCommand, DeleteWhereSchema)

    return validatedCommand
}

module.exports = { parseCommand }
