const Joi = require('@hapi/joi')
const { DeleteSchema, DeleteWhereSchema } = require('../schemas/DeleteSchema')
const { parseWhere } = require('./whereParser')
const { queryContainsWhereKeyword } = require('./parserTools/queryContains')
const { checkForAdditionalAtEnd } = require('./parserTools/checkForAdditional')
const SQLError = require('../models/SQLError')

/**
 * Parses and validates a DELETE command object from the given string array.
 * Returns a Joi validation result object containing the parsed command object
 * with key value and possible validation errors as object with key error.
 * @param {string[]} fullCommandAsStringArray command as string array
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
    return {
        name: fullCommandAsStringArray[0],
        from: fullCommandAsStringArray[1],
        tableName: fullCommandAsStringArray[2],
        finalSemicolon:
            fullCommandAsStringArray[fullCommandAsStringArray.length - 1] ===
            ';'
                ? ';'
                : undefined,
    }
}

/**
 * Parses and validates a DELETE command not containing WHERE
 * from the given array. Returns a Joi validation result object.
 * @param {string[]} fullCommandAsStringArray command as string array
 */
const parseDelete = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)

    let validatedCommand = Joi.attempt(parsedCommand, DeleteSchema)

    checkForAdditionalAtEnd(fullCommandAsStringArray, 4)

    return validatedCommand
}

/**
 * Parses and validates a DELETE command containing WHERE from the given array.
 * Returns a Joi validation result object.
 * @param {string[]} fullCommandAsStringArray command as string array
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

    let validatedCommand = Joi.attempt(parsedCommand, DeleteWhereSchema)

    return validatedCommand
}

module.exports = { parseCommand }
