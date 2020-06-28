const Joi = require('@hapi/joi')
const { CreateTableSchema } = require('../schemas/CreateTableSchema')
const { containsConstraintsPattern } = require('../helpers/regex')

/**
 * Parses and validates a CREATE TABLE command object from the given string array.
 * Returns the validated command object or throws a validation error if the object
 * fails validation.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {object} command object
 */
const parseCommand = (fullCommandAsStringArray) => {
    const parsedCommand = {
        name: fullCommandAsStringArray.slice(0, 2).join(' '),
        tableName: fullCommandAsStringArray[2],
        openingBracket: fullCommandAsStringArray[3],
        columns: parseColumns(
            fullCommandAsStringArray.slice(
                fullCommandAsStringArray.indexOf('(') + 1,
                fullCommandAsStringArray.indexOf(')')
            )
        ),
        closingBracket: fullCommandAsStringArray.join(' ').indexOf(')') > 0,
        finalSemicolon:
            fullCommandAsStringArray[fullCommandAsStringArray.length - 1] ===
            ';'
                ? ';'
                : undefined,
    }

    return Joi.attempt(parsedCommand, CreateTableSchema)
}

/**
 * Handles parsing of the column information contained in the given array.
 * @param {string[]} columnsAsStringList array containing the column information
 */
const parseColumns = (columnsAsStringList) => {
    if (!columnsAsStringList) return null

    const separatedColumnsAsStringList = columnsAsStringList
        .join(' ')
        .split(', ')

    const columns = separatedColumnsAsStringList
        .map((c) => c.split(' '))
        .map((item) => {
            return {
                name: item[0],
                type: item[1] ? item[1].toUpperCase() : null,
                constraints: parseColumnConstraints(item.slice(2)),
            }
        })

    return columns
}

/**
 * Handles parsing of the column constraints contained in the given array.
 * @param {string[]} constraintsAsStringArray array containing the column constraints
 */
const parseColumnConstraints = (constraintsAsStringArray) => {
    const separatedConstraintsAsStringList = constraintsAsStringArray
        .join(' ')
        .toUpperCase()
        .split(containsConstraintsPattern)
        .map((c) => c.trim())
        .filter(Boolean)

    return separatedConstraintsAsStringList
}

module.exports = { parseCommand }
