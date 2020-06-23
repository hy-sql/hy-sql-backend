const Joi = require('@hapi/joi')
const { CreateTableSchema } = require('../schemas/CreateTableSchema')
const { constraintsNamePatternForSplit } = require('../helpers/regex')

/**
 * Parses and validates a CREATE TABLE command object from the given string array.
 * Returns a Joi validation result object containing the parsed command object
 * with key value and possible validation errors as object with key error.
 * @param {string[]} fullCommandAsStringArray command as string array
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
 * TODO: Validate only one column to have unique constraints such as PRIMARY KEY
 * @param {string[]} constraintsAsStringArray array containing the column constraints
 */
const parseColumnConstraints = (constraintsAsStringArray) => {
    const separatedConstraintsAsStringList = constraintsAsStringArray
        .join(' ')
        .toUpperCase()
        .split(constraintsNamePatternForSplit)
        .map((c) => c.trim())
        .filter(Boolean)

    return separatedConstraintsAsStringList
}

module.exports = { parseCommand }
