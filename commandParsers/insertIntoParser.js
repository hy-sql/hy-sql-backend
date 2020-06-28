const _ = require('lodash')
const Joi = require('@hapi/joi')
const { InsertIntoSchema } = require('../schemas/InsertIntoSchema')
const { parseInputFields } = require('./fieldsParser')
const findIndexOfClosingBracket = require('./parserTools/findIndexOfClosingBracket')

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

    const indexOfOpeningColumnsBracket = fullCommandAsStringArray.findIndex(
        (e) => e === '('
    )

    const indexOfClosingColumnsBracket = findIndexOfClosingBracket(
        fullCommandAsStringArray,
        indexOfOpeningColumnsBracket
    )

    const indexOfOpeningValuesBracket = _.findIndex(
        fullCommandAsStringArray,
        (e) => e === '(',
        indexOfValuesKeyword > 0
            ? indexOfValuesKeyword
            : indexOfOpeningColumnsBracket + 1
    )

    const indexOfClosingValuesBracket = findIndexOfClosingBracket(
        fullCommandAsStringArray,
        indexOfOpeningValuesBracket
    )

    const parsedCommand = {
        name: fullCommandAsStringArray.slice(0, 2).join(' '),
        tableName: fullCommandAsStringArray[2],
        openingColumnsBracket: fullCommandAsStringArray[3],
        columns:
            indexOfOpeningColumnsBracket > 0
                ? parseInputFields(
                      fullCommandAsStringArray.slice(
                          indexOfOpeningColumnsBracket + 1,
                          indexOfClosingColumnsBracket
                              ? indexOfClosingColumnsBracket
                              : indexOfValuesKeyword
                      )
                  )
                : null,
        closingColumnsBracket:
            fullCommandAsStringArray[indexOfClosingColumnsBracket],
        valuesKeyword: fullCommandAsStringArray[indexOfValuesKeyword],
        openingValuesBracket:
            indexOfOpeningValuesBracket > indexOfOpeningColumnsBracket
                ? fullCommandAsStringArray[indexOfOpeningValuesBracket]
                : null,
        values:
            indexOfOpeningValuesBracket > 0
                ? parseInputFields(
                      fullCommandAsStringArray.slice(
                          indexOfOpeningValuesBracket + 1,
                          indexOfClosingValuesBracket ||
                              fullCommandAsStringArray.length - 1
                      )
                  )
                : null,
        closingValuesBracket:
            fullCommandAsStringArray[indexOfClosingValuesBracket],
        finalSemicolon:
            fullCommandAsStringArray[fullCommandAsStringArray.length - 1],
    }

    const validatedCommand = Joi.attempt(parsedCommand, InsertIntoSchema)

    return validatedCommand
}

module.exports = { parseCommand }
