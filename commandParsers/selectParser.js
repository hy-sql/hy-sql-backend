const {
    SelectAdvancedSchema,
    SelectAdvancedWhereSchema,
    SelectAdvancedOrderBySchema,
    SelectAdvancedWhereOrderBySchema,
} = require('../schemas/SelectAdvancedSchema')
const { parseWhere } = require('./whereParser')
const {
    queryContainsWhereKeyword,
    queryContainsOrderByKeywords,
    queryContainsWhereOrderByKeywords,
} = require('./parserTools/queryContains')
const { parseOrderBy } = require('./orderByParser')
const { parseSelectFields } = require('./fieldParser')

/** Parses and validates a SELECT command object from the given string array.
 * Returns a Joi validation result object containing the parsed command object
 * with key value and possible validation errors as object with key error.
 * @param {string[]} fullCommandAsStringArray command as string array
 */
const parseCommand = (fullCommandAsStringArray) => {
    switch (true) {
        case queryContainsWhereOrderByKeywords(fullCommandAsStringArray):
            return parseSelectWhereOrderBy(fullCommandAsStringArray)
        case queryContainsOrderByKeywords(fullCommandAsStringArray):
            return parseSelectOrderBy(fullCommandAsStringArray)
        case queryContainsWhereKeyword(fullCommandAsStringArray):
            return parseSelectWhere(fullCommandAsStringArray)
        default:
            return parseSelect(fullCommandAsStringArray)
    }
}

/** Handles parsing of the base SELECT command from the given array.
 * @param {string[]} fullCommandAsStringArray command as string array
 */
const parseBaseCommand = (fullCommandAsStringArray) => {
    const indexOfFrom = fullCommandAsStringArray.findIndex(
        (c) => c.toUpperCase() === 'FROM'
    )

    const parsedCommand = {
        name: 'SELECT',
        fields: parseSelectFields(
            fullCommandAsStringArray.slice(1, indexOfFrom)
        ),
        from: fullCommandAsStringArray[indexOfFrom],
        tableName: fullCommandAsStringArray[indexOfFrom + 1],
        finalSemicolon:
            fullCommandAsStringArray[fullCommandAsStringArray.length - 1],
    }

    return parsedCommand
}

/** Parses and validates a SELECT command not containing WHERE or ORDER BY
 * from the given array. Returns a Joi validation result object.
 * @param {string[]} fullCommandAsStringArray command as string array
 */
const parseSelect = (fullCommandAsStringArray) => {
    const parsedBaseCommand = parseBaseCommand(fullCommandAsStringArray)

    const validatedParsedCommand = SelectAdvancedSchema.validate(
        parsedBaseCommand
    )

    return validatedParsedCommand
}

/** Parses and validates a SELECT command containing WHERE but not ORDER BY
 * from the given array. Returns a Joi validation result object.
 * @param {string[]} fullCommandAsStringArray command as string array
 */
const parseSelectWhere = (fullCommandAsStringArray) => {
    const indexOfWhere = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'WHERE'
    )

    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)

    parsedCommand.where = parseWhere(
        fullCommandAsStringArray.slice(indexOfWhere)
    )

    const validatedCommand = SelectAdvancedWhereSchema.validate(parsedCommand)

    return validatedCommand
}

/** Parses and validates a SELECT command containing ORDER BY but not WHERE
 * from the given array. Returns a Joi validation result object.
 * @param {string[]} fullCommandAsStringArray command as string array
 */
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

/** Parses and validates a SELECT command containing both WHERE and ORDER BY
 * from the given array. Returns a Joi validation result object.
 * @param {string[]} fullCommandAsStringArray command as string array
 */
const parseSelectWhereOrderBy = (fullCommandAsStringArray) => {
    const indexOfWhere = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'WHERE'
    )

    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'ORDER'
    )

    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)

    parsedCommand.where = parseWhere(
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

module.exports = { parseCommand }
