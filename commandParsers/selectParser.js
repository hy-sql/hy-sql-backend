const {
    SelectSchema,
    SelectWhereSchema,
    SelectOrderBySchema,
    SelectWhereOrderBySchema,
} = require('../schemas/SelectSchema')
const { parseWhere } = require('./whereParser')
const {
    queryContainsWhereKeyword,
    queryContainsOrderByKeywords,
    queryContainsWhereOrderByKeywords,
    queryContainsLimitKeyword,
} = require('./parserTools/queryContains')
const { parseOrderBy } = require('./orderByParser')
const { parseSelectFields } = require('./fieldParser')
const { parseLimit } = require('./limitParser')

/**
 * Parses and validates a SELECT command object from the given string array.
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

/**
 * Handles parsing of the base SELECT command from the given array.
 * @param {string[]} fullCommandAsStringArray command as string array
 */
const parseBaseCommand = (fullCommandAsStringArray) => {
    const indexOfFrom = fullCommandAsStringArray.findIndex(
        (c) => c.toUpperCase() === 'FROM'
    )

    const parsedCommand = {
        name: fullCommandAsStringArray[0],
        fields: parseSelectFields(
            fullCommandAsStringArray.slice(1, indexOfFrom)
        ),
        from: fullCommandAsStringArray[indexOfFrom],
        tableName: fullCommandAsStringArray[indexOfFrom + 1],
        finalSemicolon:
            fullCommandAsStringArray[fullCommandAsStringArray.length - 1],
    }

    const additional =
        fullCommandAsStringArray.length - 1 - (indexOfFrom + 1) > 0
            ? fullCommandAsStringArray.slice(
                  indexOfFrom + 2,
                  fullCommandAsStringArray.length - 1
              )
            : null

    if (additional) {
        parsedCommand.additional = additional
    }

    // TODO: Check here or elsewhere in parser that limit is after order by and where.
    if (queryContainsLimitKeyword(fullCommandAsStringArray)) {
        const indexOfLimit = parsedCommand.additional.findIndex(
            (s) => s.toUpperCase() === 'LIMIT'
        )

        parsedCommand.limit = parseLimit(
            parsedCommand.additional.splice(indexOfLimit)
        )
    }

    return parsedCommand
}

/**
 * Parses and validates a SELECT command not containing WHERE or ORDER BY
 * from the given array. Returns a Joi validation result object.
 * @param {string[]} fullCommandAsStringArray command as string array
 */
const parseSelect = (fullCommandAsStringArray) => {
    const parsedBaseCommand = parseBaseCommand(fullCommandAsStringArray)

    const validationResult = SelectSchema.validate(parsedBaseCommand)

    return validationResult
}

/**
 * Parses and validates a SELECT command containing WHERE but not ORDER BY
 * from the given array. Returns a Joi validation result object.
 * @param {string[]} fullCommandAsStringArray command as string array
 */
const parseSelectWhere = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)

    const indexOfWhere = parsedCommand.additional.findIndex(
        (k) => k.toUpperCase() === 'WHERE'
    )

    parsedCommand.where = parseWhere(
        parsedCommand.additional.splice(indexOfWhere)
    )

    const validationResult = SelectWhereSchema.validate(parsedCommand)

    return validationResult
}

/**
 * Parses and validates a SELECT command containing ORDER BY but not WHERE
 * from the given array. Returns a Joi validation result object.
 * @param {string[]} fullCommandAsStringArray command as string array
 */
const parseSelectOrderBy = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)

    const indexOfOrder = parsedCommand.additional.findIndex(
        (k) => k.toUpperCase() === 'ORDER'
    )

    parsedCommand.orderBy = parseOrderBy(
        parsedCommand.additional.splice(indexOfOrder)
    )

    const validationResult = SelectOrderBySchema.validate(parsedCommand)

    return validationResult
}

/**
 * Parses and validates a SELECT command containing both WHERE and ORDER BY
 * from the given array. Returns a Joi validation result object.
 * @param {string[]} fullCommandAsStringArray command as string array
 */
const parseSelectWhereOrderBy = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)

    const indexOfWhere = parsedCommand.additional.findIndex(
        (k) => k.toUpperCase() === 'WHERE'
    )

    const indexOfOrder = parsedCommand.additional.findIndex(
        (k) => k.toUpperCase() === 'ORDER'
    )

    parsedCommand.where = parseWhere(
        parsedCommand.additional.splice(indexOfWhere, indexOfOrder)
    )

    parsedCommand.orderBy = parseOrderBy(parsedCommand.additional.splice(0))

    const validationResult = SelectWhereOrderBySchema.validate(parsedCommand)

    return validationResult
}

module.exports = { parseCommand }
