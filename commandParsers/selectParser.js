const Joi = require('@hapi/joi')
const {
    SelectSchema,
    SelectWhereSchema,
    SelectGroupBySchema,
    SelectGroupByHavingSchema,
    SelectOrderBySchema,
    SelectWhereGroupBySchema,
    SelectWhereGroupByHavingSchema,
    SelectWhereOrderBySchema,
    SelectGroupByOrderBySchema,
    SelectGroupByHavingOrderBySchema,
    SelectWhereGroupByOrderBySchema,
    SelectWhereGroupByHavingOrderBySchema,
} = require('../schemas/SelectSchema')
const { parseWhere } = require('./whereParser')
const {
    queryContainsWhereKeyword,
    queryContainsGroupByKeyword,
    queryContainsGroupByHavingKeywords,
    queryContainsOrderByKeyword,
    queryContainsWhereGroupByKeywords,
    queryContainsWhereGroupByHavingKeywords,
    queryContainsWhereOrderByKeywords,
    queryContainsGroupByOrderByKeywords,
    queryContainsGroupByHavingOrderByKeywords,
    queryContainsWhereGroupByOrderByKeywords,
    queryContainsWhereGroupByHavingOrderByKeywords,
    queryContainsLimitKeyword,
} = require('./parserTools/queryContains')
const { parseOrderBy } = require('./orderByParser')
const { parseSelectFields } = require('./fieldsParser')
const { parseGroupBy } = require('./groupByParser')
const { parseHaving } = require('./havingParser')
const { parseLimit } = require('./limitParser')
const {
    checkForAdditionalAtEndOfBaseSelect,
} = require('./parserTools/checkForAdditional')
const SQLError = require('../models/SQLError')

/**
 * Handles passing the given command as string array to the correct function
 * to be parsed into and validated as a SELECT command object. Returns the
 * validated command object if no error was thrown.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {object} command object
 */
const parseCommand = (fullCommandAsStringArray) => {
    switch (true) {
        case queryContainsWhereGroupByHavingOrderByKeywords(
            fullCommandAsStringArray
        ):
            return parseSelectWhereGroupByHavingOrderBy(
                fullCommandAsStringArray
            )
        case queryContainsWhereGroupByOrderByKeywords(fullCommandAsStringArray):
            return parseSelectWhereGroupByOrderBy(fullCommandAsStringArray)
        case queryContainsWhereOrderByKeywords(fullCommandAsStringArray):
            return parseSelectWhereOrderBy(fullCommandAsStringArray)
        case queryContainsGroupByHavingOrderByKeywords(
            fullCommandAsStringArray
        ):
            return parseSelectGroupByHavingOrderBy(fullCommandAsStringArray)
        case queryContainsGroupByOrderByKeywords(fullCommandAsStringArray):
            return parseSelectGroupByOrderBy(fullCommandAsStringArray)
        case queryContainsWhereGroupByHavingKeywords(fullCommandAsStringArray):
            return parseSelectWhereGroupByHaving(fullCommandAsStringArray)
        case queryContainsWhereGroupByKeywords(fullCommandAsStringArray):
            return parseSelectWhereGroupBy(fullCommandAsStringArray)
        case queryContainsOrderByKeyword(fullCommandAsStringArray):
            return parseSelectOrderBy(fullCommandAsStringArray)
        case queryContainsGroupByHavingKeywords(fullCommandAsStringArray):
            return parseSelectGroupByHaving(fullCommandAsStringArray)
        case queryContainsGroupByKeyword(fullCommandAsStringArray):
            return parseSelectGroupBy(fullCommandAsStringArray)
        case queryContainsWhereKeyword(fullCommandAsStringArray):
            return parseSelectWhere(fullCommandAsStringArray)
        default:
            return parseSelect(fullCommandAsStringArray)
    }
}

/**
 * Handles parsing of the base SELECT command from the given array. Also
 * handles calling a parser for the LIMIT part of a command and throws an
 * error if the command contains keyword OFFSET without containing LIMIT.
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

    if (queryContainsLimitKeyword(fullCommandAsStringArray)) {
        parsedCommand.indexOfLimit = fullCommandAsStringArray.findIndex(
            (s) => s.toUpperCase() === 'LIMIT'
        )
        parsedCommand.limit = parseLimit(
            fullCommandAsStringArray.slice(
                parsedCommand.indexOfLimit,
                fullCommandAsStringArray.length - 1
            )
        )
    } else if (
        fullCommandAsStringArray.some((s) => s.toUpperCase() === 'OFFSET')
    ) {
        throw new SQLError(
            'Query contains OFFSET keyword without containing LIMIT keyword.'
        )
    }

    return parsedCommand
}

/**
 * Parses and validates a SELECT command object from the given string array.
 * Returns the validated command object or throws a validation error if the
 * object fails validation.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {object} command object
 */
const parseSelect = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)

    parsedCommand.unrecognized = checkForAdditionalAtEndOfBaseSelect(
        fullCommandAsStringArray,
        parsedCommand,
        parsedCommand.indexOfLimit
    )

    delete parsedCommand.indexOfLimit

    const validatedCommand = Joi.attempt(parsedCommand, SelectSchema)

    return validatedCommand
}

/**
 * Parses and validates a SELECT command containing WHERE from the given array.
 * Returns the validated command object or throws a validation error if the
 * object fails validation.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {object} command object
 */
const parseSelectWhere = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)

    const indexOfWhere = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'WHERE'
    )

    parsedCommand.where = parseWhere(
        fullCommandAsStringArray.slice(
            indexOfWhere,
            parsedCommand.limit
                ? parsedCommand.indexOfLimit
                : fullCommandAsStringArray.length - 1
        )
    )

    parsedCommand.unrecognized = checkForAdditionalAtEndOfBaseSelect(
        fullCommandAsStringArray,
        parsedCommand,
        indexOfWhere
    )

    delete parsedCommand.indexOfLimit

    const validatedCommand = Joi.attempt(parsedCommand, SelectWhereSchema)

    return validatedCommand
}

/**
 * Parses and validates a SELECT command containing GROUP BY from the given
 * array. Returns the validated command object or throws a validation error
 * if the object fails validation.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {object} command object
 */
const parseSelectGroupBy = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)

    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'GROUP'
    )

    parsedCommand.groupBy = parseGroupBy(
        fullCommandAsStringArray.slice(
            indexOfGroup,
            parsedCommand.limit
                ? parsedCommand.indexOfLimit
                : fullCommandAsStringArray.length - 1
        )
    )

    parsedCommand.unrecognized = checkForAdditionalAtEndOfBaseSelect(
        fullCommandAsStringArray,
        parsedCommand,
        indexOfGroup
    )

    delete parsedCommand.indexOfLimit

    const validatedCommand = Joi.attempt(parsedCommand, SelectGroupBySchema)

    return validatedCommand
}

/**
 * Parses and validates a SELECT command containing GROUP BY and HAVING from
 * the given array. Returns the validated command object or throws a validation
 * error if the object fails validation.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {object} command object
 */
const parseSelectGroupByHaving = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)

    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'GROUP'
    )

    const indexOfHaving = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'HAVING'
    )

    parsedCommand.groupBy = parseGroupBy(
        fullCommandAsStringArray.slice(indexOfGroup, indexOfHaving)
    )

    parsedCommand.having = parseHaving(
        fullCommandAsStringArray.slice(
            indexOfHaving,
            parsedCommand.limit
                ? parsedCommand.indexOfLimit
                : fullCommandAsStringArray.length - 1
        )
    )

    parsedCommand.unrecognized = checkForAdditionalAtEndOfBaseSelect(
        fullCommandAsStringArray,
        parsedCommand,
        indexOfGroup
    )

    delete parsedCommand.indexOfLimit

    const validatedCommand = Joi.attempt(
        parsedCommand,
        SelectGroupByHavingSchema
    )

    return validatedCommand
}

/**
 * Parses and validates a SELECT command containing ORDER BY from the given
 * array. Returns the validated command object or throws a validation error
 * if the object fails validation.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {object} command object
 */
const parseSelectOrderBy = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)

    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'ORDER'
    )

    parsedCommand.orderBy = parseOrderBy(
        fullCommandAsStringArray.slice(
            indexOfOrder,
            parsedCommand.limit
                ? parsedCommand.indexOfLimit
                : fullCommandAsStringArray.length - 1
        )
    )

    parsedCommand.unrecognized = checkForAdditionalAtEndOfBaseSelect(
        fullCommandAsStringArray,
        parsedCommand,
        indexOfOrder
    )

    delete parsedCommand.indexOfLimit

    const validatedCommand = Joi.attempt(parsedCommand, SelectOrderBySchema)

    return validatedCommand
}

/**
 * Parses and validates a SELECT command containing WHERE and GROUP BY from
 * the given array. Returns the validated command object or throws a validation
 * error if the object fails validation.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {object} command object
 */
const parseSelectWhereGroupBy = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)

    const indexOfWhere = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'WHERE'
    )

    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'GROUP'
    )

    parsedCommand.where = parseWhere(
        fullCommandAsStringArray.slice(indexOfWhere, indexOfGroup)
    )

    parsedCommand.groupBy = parseGroupBy(
        fullCommandAsStringArray.slice(
            indexOfGroup,
            parsedCommand.limit
                ? parsedCommand.indexOfLimit
                : fullCommandAsStringArray.length - 1
        )
    )

    parsedCommand.unrecognized = checkForAdditionalAtEndOfBaseSelect(
        fullCommandAsStringArray,
        parsedCommand,
        indexOfWhere
    )

    delete parsedCommand.indexOfLimit

    const validatedCommand = Joi.attempt(
        parsedCommand,
        SelectWhereGroupBySchema
    )

    return validatedCommand
}

/**
 * Parses and validates a SELECT command containing WHERE, GROUP BY and HAVING
 * from the given array. Returns the validated command object or throws a
 * validation error if the object fails validation.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {object} command object
 */
const parseSelectWhereGroupByHaving = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)

    const indexOfWhere = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'WHERE'
    )

    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'GROUP'
    )

    const indexOfHaving = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'HAVING'
    )

    parsedCommand.where = parseWhere(
        fullCommandAsStringArray.slice(indexOfWhere, indexOfGroup)
    )

    parsedCommand.groupBy = parseGroupBy(
        fullCommandAsStringArray.slice(indexOfGroup, indexOfHaving)
    )

    parsedCommand.having = parseHaving(
        fullCommandAsStringArray.slice(
            indexOfHaving,
            parsedCommand.limit
                ? parsedCommand.indexOfLimit
                : fullCommandAsStringArray.length - 1
        )
    )

    parsedCommand.unrecognized = checkForAdditionalAtEndOfBaseSelect(
        fullCommandAsStringArray,
        parsedCommand,
        indexOfWhere
    )

    delete parsedCommand.indexOfLimit

    const validatedCommand = Joi.attempt(
        parsedCommand,
        SelectWhereGroupByHavingSchema
    )

    return validatedCommand
}

/**
 * Parses and validates a SELECT command containing WHERE and ORDER BY from
 * the given array. Returns the validated command object or throws a validation
 * error if the object fails validation.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {object} command object
 */
const parseSelectWhereOrderBy = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)

    const indexOfWhere = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'WHERE'
    )

    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'ORDER'
    )

    parsedCommand.where = parseWhere(
        fullCommandAsStringArray.slice(indexOfWhere, indexOfOrder)
    )

    parsedCommand.orderBy = parseOrderBy(
        fullCommandAsStringArray.slice(
            indexOfOrder,
            parsedCommand.limit
                ? parsedCommand.indexOfLimit
                : fullCommandAsStringArray.length - 1
        )
    )

    parsedCommand.unrecognized = checkForAdditionalAtEndOfBaseSelect(
        fullCommandAsStringArray,
        parsedCommand,
        indexOfWhere
    )

    delete parsedCommand.indexOfLimit

    const validatedCommand = Joi.attempt(
        parsedCommand,
        SelectWhereOrderBySchema
    )

    return validatedCommand
}

/**
 * Parses and validates a SELECT command containing GROUP BY and ORDER BY
 * from the given array. Returns the validated command object or throws a
 * validation error if the object fails validation.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {object} command object
 */
const parseSelectGroupByOrderBy = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)

    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'GROUP'
    )

    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'ORDER'
    )

    parsedCommand.groupBy = parseGroupBy(
        fullCommandAsStringArray.slice(indexOfGroup, indexOfOrder)
    )

    parsedCommand.orderBy = parseOrderBy(
        fullCommandAsStringArray.slice(
            indexOfOrder,
            parsedCommand.limit
                ? parsedCommand.indexOfLimit
                : fullCommandAsStringArray.length - 1
        )
    )

    parsedCommand.unrecognized = checkForAdditionalAtEndOfBaseSelect(
        fullCommandAsStringArray,
        parsedCommand,
        indexOfGroup
    )

    delete parsedCommand.indexOfLimit

    const validatedCommand = Joi.attempt(
        parsedCommand,
        SelectGroupByOrderBySchema
    )

    return validatedCommand
}

/**
 * Parses and validates a SELECT command containing GROUP BY, HAVING and
 * ORDER BY from the given array. Returns the validated command object or
 * throws a validation error if the object fails validation.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {object} command object
 */
const parseSelectGroupByHavingOrderBy = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)

    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'GROUP'
    )

    const indexOfHaving = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'HAVING'
    )

    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'ORDER'
    )

    parsedCommand.groupBy = parseGroupBy(
        fullCommandAsStringArray.slice(indexOfGroup, indexOfHaving)
    )

    parsedCommand.having = parseHaving(
        fullCommandAsStringArray.slice(indexOfHaving, indexOfOrder)
    )

    parsedCommand.orderBy = parseOrderBy(
        fullCommandAsStringArray.slice(
            indexOfOrder,
            parsedCommand.limit
                ? parsedCommand.indexOfLimit
                : fullCommandAsStringArray.length - 1
        )
    )

    parsedCommand.unrecognized = checkForAdditionalAtEndOfBaseSelect(
        fullCommandAsStringArray,
        parsedCommand,
        indexOfGroup
    )

    delete parsedCommand.indexOfLimit

    const validatedCommand = Joi.attempt(
        parsedCommand,
        SelectGroupByHavingOrderBySchema
    )

    return validatedCommand
}

/**
 * Parses and validates a SELECT command containing WHERE, GROUP BY and
 * ORDER BY from the given array. Returns the validated command object
 * or throws a validation error if the object fails validation.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {object} command object
 */
const parseSelectWhereGroupByOrderBy = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)

    const indexOfWhere = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'WHERE'
    )

    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'GROUP'
    )

    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'ORDER'
    )

    parsedCommand.where = parseWhere(
        fullCommandAsStringArray.slice(indexOfWhere, indexOfGroup)
    )

    parsedCommand.groupBy = parseGroupBy(
        fullCommandAsStringArray.slice(indexOfGroup, indexOfOrder)
    )

    parsedCommand.orderBy = parseOrderBy(
        fullCommandAsStringArray.slice(
            indexOfOrder,
            parsedCommand.limit
                ? parsedCommand.indexOfLimit
                : fullCommandAsStringArray.length - 1
        )
    )

    parsedCommand.unrecognized = checkForAdditionalAtEndOfBaseSelect(
        fullCommandAsStringArray,
        parsedCommand,
        indexOfWhere
    )

    delete parsedCommand.indexOfLimit

    const validatedCommand = Joi.attempt(
        parsedCommand,
        SelectWhereGroupByOrderBySchema
    )

    return validatedCommand
}

/**
 * Parses and validates a SELECT command containing WHERE, GROUP BY, HAVING
 * and ORDER BY from the given array. Returns the validated command object
 * or throws a validation error if the object fails validation.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {object} command object
 */
const parseSelectWhereGroupByHavingOrderBy = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)

    const indexOfWhere = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'WHERE'
    )

    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'GROUP'
    )

    const indexOfHaving = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'HAVING'
    )

    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (k) => k.toUpperCase() === 'ORDER'
    )

    parsedCommand.where = parseWhere(
        fullCommandAsStringArray.slice(indexOfWhere, indexOfGroup)
    )

    parsedCommand.groupBy = parseGroupBy(
        fullCommandAsStringArray.slice(indexOfGroup, indexOfHaving)
    )

    parsedCommand.having = parseHaving(
        fullCommandAsStringArray.slice(indexOfHaving, indexOfOrder)
    )

    parsedCommand.orderBy = parseOrderBy(
        fullCommandAsStringArray.slice(
            indexOfOrder,
            parsedCommand.limit
                ? parsedCommand.indexOfLimit
                : fullCommandAsStringArray.length - 1
        )
    )

    parsedCommand.unrecognized = checkForAdditionalAtEndOfBaseSelect(
        fullCommandAsStringArray,
        parsedCommand,
        indexOfWhere
    )

    delete parsedCommand.indexOfLimit

    const validatedCommand = Joi.attempt(
        parsedCommand,
        SelectWhereGroupByHavingOrderBySchema
    )

    return validatedCommand
}

module.exports = { parseCommand }
