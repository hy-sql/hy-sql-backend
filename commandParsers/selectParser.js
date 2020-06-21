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
    queryContainsGroupByKeywords,
    queryContainsGroupByHavingKeywords,
    queryContainsOrderByKeywords,
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
const { parseSelectFields } = require('./fieldParser')
const { parseGroupBy } = require('./groupByParser')
const { parseHaving } = require('./havingParser')
const { parseLimit } = require('./limitParser')
const SQLError = require('../models/SQLError')

/**
 * Parses and validates a SELECT command object from the given string array.
 * Returns a Joi validation result object containing the parsed command object
 * with key value and possible validation errors as object with key error.
 * @param {string[]} fullCommandAsStringArray command as string array
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
        case queryContainsOrderByKeywords(fullCommandAsStringArray):
            return parseSelectOrderBy(fullCommandAsStringArray)
        case queryContainsGroupByHavingKeywords(fullCommandAsStringArray):
            return parseSelectGroupByHaving(fullCommandAsStringArray)
        case queryContainsGroupByKeywords(fullCommandAsStringArray):
            return parseSelectGroupBy(fullCommandAsStringArray)
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

    if (queryContainsLimitKeyword(fullCommandAsStringArray)) {
        parsedCommand.limit = parseLimit(fullCommandAsStringArray)
        parsedCommand.indexOfLimit = fullCommandAsStringArray.findIndex(
            (s) => s.toUpperCase() === 'LIMIT'
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
 * Parses and validates a SELECT command not containing WHERE or ORDER BY
 * from the given array. Returns a Joi validation result object.
 * @param {string[]} fullCommandAsStringArray command as string array
 */
const parseSelect = (fullCommandAsStringArray) => {
    const parsedBaseCommand = parseBaseCommand(fullCommandAsStringArray)
    delete parsedBaseCommand.indexOfLimit

    const validatedCommand = Joi.attempt(parsedBaseCommand, SelectSchema)

    return validatedCommand
}

/**
 * Parses and validates a SELECT command containing WHERE but not ORDER BY
 * from the given array. Returns a Joi validation result object.
 * @param {string[]} fullCommandAsStringArray command as string array
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

    delete parsedCommand.indexOfLimit

    const validatedCommand = Joi.attempt(parsedCommand, SelectWhereSchema)

    return validatedCommand
}

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

    delete parsedCommand.indexOfLimit

    const validatedCommand = Joi.attempt(parsedCommand, SelectGroupBySchema)

    return validatedCommand
}

/**
 * Parses and validates a SELECT command containing GROUP BY and HAVING but not WHERE
 * from the given array. Returns a Joi validation result object.
 * @param {string[]} fullCommandAsStringArray command as string array
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

    delete parseBaseCommand.indexOfLimit
    const validationResult = SelectGroupByHavingSchema.validate(parsedCommand)

    return validationResult
}

/**
 * Parses and validates a SELECT command containing ORDER BY but not WHERE
 * from the given array. Returns a Joi validation result object.
 * @param {string[]} fullCommandAsStringArray command as string array
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

    delete parsedCommand.indexOfLimit

    const validatedCommand = Joi.attempt(parsedCommand, SelectOrderBySchema)

    return validatedCommand
}

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

    delete parsedCommand.indexOfLimit

    const validatedCommand = Joi.attempt(
        parsedCommand,
        SelectWhereGroupBySchema
    )

    return validatedCommand
}

/**
 * Parses and validates a SELECT command containing WHERE, GROUP BY and HAVING
 * from the given array. Returns a Joi validation result object.
 * @param {string[]} fullCommandAsStringArray command as string array
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

    delete parseBaseCommand.indexOfLimit
    const validationResult = SelectWhereGroupByHavingSchema.validate(
        parsedCommand
    )

    return validationResult
}

/**
 * Parses and validates a SELECT command containing both WHERE and ORDER BY
 * from the given array. Returns a Joi validation result object.
 * @param {string[]} fullCommandAsStringArray command as string array
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

    delete parsedCommand.indexOfLimit

    const validatedCommand = Joi.attempt(
        parsedCommand,
        SelectWhereOrderBySchema
    )

    return validatedCommand
}

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

    delete parsedCommand.indexOfLimit

    const validatedCommand = Joi.attempt(
        parsedCommand,
        SelectGroupByOrderBySchema
    )

    return validatedCommand
}

/**
 * Parses and validates a SELECT command containing GROUP BY and HAVING and ORDER BY but not WHERE
 * from the given array. Returns a Joi validation result object.
 * @param {string[]} fullCommandAsStringArray command as string array
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

    delete parseBaseCommand.indexOfLimit
    const validationResult = SelectGroupByHavingOrderBySchema.validate(
        parsedCommand
    )

    return validationResult
}

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

    delete parsedCommand.indexOfLimit

    const validatedCommand = Joi.attempt(
        parsedCommand,
        SelectWhereGroupByOrderBySchema
    )

    return validatedCommand
}

/**
 * Parses and validates a SELECT command containing WHERE and GROUP BY and HAVING and ORDER BY
 * from the given array. Returns a Joi validation result object.
 * @param {string[]} fullCommandAsStringArray command as string array
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

    delete parseBaseCommand.indexOfLimit
    const validationResult = SelectWhereGroupByHavingOrderBySchema.validate(
        parsedCommand
    )

    return validationResult
}

module.exports = { parseCommand }
