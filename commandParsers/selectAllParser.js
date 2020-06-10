const {
    SelectAllSchema,
    SelectAllOrderBySchema,
    SelectAllWhereSchema,
    SelectAllWhereOrderBySchema,
} = require('../schemas/SelectAllSchema')
const { parseWhereToCommandObject } = require('./whereCommand')
const { parseOrderBy } = require('./orderByParser')
const {
    queryContainsWhereKeyword,
    queryContainsOrderByKeywords,
    queryContainsWhereOrderByKeywords,
} = require('./parserTools/queryContains')
const checkForAdditionalAtEnd = require('./parserTools/checkForAdditional')

const parseCommand = (fullCommandAsStringArray) => {
    if (queryContainsWhereOrderByKeywords(fullCommandAsStringArray)) {
        return parseSelectWhereOrderBy(fullCommandAsStringArray)
    } else if (queryContainsOrderByKeywords(fullCommandAsStringArray)) {
        return parseSelectAllOrderBy(fullCommandAsStringArray)
    } else if (queryContainsWhereKeyword(fullCommandAsStringArray)) {
        return parseSelectAllWhere(fullCommandAsStringArray)
    }

    return parseSelectAll(fullCommandAsStringArray)
}

const parseBaseCommand = (fullCommandAsStringArray) => {
    return {
        name: fullCommandAsStringArray.slice(0, 2).join(' '),
        from: fullCommandAsStringArray[2],
        tableName: fullCommandAsStringArray[3],
        finalSemicolon:
            fullCommandAsStringArray[fullCommandAsStringArray.length - 1] ===
            ';'
                ? ';'
                : undefined,
    }
}

const parseSelectAll = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)
    let validationResult = SelectAllSchema.validate(parsedCommand)
    validationResult = checkForAdditionalAtEnd(
        fullCommandAsStringArray,
        validationResult,
        5
    )

    return validationResult
}

const parseSelectWhereOrderBy = (fullCommandAsStringArray) => {
    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (c) => c.toUpperCase() === 'ORDER'
    )

    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)
    parsedCommand.where = parseWhereToCommandObject(
        fullCommandAsStringArray.slice(4, indexOfOrder)
    )
    parsedCommand.orderBy = parseOrderBy(
        fullCommandAsStringArray.slice(
            indexOfOrder,
            fullCommandAsStringArray.length - 1
        )
    )

    return SelectAllWhereOrderBySchema.validate(parsedCommand)
}

const parseSelectAllOrderBy = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)
    parsedCommand.orderBy = parseOrderBy(
        fullCommandAsStringArray.slice(4, fullCommandAsStringArray.length - 1)
    )

    const validationResult = SelectAllOrderBySchema.validate(parsedCommand)

    return validationResult
}

const parseSelectAllWhere = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)
    parsedCommand.where = parseWhereToCommandObject(
        fullCommandAsStringArray.slice(4)
    )

    let validationResult = SelectAllWhereSchema.validate(parsedCommand)
    validationResult = checkForAdditionalAtEnd(
        fullCommandAsStringArray,
        validationResult,
        5 + validationResult.value.where.indexCounter
    )

    return validationResult
}

module.exports = { parseCommand }
