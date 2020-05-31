const {
    SelectAllSchema,
    SelectAllOrderBySchema,
    SelectAllWhereSchema,
    SelectAllWhereOrderBySchema,
} = require('../models/SelectAllSchema')
const {
    queryContainsWhereKeyword,
    parseWhereToCommandObject,
} = require('./whereCommand')
const {
    parseOrderBy,
    hasOrderByKeywords,
    hasWhereOrderByKeywords,
} = require('./orderByCommand')

const parseCommand = (fullCommandAsStringArray) => {
    if (hasWhereOrderByKeywords(fullCommandAsStringArray)) {
        return parseSelectWhereOrderBy(fullCommandAsStringArray)
    } else if (hasOrderByKeywords(fullCommandAsStringArray)) {
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
    validationResult = checkForAdditional(
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
    validationResult = checkForAdditional(
        fullCommandAsStringArray,
        validationResult,
        5 + validationResult.value.where.indexCounter
    )

    return validationResult
}

/* if there is something additional between expected end of query and the ending semicolon
  an error about the nonbelonging part is created and added to the given validation object */
const checkForAdditional = (
    fullCommandAsStringArray,
    validationResult,
    expectedLength
) => {
    if (fullCommandAsStringArray.length > expectedLength) {
        const additional = fullCommandAsStringArray
            .slice(expectedLength - 1, fullCommandAsStringArray.length - 1)
            .join(' ')
        const errorMessage = `The following part of the query is probably incorrect and causing it to fail: '${additional}'`

        validationResult.error
            ? validationResult.error.details.push({ message: errorMessage })
            : (validationResult.error = {
                  details: [{ message: errorMessage }],
              })
    }
    return validationResult
}

module.exports = { parseCommand }
