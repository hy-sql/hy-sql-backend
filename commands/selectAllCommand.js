const {
    SelectAllSchema,
    SelectAllOrderBySchema,
    SelectAllWhereSchema,
} = require('../models/SelectAllSchema')
const {
    queryContainsWhereKeyword,
    parseWhereToCommandObject,
} = require('./whereCommand')

const parseCommand = (fullCommandAsStringList) => {
    if (hasOrderByKeywords(fullCommandAsStringList)) {
        return parseSelectAllOrderBy(fullCommandAsStringList)
    } else if (queryContainsWhereKeyword(fullCommandAsStringList)) {
        return parseSelectAllWhere(fullCommandAsStringList)
    }

    return parseSelectAll(fullCommandAsStringList)
}

const hasOrderByKeywords = (fullCommandAsStringList) => {
    const hasOrder = fullCommandAsStringList.findIndex(
        (s) => s.toUpperCase() === 'ORDER'
    )

    const hasBy = fullCommandAsStringList.findIndex(
        (s) => s.toUpperCase() === 'BY'
    )

    return hasOrder > 0 && hasBy > 0 ? hasOrder < hasBy : false
}

const parseBaseCommand = (fullCommandAsStringList) => {
    return {
        name: fullCommandAsStringList.slice(0, 2).join(' '),
        from: fullCommandAsStringList[2],
        tableName: fullCommandAsStringList[3],
        finalSemicolon:
            fullCommandAsStringList[fullCommandAsStringList.length - 1] === ';'
                ? ';'
                : undefined,
    }
}

const parseSelectAll = (fullCommandAsStringList) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringList)
    const validationResult = SelectAllSchema.validate(parsedCommand)

    /* if there is something additional between the table name and ending semicolon
        an error about the nonbelonging part is created and added to existing
        validation errors or an error object is added into the validation object */
    if (fullCommandAsStringList.length > 5) {
        const additional = fullCommandAsStringList
            .slice(4, fullCommandAsStringList.length - 1)
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

const parseSelectAllOrderBy = (fullCommandAsStringList) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringList)
    parsedCommand.orderBy = parseOrderBy(
        fullCommandAsStringList.slice(4, fullCommandAsStringList.length - 1)
    )

    return SelectAllOrderBySchema.validate(parsedCommand)
}

const parseSelectAllWhere = (fullCommandAsStringList) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringList)
    parsedCommand.where = parseWhereToCommandObject(
        fullCommandAsStringList.slice(4)
    )

    return SelectAllWhereSchema.validate(parsedCommand)
}

const parseOrderBy = (slicedCommandAsStringArray) => {
    console.log(slicedCommandAsStringArray)
    return slicedCommandAsStringArray.slice(0, 2).join(' ').toUpperCase() ===
        'ORDER BY'
        ? {
            keyword: slicedCommandAsStringArray
                .slice(0, 2)
                .join(' ')
                .toUpperCase(),
            columnName: slicedCommandAsStringArray[2],
            order: slicedCommandAsStringArray
                .slice(3)
                .join(' ')
                .toUpperCase(),
        }
        : null
}

module.exports = { parseCommand, parseOrderBy }
