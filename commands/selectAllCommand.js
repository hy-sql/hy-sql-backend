const {
    SelectAllSchema,
    SelectAllOrderBySchema,
} = require('../models/SelectAllSchema')

const parseCommand = (fullCommandAsStringList) => {
    if (
        fullCommandAsStringList.includes('ORDER') &&
        fullCommandAsStringList.includes('BY')
    ) {
        return parseSelectAllOrderBy(fullCommandAsStringList)
    }

    return parseSelectAll(fullCommandAsStringList)
}

const parseSelectAll = (fullCommandAsStringList) => {
    const parsedCommand = {
        name: fullCommandAsStringList.slice(0, 2).join(' '),
        from: fullCommandAsStringList[2],
        tableName: fullCommandAsStringList[3],
        finalSemicolon:
            fullCommandAsStringList[fullCommandAsStringList.length - 1] === ';'
                ? ';'
                : undefined,
    }

    const validationResult = SelectAllSchema.validate(parsedCommand)

    /* if there is something additional between the table name and ending semicolon
        an error about the nonbelonging part is created and added to existing
        validation errors or an error object is added into the validation object */
    if (fullCommandAsStringList.length > 5) {
        const additional = fullCommandAsStringList
            .slice(4, fullCommandAsStringList.length - 1)
            .join(' ')
        const errorMessage = `The following part of the query is causing it to fail: '${additional}'`

        validationResult.error
            ? validationResult.error.details.push({ message: errorMessage })
            : (validationResult.error = {
                details: [{ message: errorMessage }],
            })
    }

    return validationResult
}

const parseSelectAllOrderBy = (fullCommandAsStringList) => {
    const parsedCommand = {
        name: fullCommandAsStringList.slice(0, 2).join(' '),
        from: fullCommandAsStringList[2],
        tableName: fullCommandAsStringList[3],
        orderBy: parseOrderBy(
            fullCommandAsStringList.slice(4, fullCommandAsStringList.length - 1)
        ),
        finalSemicolon:
            fullCommandAsStringList[fullCommandAsStringList.length - 1] === ';'
                ? ';'
                : undefined,
    }

    return SelectAllOrderBySchema.validate(parsedCommand)
}

const parseOrderBy = (slicedCommandAsStringArray) => {
    console.log(slicedCommandAsStringArray)

    return slicedCommandAsStringArray.slice(0, 2).join(' ') === 'ORDER BY'
        ? {
            keyword: slicedCommandAsStringArray.slice(0, 2).join(' '),
            columnName: slicedCommandAsStringArray[2],
            order: slicedCommandAsStringArray.slice(3).join(' '),
        }
        : null
}

module.exports = { parseCommand }
