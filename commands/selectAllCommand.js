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

    return SelectAllSchema.validate(parsedCommand)
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

    console.log(SelectAllOrderBySchema.validate(parsedCommand))

    return SelectAllOrderBySchema.validate(parsedCommand)
}

const parseOrderBy = (slicedCommandAsStringArray) => {
    console.log(slicedCommandAsStringArray)

    return slicedCommandAsStringArray.slice(0, 2).join(' ') === 'ORDER BY'
        ? {
            keyword: slicedCommandAsStringArray.slice(0, 2).join(' '),
            columnName: slicedCommandAsStringArray[2],
            order: slicedCommandAsStringArray[3],
        }
        : console.log('fail')
}

module.exports = { parseCommand }
