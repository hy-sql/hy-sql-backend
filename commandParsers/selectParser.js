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
} = require('./parserTools/queryContains')
const { parseOrderBy } = require('./orderByParser')
const { parseSelectFields } = require('./fieldParser')

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

    return parsedCommand
}

const parseSelect = (fullCommandAsStringArray) => {
    const parsedBaseCommand = parseBaseCommand(fullCommandAsStringArray)

    const validationResult = SelectSchema.validate(parsedBaseCommand)

    return validationResult
}

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
