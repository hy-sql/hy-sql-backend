const { DeleteSchema, DeleteWhereSchema } = require('../schemas/DeleteSchema')
const {
    queryContainsWhereKeyword,
    parseWhereToCommandObject,
} = require('./whereCommand')
const checkForAdditionalAtEnd = require('./parserTools/checkForAdditional')

const parseCommand = (fullCommandAsStringArray) => {
    if (queryContainsWhereKeyword(fullCommandAsStringArray)) {
        return parseDeleteWhere(fullCommandAsStringArray)
    }

    return parseDelete(fullCommandAsStringArray)
}

const parseBaseCommand = (fullCommandAsStringArray) => {
    return {
        name: fullCommandAsStringArray[0],
        from: fullCommandAsStringArray[1],
        tableName: fullCommandAsStringArray[2],
        finalSemicolon:
            fullCommandAsStringArray[fullCommandAsStringArray.length - 1] ===
            ';'
                ? ';'
                : undefined,
    }
}

const parseDelete = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)
    let validationResult = DeleteSchema.validate(parsedCommand)
    validationResult = checkForAdditionalAtEnd(
        fullCommandAsStringArray,
        validationResult,
        4
    )

    return validationResult
}

const parseDeleteWhere = (fullCommandAsStringArray) => {
    const parsedCommand = parseBaseCommand(fullCommandAsStringArray)
    parsedCommand.where = parseWhereToCommandObject(
        fullCommandAsStringArray.slice(3)
    )

    let validationResult = DeleteWhereSchema.validate(parsedCommand)
    validationResult = checkForAdditionalAtEnd(
        fullCommandAsStringArray,
        validationResult,
        4 + validationResult.value.where.indexCounter
    )

    return validationResult
}

module.exports = { parseCommand }
