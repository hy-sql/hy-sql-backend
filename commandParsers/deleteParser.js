const { DeleteSchema, DeleteWhereSchema } = require('../schemas/DeleteSchema')
const parseWhere = require('./whereParser')
const { queryContainsWhereKeyword } = require('./parserTools/queryContains')
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

    const whereIndex = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'WHERE'
    )
    parsedCommand.where = parseWhere(fullCommandAsStringArray.slice(whereIndex))

    let validationResult = DeleteWhereSchema.validate(parsedCommand)

    if (whereIndex !== 3) {
        const errorMessage = 'WHERE should be directly after the table name'

        validationResult.error
            ? validationResult.error.details.push({ message: errorMessage })
            : (validationResult.error = {
                  details: [{ message: errorMessage }],
              })
    }

    return validationResult
}

module.exports = { parseCommand }
