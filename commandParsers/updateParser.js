const {
    UpdateSchema,
    UpdateColumnsWhereSchema,
} = require('../schemas/UpdateSchema')
const { parseWhere } = require('./whereParser')
const { queryContainsWhereKeyword } = require('./parserTools/queryContains')

/** Parses and validates a UPDATE command object from the given string array.
 * Returns a Joi validation result object containing the parsed command object
 * with key value and possible validation errors as object with key error.
 * @param {string[]} fullCommandAsStringList command as string array
 */
const parseCommand = (fullCommandAsStringList) => {
    if (queryContainsWhereKeyword(fullCommandAsStringList)) {
        return parseUpdateWithWhere(fullCommandAsStringList)
    } else {
        return parseUpdateWithoutWhere(fullCommandAsStringList)
    }
}

/** Handles parsing of the base UPDATE command from the given array.
 * @param {string[]} fullCommandAsStringList command as string array
 */
const parseBaseCommand = (fullCommandAsStringList) => {
    let tableName = fullCommandAsStringList[1]
        ? fullCommandAsStringList[1]
        : undefined
    let set =
        fullCommandAsStringList[2].toUpperCase() === 'SET'
            ? fullCommandAsStringList[2]
            : undefined
    let finalSemicolon =
        fullCommandAsStringList[fullCommandAsStringList.length - 1] === ';'
            ? ';'
            : undefined

    const parsedCommand = {
        name: fullCommandAsStringList[0],
        tableName,
        set,
        finalSemicolon,
    }

    return parsedCommand
}

/** Parses and validates a UPDATE command not containing WHERE
 * from the given array. Returns a Joi validation result object.
 * @param {string[]} fullCommandAsStringList command as string array
 */
const parseUpdateWithoutWhere = (fullCommandAsStringList) => {
    const updateCommand = parseBaseCommand(fullCommandAsStringList)

    const columnsAndValuesAsStringList =
        updateCommand.set !== undefined
            ? fullCommandAsStringList.slice(
                  3,
                  fullCommandAsStringList.indexOf(';')
              )
            : undefined

    updateCommand.columns = parseUpdatedColumns(columnsAndValuesAsStringList)

    return UpdateSchema.validate(updateCommand)
}

/** Parses and validates a UPDATE command containing WHERE from the given array.
 * Returns a Joi validation result object.
 * @param {string[]} fullCommandAsStringList command as string array
 */
const parseUpdateWithWhere = (fullCommandAsStringList) => {
    const updateCommand = parseBaseCommand(fullCommandAsStringList)

    const whereIndex = fullCommandAsStringList.findIndex(
        (string) => string.toUpperCase() === 'WHERE'
    )

    const columnsAndValuesAsStringList =
        updateCommand.set !== undefined
            ? fullCommandAsStringList.slice(3, whereIndex)
            : undefined

    const wherePartAsArray = fullCommandAsStringList.slice(
        whereIndex
        //fullCommandAsStringList.indexOf(';') //this is commented out because of a bug in whereParser
    )

    updateCommand.columns = parseUpdatedColumns(columnsAndValuesAsStringList)

    updateCommand.where = parseWhere(wherePartAsArray)

    return UpdateColumnsWhereSchema.validate(updateCommand)
}

/** Handles parsing of the column information from the given array.
 * @param {string[]} columnsAndValuesAsStringList array containing information of columns and values
 * @returns {object[]}
 */
const parseUpdatedColumns = (columnsAndValuesAsStringList) => {
    if (!columnsAndValuesAsStringList) return undefined

    const parsedUpdatedColumns = []
    /*first change array to string and then remove unnecessary commas (,) and change back to array*/
    const separatedColumnsAsList = columnsAndValuesAsStringList
        .join('')
        .split(',')

    /*every item of the array is {column=value}, this loop parses them into pairs and removes singlequotes*/
    separatedColumnsAsList.forEach((element) => {
        if (element.indexOf('=') === -1) {
            parsedUpdatedColumns.push({
                columnName: 'equal_sign_missing',
                sign: false,
                valueType: undefined,
                value: 'equal_sign_missing',
            })
            return
        }

        const columnValuePairAsList = element.split('=')

        let valueType = 'INTEGER'
        const valueContainsQuotes = columnValuePairAsList[1].charAt(0) === "'"
        /*remove quotes if they exist around string*/
        if (valueContainsQuotes) {
            columnValuePairAsList[1] = columnValuePairAsList[1]
                .split("'")
                .join('')

            valueType = 'TEXT'
        }
        /*new pair for return*/
        const columnValuePair = {
            columnName: columnValuePairAsList[0],
            sign: true,
            valueType,
            value: columnValuePairAsList[1],
        }

        parsedUpdatedColumns.push(columnValuePair)
    })

    return parsedUpdatedColumns
}

module.exports = { parseCommand }
