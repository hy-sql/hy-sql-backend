const Joi = require('@hapi/joi')
const {
    UpdateSchema,
    UpdateColumnsWhereSchema,
} = require('../schemas/UpdateSchema')
const { parseWhere } = require('./whereParser')
const { queryContainsWhereKeyword } = require('./parserTools/queryContains')

/**
 * Handles passing the given command as string array to the correct function
 * to be parsed into and validated as a UPDATE command object. Returns the
 * validated command object if no error was thrown.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {object} command object
 */
const parseCommand = (fullCommandAsStringList) => {
    if (queryContainsWhereKeyword(fullCommandAsStringList)) {
        return parseUpdateWithWhere(fullCommandAsStringList)
    } else {
        return parseUpdateWithoutWhere(fullCommandAsStringList)
    }
}

/**
 * Handles parsing of the base UPDATE command from the given array.
 * @param {string[]} fullCommandAsStringList command as string array
 */
const parseBaseCommand = (fullCommandAsStringList) => {
    const tableName = fullCommandAsStringList[1]
        ? fullCommandAsStringList[1]
        : undefined
    const set =
        fullCommandAsStringList[2].toUpperCase() === 'SET'
            ? fullCommandAsStringList[2]
            : undefined
    const finalSemicolon =
        fullCommandAsStringList[fullCommandAsStringList.length - 1] === ';'
            ? ';'
            : undefined

    const parsedBaseCommand = {
        name: fullCommandAsStringList[0],
        tableName,
        set,
        finalSemicolon,
    }

    return parsedBaseCommand
}

/**
 * Parses and validates a UPDATE command object not containing WHERE from
 * the given string array. Returns the validated command object or throws
 * a validation error if the object fails validation.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {object} command object
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

    const validatedCommand = Joi.attempt(updateCommand, UpdateSchema)

    return validatedCommand
}

/**
 * Parses and validates a UPDATE command object containing WHERE from
 * the given string array. Returns the validated command object or throws
 * a validation error if the object fails validation.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {object} command object
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
        whereIndex,
        fullCommandAsStringList.indexOf(';')
    )

    updateCommand.columns = parseUpdatedColumns(columnsAndValuesAsStringList)

    updateCommand.where = parseWhere(wherePartAsArray)

    const validatedCommand = Joi.attempt(
        updateCommand,
        UpdateColumnsWhereSchema
    )

    return validatedCommand
}

/**
 * Handles parsing of the column information from the given array.
 * @param {string[]} columnsAndValuesAsStringList array containing information of columns and values
 * @returns {object[]}
 */
const parseUpdatedColumns = (columnsAndValuesAsStringList) => {
    if (!columnsAndValuesAsStringList) return undefined

    /*first change array to string and then remove unnecessary commas (,) and change back to array*/
    const separatedColumnsAsList = columnsAndValuesAsStringList
        .join('')
        .split(',')

    /*every item of the array is {column=value}, this loop parses them into pairs and removes singlequotes*/
    const parsedUpdatedColumns = separatedColumnsAsList.map((element) => {
        if (element.indexOf('=') === -1) {
            return {
                columnName: 'equal_sign_missing',
                sign: false,
                valueType: undefined,
                value: 'equal_sign_missing',
            }
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

        return columnValuePair
    })

    return parsedUpdatedColumns
}

module.exports = { parseCommand }
