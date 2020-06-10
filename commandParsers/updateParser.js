/* eslint-disable no-unused-vars */
const {
    UpdateSchema,
    UpdateColumnsWhereSchema,
} = require('../schemas/UpdateSchema')
const {
    parseWhereToCommandObject,
    queryContainsWhereKeyword,
} = require('./whereCommand')

const parseCommand = (fullCommandAsStringArray) => {
    if (queryContainsWhereKeyword(fullCommandAsStringArray)) {
        return parseUpdateWithWhere(fullCommandAsStringArray)
    } else {
        return parseUpdateWithoutWhere(fullCommandAsStringArray)
    }
}

const parseBaseCommand = (fullCommandAsStringArray) => {
    let tableName = fullCommandAsStringArray[1]
        ? fullCommandAsStringArray[1]
        : undefined
    let set =
        fullCommandAsStringArray[2].toUpperCase() === 'SET'
            ? fullCommandAsStringArray[2]
            : undefined
    let finalSemicolon =
        fullCommandAsStringArray[fullCommandAsStringArray.length - 1] === ';'
            ? ';'
            : undefined

    const parsedCommand = {
        name: fullCommandAsStringArray[0],
        tableName,
        set,
        finalSemicolon,
    }

    return parsedCommand
}

const parseUpdateWithoutWhere = (fullCommandAsStringArray) => {
    const updateParser = parseBaseCommand(fullCommandAsStringArray)

    const columnsAndValuesAsStringList =
        updateParser.set !== undefined
            ? fullCommandAsStringArray.slice(
                  3,
                  fullCommandAsStringArray.indexOf(';')
              )
            : undefined

    updateParser.columns = parseUpdatedColumns(columnsAndValuesAsStringList)

    return UpdateSchema.validate(updateParser)
}

const parseUpdateWithWhere = (fullCommandAsStringArray) => {
    const updateParser = parseBaseCommand(fullCommandAsStringArray)

    const whereIndex = fullCommandAsStringArray.findIndex(
        (string) => string.toUpperCase() === 'WHERE'
    )

    const columnsAndValuesAsStringList =
        updateParser.set !== undefined
            ? fullCommandAsStringArray.slice(3, whereIndex)
            : undefined

    const wherePartAsArray = fullCommandAsStringArray.slice(
        whereIndex,
        fullCommandAsStringArray.indexOf(';')
    )

    updateParser.columns = parseUpdatedColumns(columnsAndValuesAsStringList)

    updateParser.where = parseWhereToCommandObject(wherePartAsArray)

    return UpdateColumnsWhereSchema.validate(updateParser)
}

const parseUpdatedColumns = (columnsAndValuesAsStringList) => {
    if (!columnsAndValuesAsStringList) return undefined

    const parsedUpdatedColumns = []
    // console.log('ORIGINAL', columnsAndValuesAsStringList)

    // console.log('join:', columnsAndValuesAsStringList.join(''))
    /*first change array to string and then remove unnecessary commas (,) and change back to array*/
    const separatedColumnsAsList = columnsAndValuesAsStringList
        .join('')
        .split(',')
    // console.log('FIRST SEPARATED ', separatedColumnsAsList)

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
        // console.log('FINAL OBJECT', columnValuePair)
        parsedUpdatedColumns.push(columnValuePair)
    })

    return parsedUpdatedColumns
}

module.exports = { parseCommand }
