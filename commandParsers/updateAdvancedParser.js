/* eslint-disable no-unused-vars */
const {
    UpdateSchema,
    UpdateColumnsWhereSchema,
} = require('../schemas/UpdateAdvancedSchema')
const { parseWhere } = require('./whereParser')
const { queryContainsWhereKeyword } = require('./parserTools/queryContains')

// const {
//     parseWhereToCommandObject,
//     queryContainsWhereKeyword,
// } = require('./whereCommand')

const parseCommand = (fullCommandAsStringList) => {
    if (queryContainsWhereKeyword(fullCommandAsStringList)) {
        return parseUpdateWithWhere(fullCommandAsStringList)
    } else {
        return parseUpdateWithoutWhere(fullCommandAsStringList)
    }
}

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
