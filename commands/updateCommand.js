/* eslint-disable no-unused-vars */
const { UpdateSchema } = require('../models/UpdateSchema')

const parseCommand = (fullCommandAsStringList) => {
    const containsWhere = fullCommandAsStringList.indexOf('WHERE')

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

    const columnsAndValuesAsStringList =
        containsWhere === -1
            ? fullCommandAsStringList.slice(
                fullCommandAsStringList.indexOf('SET') + 1,
                fullCommandAsStringList.indexOf(';')
            )
            : fullCommandAsStringList.slice(
                fullCommandAsStringList.indexOf('SET') + 1,
                fullCommandAsStringList.indexOf('WHERE')
            )

    const parsedCommand = {
        name: fullCommandAsStringList[0],
        tableName,
        set,
        columns:
            set !== undefined
                ? parseUpdatedColumns(columnsAndValuesAsStringList)
                : undefined,
        finalSemicolon,
    }

    return UpdateSchema.validate(parsedCommand)
}

const parseUpdatedColumns = (columnsAndValuesAsStringList) => {
    if (!columnsAndValuesAsStringList) return null

    const parsedUpdatedColumns = []
    console.log('ORIGINAL', columnsAndValuesAsStringList)

    // console.log('join:', columnsAndValuesAsStringList.join(''))
    /*first change array to string and then remove unnecessary commas (,) and change back to array*/
    const separatedColumnsAsList = columnsAndValuesAsStringList
        .join('')
        .split(',')
    // console.log('FIRST SEPARATED ', separatedColumnsAsList)

    /*every item of the array is {column=value}, this loop parses them into pairs and removes singlequotes*/
    separatedColumnsAsList.forEach((element) => {
        const columnValuePairAsList = element.split('=')

        const valueContainsQuotes = columnValuePairAsList[1].charAt(0) === "'"
        /*remove quotes if they exist around string*/
        if (valueContainsQuotes) {
            columnValuePairAsList[1] = columnValuePairAsList[1]
                .split("'")
                .join('')
        }
        /*new pair for return*/
        const columnValuePair = {
            columnName: columnValuePairAsList[0],
            value: columnValuePairAsList[1],
        }
        // console.log('FINAL OBJECT', columnValuePair)
        parsedUpdatedColumns.push(columnValuePair)
    })

    return parsedUpdatedColumns
}

module.exports = { parseCommand }
