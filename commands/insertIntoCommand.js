const { InsertIntoSchema } = require('../models/InsertIntoSchema')

const isCommand = (fullCommandAsStringList) =>
    fullCommandAsStringList.slice(0, 2).join(' ').toUpperCase() ===
    'INSERT INTO'

const parseCommand = (fullCommandAsStringList) => {
    let anchorLocation = fullCommandAsStringList.indexOf('VALUES')

    console.log(anchorLocation)

    if (anchorLocation === -1)
        return {
            name: fullCommandAsStringList.slice(0, 2).join(' '),
            error:
                'INSERT INTO needs a VALUES keyword before the actual values to be inserted',
        }

    const columnList = cleanStringArray(
        fullCommandAsStringList.slice(4, anchorLocation - 1)
    )

    const parsedCommand = {
        name: fullCommandAsStringList.slice(0, 2).join(' '),
        tableName: fullCommandAsStringList[2],
        columnsOpeningBracket: fullCommandAsStringList[3],
        columns: columnList,
        columnsClosingBracket: fullCommandAsStringList[anchorLocation - 1],
        anchorKeyword: fullCommandAsStringList[anchorLocation],
        valuesOpeningBracket: fullCommandAsStringList[anchorLocation + 1],
        values: addAttributesToValuesArray(
            columnList,
            cleanStringArray(
                fullCommandAsStringList.slice(
                    anchorLocation + 2,
                    fullCommandAsStringList.length - 2
                )
            )
        ),
        valuesClosingBracket:
            fullCommandAsStringList[fullCommandAsStringList.length - 2],
        finalSemicolon:
            fullCommandAsStringList[fullCommandAsStringList.length - 1],
    }

    console.log(InsertIntoSchema.validate(parsedCommand))

    return InsertIntoSchema.validate(parsedCommand)
}

const cleanStringArray = (columnsAsStringList) => {
    return columnsAsStringList.join(' ').split(', ')
}

const addAttributesToValuesArray = (columnList, stringArray) => {
    const taulukko = []
    stringArray.forEach((value, index) => {
        value.match('[0-9]')
            ? taulukko.push({
                column: columnList[index],
                value,
                type: 'INTEGER',
            })
            : taulukko.push({ column: columnList[index], value, type: 'TEXT' })
    })
    return taulukko
}

module.exports = { isCommand, parseCommand }
