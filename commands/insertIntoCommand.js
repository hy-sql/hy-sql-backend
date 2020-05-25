const { InsertIntoSchema } = require('../models/InsertIntoSchema')

const parseCommand = (fullCommandAsStringList) => {
    let anchorLocation = fullCommandAsStringList
        .join(' ')
        .toUpperCase()
        .split(' ')
        .indexOf('VALUES')

    if (anchorLocation === -1)
        return {
            name: fullCommandAsStringList.slice(0, 2).join(' '),
            error:
                'INSERT INTO needs a VALUES keyword before the actual values to be inserted',
        }

    const columnList = []
    cleanStringArray(
        fullCommandAsStringList.slice(
            fullCommandAsStringList[3] === '(' ? 4 : 3,
            fullCommandAsStringList[anchorLocation - 1] === ')'
                ? anchorLocation - 1
                : anchorLocation
        )
    ).forEach((col) => {
        columnList.push({ name: col })
    })

    const parsedCommand = {
        name: fullCommandAsStringList.slice(0, 2).join(' '),
        tableName: fullCommandAsStringList[2],
        columnsOpeningBracket:
            fullCommandAsStringList[3] === '(' ? '(' : undefined,
        columns: columnList,
        columnsClosingBracket:
            fullCommandAsStringList[anchorLocation - 1] === ')'
                ? ')'
                : undefined,
        anchorKeyword: fullCommandAsStringList[anchorLocation],
        valuesOpeningBracket:
            fullCommandAsStringList[anchorLocation + 1] &&
            fullCommandAsStringList[anchorLocation + 1] === '('
                ? '('
                : undefined,
        values: addAttributesToValuesArray(
            columnList,
            cleanStringArray(
                fullCommandAsStringList.slice(
                    fullCommandAsStringList[anchorLocation + 1] &&
                        fullCommandAsStringList[anchorLocation + 1] === '('
                        ? anchorLocation + 2
                        : anchorLocation + 1,
                    fullCommandAsStringList.length - 2
                )
            )
        ),
        valuesClosingBracket:
            fullCommandAsStringList[fullCommandAsStringList.length - 2] === ')'
                ? ')'
                : undefined,
        finalSemicolon:
            fullCommandAsStringList[fullCommandAsStringList.length - 1],
    }

    if (parsedCommand.error) return parsedCommand.error

    return InsertIntoSchema.validate(parsedCommand)
}

const cleanStringArray = (columnsAsStringList) => {
    return columnsAsStringList
        .join(' ')
        .split(', ')
        .map((col) => col.trim())
}

const addAttributesToValuesArray = (columnList, stringArray) => {
    const taulukko = []
    stringArray.forEach((value, index) =>
        value.match('[0-9]')
            ? taulukko.push({
                column: columnList[index] ? columnList[index].name : null,
                value,
                type: 'INTEGER',
            })
            : taulukko.push({
                column: columnList[index] ? columnList[index].name : null,
                value: value.replace(/'/g, ' ').trim(),
                type: 'TEXT',
            })
    )
    return taulukko
}

module.exports = { parseCommand }
