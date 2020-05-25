/* eslint-disable no-unused-vars */
const { CreateTableSchema } = require('../models/CreateTableSchema')

const parseCommand = (fullCommandAsStringList) => {
    const parsedCommand = {
        name: fullCommandAsStringList.slice(0, 2).join(' '),
        tableName: fullCommandAsStringList[2],
        openingBracket: fullCommandAsStringList[3],
        columns: parseColumns(
            fullCommandAsStringList.slice(4, fullCommandAsStringList.length - 2)
        ),
        closingBracket:
            fullCommandAsStringList[fullCommandAsStringList.length - 2],
        finalSemicolon:
            fullCommandAsStringList[fullCommandAsStringList.length - 1],
    }

    return CreateTableSchema.validate(parsedCommand)
}

const parseColumns = (columnsAsStringList) => {
    if (!columnsAsStringList) return null

    const separatedColumnsAsStringList = columnsAsStringList
        .join(' ')
        .split(', ')

    const columns = separatedColumnsAsStringList
        .map((c) => c.split(' '))
        .map((item) => {
            return {
                name: item[0],
                type: item[1] ? item[1].toUpperCase() : null,
                primaryKey: parsePrimaryKey(item.slice(2)),
            }
        })

    return columns
}

const parsePrimaryKey = (stringArray) => {
    if (!Array.isArray(stringArray) || !stringArray.length) return false
    const value = stringArray.join(' ').trim().toUpperCase()

    if (value === 'PRIMARY KEY') {
        return true
    } else {
        return null
    }
}

module.exports = { parseCommand }
