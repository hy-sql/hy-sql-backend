/* eslint-disable no-unused-vars */
const { CreateTableSchema } = require('../models/CreateTableSchema')

const isCommand = (fullCommandAsStringList) =>
    fullCommandAsStringList.slice(0, 2).join(' ').toUpperCase() ===
    'CREATE TABLE'

const execute = (fullCommandAsStringList) => {
    const parsedCommand = parseCommand(fullCommandAsStringList)

    return CreateTableSchema.validate(parsedCommand)
}

const parseCommand = (fullCommandAsStringList) => {
    const command = {
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

    return command
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
    const test = stringArray.join(' ').trim().toUpperCase()

    if (test === 'PRIMARY KEY') {
        return true
    } else {
        return null
    }
}

module.exports = { isCommand, execute, parseCommand, parseColumns }
