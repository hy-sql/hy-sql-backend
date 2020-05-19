/* eslint-disable no-unused-vars */
const CreateTableSchema = require('../models/CreateTableSchema')

const isCommand = (fullCommandAsStringList) =>
    fullCommandAsStringList.slice(0, 2).join(' ') === 'CREATE TABLE'

const execute = (fullCommandAsStringList) => {
    const parsedCommand = parseCommand(fullCommandAsStringList)

    const result = CreateTableSchema.validate(parsedCommand)

    return !result.error ? parsedCommand : result.error
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
    const separatedColumnsAsStringList = columnsAsStringList
        .join(' ')
        .split(', ')

    console.log(separatedColumnsAsStringList)

    const columns = separatedColumnsAsStringList
        .map((c) => c.split(' '))
        .map((item) => {
            console.log(item)
            return {
                name: item[0],
                type: item[1].toUpperCase(),
                primaryKey: item.slice(2).join(' ').trim() === 'PRIMARY KEY',
            }
        })

    return columns
}

module.exports = { isCommand, execute }
