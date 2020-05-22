const selectAllSchema = require('../models/SelectAllSchema')

const isCommand = (fullCommandAsStringList) =>
    fullCommandAsStringList.slice(0, 2).join(' ').toUpperCase() === 'SELECT *'

const execute = (fullCommandAsStringList) => {
    const parsedCommand = parseCommand(fullCommandAsStringList)

    return selectAllSchema.validate(parsedCommand)
}

const parseCommand = (fullCommandAsStringList) => {
    const command = {
        name: fullCommandAsStringList.slice(0, 2).join(' '),
        from: fullCommandAsStringList[2],
        tableName: fullCommandAsStringList[3],
        finalSemicolon: fullCommandAsStringList[4],
    }

    return command
}

module.exports = { isCommand, execute, parseCommand }
