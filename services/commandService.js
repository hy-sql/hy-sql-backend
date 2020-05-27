const createTableCommand = require('../commands/createTableCommand')
const insertIntoCommand = require('../commands/insertIntoCommand')
const selectAllCommand = require('../commands/selectAllCommand')

const parseCommand = (fullCommandAsStringArray) => {
    switch (fullCommandAsStringArray.slice(0, 2).join(' ').toUpperCase()) {
        case 'CREATE TABLE':
            return createTableCommand.parseCommand(fullCommandAsStringArray)
        case 'INSERT INTO':
            return insertIntoCommand.parseCommand(fullCommandAsStringArray)
        case 'SELECT *':
            return selectAllCommand.parseCommand(fullCommandAsStringArray)
        default:
            console.log('sth went wrong')
            return null
    }
}

module.exports = { parseCommand }
