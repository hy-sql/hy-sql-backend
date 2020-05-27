const createTableCommand = require('../commands/createTableCommand')
const insertIntoCommand = require('../commands/insertIntoCommand')
const selectCommand = require('../commands/selectCommand')

const parseCommand = (fullCommandAsStringArray) => {
    switch (fullCommandAsStringArray[0].toUpperCase()) {
        case 'CREATE':
            return createTableCommand.parseCommand(fullCommandAsStringArray)
        case 'INSERT':
            return insertIntoCommand.parseCommand(fullCommandAsStringArray)
        case 'SELECT':
            return selectCommand.parseCommand(fullCommandAsStringArray)
        default:
            console.log('sth went wrong')
            return null
    }
}

module.exports = { parseCommand }
