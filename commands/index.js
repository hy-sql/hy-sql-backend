const createTableCommand = require('./createTableCommand')
const insertIntoCommand = require('./insertIntoCommand')
const selectCommand = require('./selectCommand')

const isCommand = (fullCommandAsStringArray) => {
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

module.exports = { isCommand }
