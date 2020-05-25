const createTableCommand = require('./createTableCommand')
const insertIntoCommand = require('./insertIntoCommand')
const selectAllCommand = require('./selectAllCommand')

const isCommand = (fullCommandAsStringArray) => {
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

module.exports = { isCommand }
