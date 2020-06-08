const createTableCommand = require('../commands/createTableCommand')
const insertIntoCommand = require('../commands/insertIntoCommand')
const selectWithOperatorsCommand = require('../commands/selectAdvancedCommand')
const updateCommand = require('../commands/updateCommand')
const deleteCommand = require('../commands/deleteCommand')

const parseCommand = (fullCommandAsStringArray) => {
    //tämä pitää siistiä käyttämään yksi- ja kaksisanaisia komentoja
    switch (fullCommandAsStringArray[0].toUpperCase()) {
        case 'CREATE':
            if (fullCommandAsStringArray[1].trim().toUpperCase() === 'TABLE')
                return createTableCommand.parseCommand(fullCommandAsStringArray)
            return null
        case 'INSERT':
            if (fullCommandAsStringArray[1].trim().toUpperCase() === 'INTO')
                return insertIntoCommand.parseCommand(fullCommandAsStringArray)
            return null
        case 'SELECT':
            return selectWithOperatorsCommand.parseCommand(
                fullCommandAsStringArray
            )
        case 'UPDATE':
            return updateCommand.parseCommand(fullCommandAsStringArray)
        case 'DELETE':
            return deleteCommand.parseCommand(fullCommandAsStringArray)
        default:
            console.log('sth went wrong')
            return null
    }
}

module.exports = { parseCommand }
