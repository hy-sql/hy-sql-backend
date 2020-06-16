const createTableParser = require('../commandParsers/createTableParser')
const insertIntoParser = require('../commandParsers/insertIntoParser')
const selectParser = require('../commandParsers/selectParser')
const updateParser = require('../commandParsers/updateParser')
const deleteParser = require('../commandParsers/deleteParser')

/**
 * Handles selecting and utilising the correct command parser for the given command.
 * Returns the parsed command object or null if the command is not recognised as an
 * existing command.
 * @param {string[]} fullCommandAsStringArray command as string array
 */
const parseCommand = (fullCommandAsStringArray) => {
    //tämä pitää siistiä käyttämään yksi- ja kaksisanaisia komentoja
    switch (fullCommandAsStringArray[0].toUpperCase()) {
        case 'CREATE':
            if (fullCommandAsStringArray[1].trim().toUpperCase() === 'TABLE')
                return createTableParser.parseCommand(fullCommandAsStringArray)
            return null
        case 'INSERT':
            if (fullCommandAsStringArray[1].trim().toUpperCase() === 'INTO')
                return insertIntoParser.parseCommand(fullCommandAsStringArray)
            return null
        case 'SELECT':
            return selectParser.parseCommand(fullCommandAsStringArray)
        case 'UPDATE':
            return updateParser.parseCommand(fullCommandAsStringArray)
        case 'DELETE':
            return deleteParser.parseCommand(fullCommandAsStringArray)
        default:
            console.log('sth went wrong')
            return null
    }
}

module.exports = { parseCommand }
