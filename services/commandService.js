const createTableParser = require('../commandParsers/createTableParser')
const insertIntoParser = require('../commandParsers/insertIntoParser')
const selectParser = require('../commandParsers/selectParser')
const updateParser = require('../commandParsers/updateParser')
const deleteParser = require('../commandParsers/deleteParser')
const SQLError = require('../models/SQLError')

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
            return createTableParser.parseCommand(fullCommandAsStringArray)
        case 'INSERT':
            return insertIntoParser.parseCommand(fullCommandAsStringArray)
        case 'SELECT':
            return selectParser.parseCommand(fullCommandAsStringArray)
        case 'UPDATE':
            return updateParser.parseCommand(fullCommandAsStringArray)
        case 'DELETE':
            return deleteParser.parseCommand(fullCommandAsStringArray)
        default:
            throw new SQLError(
                'Query was not recognised as any existing valid query'
            )
    }
}

module.exports = { parseCommand }
