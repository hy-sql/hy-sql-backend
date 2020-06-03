const createTableCommand = require('../commands/createTableCommand')
const insertIntoCommand = require('../commands/insertIntoCommand')
const selectCommand = require('../commands/selectCommand')
const selectAllCommand = require('../commands/selectAllCommand')
const selectWithOperatorsCommand = require('../commands/selectAdvancedCommand')

const { containsOperator } = require('../utils/containsOperator')

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
            if (fullCommandAsStringArray[1] === '*') {
                return selectAllCommand.parseCommand(fullCommandAsStringArray)
            } else if (containsOperator(fullCommandAsStringArray[1])) {
                return selectWithOperatorsCommand.parseCommand(
                    fullCommandAsStringArray
                )
            }
            return selectCommand.parseCommand(fullCommandAsStringArray)
        default:
            console.log('sth went wrong')
            return null
    }
}

module.exports = { parseCommand }
