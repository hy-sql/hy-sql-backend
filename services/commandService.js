const createTableCommand = require('../commands/createTableCommand')
const insertIntoCommand = require('../commands/insertIntoCommand')
const selectCommand = require('../commands/selectCommand')
const selectAllCommand = require('../commands/selectAllCommand')
const selectWithOperatorsCommand = require('../commands/selectAdvancedCommand')
const updateCommand = require('../commands/updateCommand')
const { containsOperator } = require('../utils/containsOperator')
const {
    arithmeticExpressionPattern,
    stringFunctionsPattern,
} = require('../utils/regex')

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
            if (
                containsOperator(fullCommandAsStringArray[1]) ||
                containsExpressionOrFunction(fullCommandAsStringArray)
            ) {
                return selectWithOperatorsCommand.parseCommand(
                    fullCommandAsStringArray
                )
            } else if (fullCommandAsStringArray[1] === '*') {
                return selectAllCommand.parseCommand(fullCommandAsStringArray)
            }
            return selectCommand.parseCommand(fullCommandAsStringArray)
        case 'UPDATE':
            return updateCommand.parseCommand(fullCommandAsStringArray)
        default:
            console.log('sth went wrong')
            return null
    }
}

const containsExpressionOrFunction = (fullCommandAsStringArray) =>
    fullCommandAsStringArray.find(
        (k) =>
            k.match(arithmeticExpressionPattern) ||
            k.match(stringFunctionsPattern)
    )

module.exports = { parseCommand }
