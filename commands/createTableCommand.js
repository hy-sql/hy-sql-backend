/* eslint-disable no-unused-vars */
const validator = require('validator')

const isCommand = (fullCommandAsStringList) =>
    fullCommandAsStringList.slice(0, 2).join(' ') === 'CREATE TABLE'

const execute = (fullCommandAsStringList) => {
    console.log(fullCommandAsStringList)
    console.log('working')
    parseCommand(fullCommandAsStringList)
}

const parseCommand = (fullCommandAsStringList) => {
    const command = {
        name: fullCommandAsStringList.slice(0, 2).join(' '),
        tableName: fullCommandAsStringList[2],
        openingBracket: fullCommandAsStringList[3],
        columns: parseColumns(
            fullCommandAsStringList.slice(4, fullCommandAsStringList.length - 2)
        ),
        closingBracket:
            fullCommandAsStringList[fullCommandAsStringList.length - 2],
        finalSemicolon:
            fullCommandAsStringList[fullCommandAsStringList.length - 1],
    }

    console.log(command)
}

const parseColumns = (columnsAsStringList) => {
    console.log(columnsAsStringList)
}

module.exports = { isCommand, execute }
