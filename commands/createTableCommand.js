/* eslint-disable no-unused-vars */
const validator = require('validator')

const isCommand = (fullCommandAsStringList) =>
    fullCommandAsStringList.slice(0, 2).join(' ') === 'CREATE TABLE'

const execute = (fullCommandAsStringList) => {
    parseCommand(fullCommandAsStringList)
}

const parseCommand = (fullCommandAsStringList) => {
    const command = {
        name: fullCommandAsStringList.slice(0, 2).join(' '),
        tableName: fullCommandAsStringList[2],
        openingBracket: fullCommandAsStringList[3],
        columns: fullCommandAsStringList.slice(
            4,
            fullCommandAsStringList.length - 2
        ),
        closingBracket:
            fullCommandAsStringList[fullCommandAsStringList.length - 2],
        finalSemicolon:
            fullCommandAsStringList[fullCommandAsStringList.length - 1],
    }

    console.log(command)
}

module.exports = { isCommand, execute }
