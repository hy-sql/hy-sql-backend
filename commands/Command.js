/* eslint-disable no-unused-vars */
const CreateTableCommand = require('./CreateTableCommand')
const InsertIntoCommand = require('./InsertIntoCommand')
const SelectAllCommand = require('./SelectAllCommand')

const Command = (input) => {
    const fullCommandAsStringList = input.trim().split(/[\s]|(?<=\()|(?<=\))/)

    if (fullCommandAsStringList.slice(0, 2).join(' ') === 'CREATE TABLE') {
        return CreateTableCommand(fullCommandAsStringList)
    }

    if (fullCommandAsStringList.slice(0, 2).join(' ') === 'INSERT INTO') {
        return InsertIntoCommand(fullCommandAsStringList)
    }

    if (fullCommandAsStringList.slice(0, 2).join(' ') === 'SELECT *') {
        return SelectAllCommand(fullCommandAsStringList)
    }

    return null
}

module.exports = Command
