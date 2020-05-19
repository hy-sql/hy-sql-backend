/* eslint-disable indent */
/* eslint-disable no-unused-vars */
const CreateTableCommand = require('./createTableCommand')
const InsertIntoCommand = require('./insertIntoCommand')
const SelectAllCommand = require('./selectAllCommand')

const commands = [CreateTableCommand, InsertIntoCommand, SelectAllCommand]

const Command = (fullCommandAsStringList) => {
    return commands.find((c) => c.isCommand(fullCommandAsStringList))
}

module.exports = Command
