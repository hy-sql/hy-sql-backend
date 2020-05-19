/* eslint-disable indent */
/* eslint-disable no-unused-vars */
const CreateTableCommand = require('./createTableCommand')
const InsertIntoCommand = require('./insertIntoCommand')
const SelectAllCommand = require('./selectAllCommand')

const Command = (fullCommandAsStringList) => {
    const commands = [CreateTableCommand, InsertIntoCommand, SelectAllCommand]

    let commandToReturn = null

    console.log(fullCommandAsStringList)

    commands.forEach((c) => {
        if (c.isCommand(fullCommandAsStringList)) {
            console.log('is command')
            commandToReturn = c
            return
        }
    })

    return commandToReturn
}

module.exports = Command
