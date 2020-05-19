/* eslint-disable no-unused-vars */
const validator = require('validator')
const Command = require('../commands/Command')

let command =
  'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);'

const parseCommand = (input) => {
    console.log(input)

    const command = Command(input) || null

    if (!command) throw new Error('INVALID COMMAND')

    return command.execute()
}

console.log(parseCommand(command))
