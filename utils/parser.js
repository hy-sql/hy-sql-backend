/* eslint-disable no-unused-vars */
const validator = require('validator')
const Command = require('../commands/Command')

let command =
    'CREATE TABLEA Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);'

const parseCommand = (input) => {
    console.log(input)

    const command = Command(input) || null

    if (!command) throw new Error('INVALID COMMAND')

    command.execute()

    return 'SUCCESS'
}

console.log(parseCommand(command))
