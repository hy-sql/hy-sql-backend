/* eslint-disable no-unused-vars */
const Command = require('../commands/Command')
const State = require('../models/State')

let command =
    'CREATE TABLE Tuotteet    (id INTEGER PRIMARY KEY,    nimi TEXT,    hinta INTEGER);'

const parseCommand = (state, input) => {
    const fullCommandAsStringList = input
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

    const command = Command(fullCommandAsStringList)

    if (!command) throw new Error('Invalid command')

    const result = command.execute(fullCommandAsStringList)

    state.updateState(result)

    return command ? console.log(result) : console.log('error')
}

const state = new State([])

parseCommand(state, command)
