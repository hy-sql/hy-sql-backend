/* eslint-disable no-unused-vars */
const Command = require('../commands/Command')
const State = require('../models/State')

let command =
    'CREATE TABLE Tuotteet (id INTEGER PRIMARY KEY, nimi TEXT, hinta INTEGER);'

const parseCommand = (state, input) => {
    const fullCommandAsStringList = input
        .trim()
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

    console.log(fullCommandAsStringList)

    const command = Command(fullCommandAsStringList)

    if (!command) throw new Error('Invalid command')

    const result = command.execute(fullCommandAsStringList)

    console.log('result', result)

    state.createTable(result)

    console.log('tables', state.tables[0])

    return command ? console.log(result) : console.log('error')
}

const state = new State([])

parseCommand(state, command)
