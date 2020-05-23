const Command = require('../commands/Command')

const parseCommand = (state, input) => {
    const fullCommandAsStringList = input
        .trim()
        .replace(/\s\s+/g, ' ')
        .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

    const command = Command(fullCommandAsStringList)

    if (!command) throw new Error('Invalid command')

    const result = command.execute(fullCommandAsStringList)

    state.updateState(result)

    return command ? result : 'error'
}

module.exports = parseCommand
