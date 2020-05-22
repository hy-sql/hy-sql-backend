const commands = require('../commands')
const State = require('../models/State')

const executer = (request, response, next) => {
    const state = new State([])

    const { commandArray } = request.body

    commandArray.forEach((input) => {
        const singleCommandAsStringArray = input
            .trim()
            .replace(/\s\s+/g, ' ')
            .split(/[\s]|(?<=\()|(?=\))|(?=;)/)

        const command = commands.find((c) =>
            c.isCommand(singleCommandAsStringArray)
        )

        if (!command) return new Error('command not found')

        console.log(singleCommandAsStringArray)

        const parsedCommand = command.parseCommand(singleCommandAsStringArray)

        // Optimaalisesti olisi siirtää validaatiovirheet omaan virhekäsittelijään
        if (parsedCommand.error)
            return response.status(400).send(parsedCommand.error)

        state.updateState(parsedCommand.value)
    })

    console.log(state)

    request.state = state

    next()
}

module.exports = executer
