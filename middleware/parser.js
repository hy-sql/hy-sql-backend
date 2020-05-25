const commands = require('../commands')

const parser = (request, response, next) => {
    if (!request.body.commandArray) {
        return response.status(400).json({
            error: 'commandArray missing',
        })
    }

    const { commandArray } = request.body

    const splitCommandArray = commandArray.map((input) =>
        input
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(?=;)/)
    )

    const parsedCommands = splitCommandArray.map((c) => commands.isCommand(c))

    request.parsedCommands = parsedCommands

    next()
}

module.exports = parser
