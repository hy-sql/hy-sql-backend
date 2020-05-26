const commandService = require('../services/CommandService')

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
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
            .filter(Boolean)
    )

    const parsedCommands = splitCommandArray.map((c) =>
        commandService.parseCommand(c)
    )

    request.parsedCommands = parsedCommands

    next()
}

module.exports = parser
