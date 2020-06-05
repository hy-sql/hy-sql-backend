const commandService = require('../services/commandService')
const CommandArraySchema = require('../models/CommandArraySchema')

const parser = (request, response, next) => {
    const commandArray = request.body.commandArray

    if (!commandArray) {
        return response.status(400).json({
            error: 'commandArray missing',
        })
    }

    const validatedCommandArray = CommandArraySchema.validate(commandArray)

    if (validatedCommandArray.error) {
        return response.status(400).json({
            error: 'invalid format',
        })
    }

    const splitCommandArray = commandArray.map((input) =>
        input
            .trim()
            .replace(/\s\s+/g, ' ')
            .replace(/\s+,/g, ',')
            .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(?<=\)|(?=\())|(;$)/)
            .filter(Boolean)
    )

    const parsedCommands = splitCommandArray.map((c) =>
        commandService.parseCommand(c)
    )

    request.parsedCommands = parsedCommands

    next()
}

module.exports = parser
