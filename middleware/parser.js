const commandService = require('../services/commandService')
const CommandArraySchema = require('../models/CommandArraySchema')
const cleanCommand = require('../utils/cleanCommand')

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

    const splitCommandArray = commandArray.map((input) => cleanCommand(input))

    const parsedCommands = splitCommandArray.map((c) =>
        commandService.parseCommand(c)
    )

    request.parsedCommands = parsedCommands

    next()
}

module.exports = parser
