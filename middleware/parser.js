const commandService = require('../services/commandService')
const CommandArraySchema = require('../schemas/CommandArraySchema')
const splitCommandIntoArray = require('../commandParsers/parserTools/splitCommandIntoArray')

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
        splitCommandIntoArray(input)
    )

    const parsedCommands = splitCommandArray.map((c) =>
        commandService.parseCommand(c)
    )

    request.parsedCommands = parsedCommands

    next()
}

module.exports = parser
