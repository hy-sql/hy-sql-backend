const CommandArraySchema = require('../schemas/CommandArraySchema')
const splitCommandIntoArray = require('../commandParsers/parserTools/splitCommandIntoArray')

/**
 * Handles parsing of the array of sql queries (strings) received in the request
 * into an array of command objects. Expects the sql query array to be found in
 * request.body.commandArray and at places the array of command objects
 * into request.parsedCommands.
 */
const splitCommands = (request, response, next) => {
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

    const splitCommands = commandArray.map((input) =>
        splitCommandIntoArray(input)
    )

    request.splitCommands = splitCommands

    next()
}

module.exports = splitCommands