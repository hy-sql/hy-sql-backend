const State = require('../models/State')
const StateService = require('../services/StateService')
const commandService = require('../services/commandService')

/**
 * Handles executing commands. Expects the split commands to be found in request.splitCommands
 * in an array. Each query is passed to StateService for handling and from the result either an error
 * object or the result is added to the maintained array of results. Execution of commands is halted
 * when an error or no command is found. When execution is ready the result array and the current tables
 * in State are placed into request.resultArray as an object: { resultArray, state: stateArray }
 */
const execute = (request, response, next) => {
    const state = new State(new Map())
    const stateService = new StateService(state)

    const splitCommands = request.splitCommands

    let results = []

    for (let splitCommand of splitCommands) {
        try {
            const command = commandService.parseCommand(splitCommand)

            results = results.concat(stateService.updateState(command))
        } catch (error) {
            if (error.name === 'ValidationError') {
                results = results.concat({ error: error.details[0].message })
            } else if (error.name === 'SQLError') {
                results = results.concat({ error: error.message })
            }

            break
        }
    }

    const stateArray = Array.from(state.tables.values())

    request.resultArray = {
        results,
        state: stateArray,
    }

    next()
}

module.exports = execute
